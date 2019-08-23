package gameapi

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"net/http"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/jbowens/dictionary"
)

// Handler implements the codenames green server handler.
func Handler(wordLists map[string][]string) http.Handler {
	h := &handler{
		mux:       http.NewServeMux(),
		wordLists: wordLists,
		rand:      rand.New(rand.NewSource(time.Now().UnixNano())),
		games:     make(map[string]*Game),
	}
	h.mux.HandleFunc("/new-game", h.handleNewGame)
	h.mux.HandleFunc("/guess", h.handleGuess)
	h.mux.HandleFunc("/chat", h.handleChat)
	h.mux.HandleFunc("/events", h.handleEvents)

	// Periodically remove games that are old and inactive.
	go func() {
		for now := range time.Tick(10 * time.Minute) {
			h.mu.Lock()
			for id, g := range h.games {
				remaining := g.pruneOldPlayers(now)
				if remaining > 0 {
					continue // at least one player is still in the game
				}
				if g.CreatedAt.Add(24 * time.Hour).After(time.Now()) {
					continue // hasn't been 24 hours since the game started
				}
				delete(h.games, id)
			}
			h.mu.Unlock()
		}
	}()

	return h
}

type handler struct {
	mux       *http.ServeMux
	wordLists map[string][]string
	rand      *rand.Rand

	mu    sync.Mutex
	games map[string]*Game
}

func (h *handler) ServeHTTP(rw http.ResponseWriter, req *http.Request) {
	// Allow all cross-origin requests.
	header := rw.Header()
	header.Set("Access-Control-Allow-Origin", "*")
	header.Set("Access-Control-Allow-Methods", "*")
	header.Set("Access-Control-Allow-Headers", "Content-Type")
	header.Set("Access-Control-Max-Age", "1728000") // 20 days

	if req.Method == "OPTIONS" {
		rw.WriteHeader(http.StatusOK)
		return
	}
	h.mux.ServeHTTP(rw, req)
}

// POST /new-game
func (h *handler) handleNewGame(rw http.ResponseWriter, req *http.Request) {
	var body struct {
		GameID   string   `json:"game_id"`
		Words    []string `json:"words,omitempty"`
		PrevSeed *Seed    `json:"prev_seed,omitempty"` // a string because of js number precision
	}
	err := json.NewDecoder(req.Body).Decode(&body)
	if err != nil || body.GameID == "" {
		writeError(rw, "malformed_body", "Unable to parse request body.", 400)
		return
	}

	h.mu.Lock()
	defer h.mu.Unlock()

	// If the game already exists, make sure that the request includes
	// the existing game's seed so a delayed request doesn't reset an
	// existing game.
	oldGame, ok := h.games[body.GameID]
	if ok && (body.PrevSeed == nil || *body.PrevSeed != oldGame.Seed) {
		writeJSON(rw, oldGame)
		return
	}

	words := body.Words
	if len(words) == 0 {
		words = h.wordLists["green"]
	}
	if len(words) < len(colorDistribution) {
		writeError(rw, "too_few_words",
			fmt.Sprintf("A word list must have at least %d words.", len(colorDistribution)), 400)
		return
	}

	game := ReconstructGame(NewState(h.rand.Int63(), words))
	if oldGame != nil {
		// Carry over the players but without teams in case
		// they want to switch them up.
		for id, p := range oldGame.players {
			game.players[id] = Player{LastSeen: p.LastSeen}
		}
	}

	g := &game
	g.CreatedAt = time.Now()
	h.games[body.GameID] = g
	writeJSON(rw, g)
}

// POST /guess
func (h *handler) handleGuess(rw http.ResponseWriter, req *http.Request) {
	var body struct {
		GameID    string `json:"game_id"`
		PlayerID  string `json:"player_id"`
		Name      string `json:"name"`
		Team      int    `json:"team"`
		Index     int    `json:"index"`
		LastEvent int    `json:"last_event"`
	}

	err := json.NewDecoder(req.Body).Decode(&body)
	if err != nil || body.GameID == "" || body.Team == 0 || body.PlayerID == "" {
		writeError(rw, "malformed_body", "Unable to parse request body.", 400)
		return
	}

	h.mu.Lock()
	defer h.mu.Unlock()
	g, ok := h.games[body.GameID]
	if !ok {
		writeError(rw, "not_found", "Game not found", 404)
		return
	}

	g.guess(body.PlayerID, body.Name, body.Team, body.Index, time.Now())

	evts, _ := g.eventsSince(body.LastEvent)
	writeJSON(rw, GameUpdate{Seed: g.Seed, Events: evts})
}

// POST /chat
func (h *handler) handleChat(rw http.ResponseWriter, req *http.Request) {
	var body struct {
		GameID    string `json:"game_id"`
		PlayerID  string `json:"player_id"`
		Name      string `json:"name"`
		Team      int    `json:"team"`
		Message   string `json:"message"`
		LastEvent int    `json:"last_event"`
	}

	err := json.NewDecoder(req.Body).Decode(&body)
	if err != nil || body.GameID == "" || body.Team == 0 || body.PlayerID == "" || body.Message == "" {
		writeError(rw, "malformed_body", "Unable to parse request body.", 400)
		return
	}

	h.mu.Lock()
	defer h.mu.Unlock()
	g, ok := h.games[body.GameID]
	if !ok {
		writeError(rw, "not_found", "Game not found", 404)
		return
	}

	g.markSeen(body.PlayerID, body.Name, body.Team, time.Now())

	g.addEvent(Event{
		Type:     "chat",
		Team:     body.Team,
		PlayerID: body.PlayerID,
		Message:  body.Message,
	})

	evts, _ := g.eventsSince(body.LastEvent)
	writeJSON(rw, GameUpdate{Seed: g.Seed, Events: evts})
}

// POST /events
func (h *handler) handleEvents(rw http.ResponseWriter, req *http.Request) {
	var body struct {
		GameID    string `json:"game_id"`
		PlayerID  string `json:"player_id"`
		Name      string `json:"name"`
		Team      int    `json:"team"`
		LastEvent int    `json:"last_event"`
	}

	err := json.NewDecoder(req.Body).Decode(&body)
	if err != nil || body.GameID == "" || body.PlayerID == "" {
		writeError(rw, "malformed_body", "Unable to parse request body.", 400)
		return
	}

	h.mu.Lock()
	g, ok := h.games[body.GameID]
	h.mu.Unlock()
	if !ok {
		writeError(rw, "not_found", "Game not found", 404)
		return
	}

	g.markSeen(body.PlayerID, body.Name, body.Team, time.Now())

	evts, ch := g.eventsSince(body.LastEvent)
	if len(evts) > 0 {
		writeJSON(rw, GameUpdate{Seed: g.Seed, Events: evts})
		return
	}

	// Wait until a new event becomes available, the client
	// gives up, or we time out.
	select {
	case <-ch:
		evts, _ = g.eventsSince(body.LastEvent)

	case <-req.Context().Done():
	case <-time.After(25 * time.Second):
	}
	writeJSON(rw, GameUpdate{Seed: g.Seed, Events: evts})
}

type GameUpdate struct {
	Seed   Seed    `json:"seed"`
	Events []Event `json:"events"`
}

func writeError(rw http.ResponseWriter, code, message string, statusCode int) {
	rw.WriteHeader(statusCode)
	writeJSON(rw, struct {
		Code    string `json:"code"`
		Message string `json:"message"`
	}{Code: code, Message: message})
}

func writeJSON(rw http.ResponseWriter, resp interface{}) {
	j, err := json.Marshal(resp)
	if err != nil {
		http.Error(rw, "unable to marshal response: "+err.Error(), 500)
		return
	}

	rw.Header().Set("Content-Type", "application/json")
	rw.Write(j)
}

func DefaultWordlists() (map[string][]string, error) {
	matches, err := filepath.Glob("wordlists/*txt")
	if err != nil {
		return nil, err
	}

	lists := map[string][]string{}
	for _, m := range matches {
		base := filepath.Base(m)
		name := strings.TrimSuffix(base, filepath.Ext(base))

		d, err := dictionary.Load(m)
		if err != nil {
			return nil, err
		}
		words := d.Words()
		sort.Strings(words)
		lists[name] = words
	}
	return lists, nil
}
