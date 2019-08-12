package gameapi

import (
	"encoding/json"
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
	h := handler{
		ServeMux:  http.NewServeMux(),
		wordLists: wordLists,
		rand:      rand.New(rand.NewSource(time.Now().UnixNano())),
		games:     make(map[string]*Game),
	}
	h.HandleFunc("/game-state", h.handleGameState)
	return h
}

type handler struct {
	*http.ServeMux
	wordLists map[string][]string
	rand      *rand.Rand

	mu    sync.Mutex
	games map[string]*Game
}

func (h *handler) getGame(gameID string) *Game {
	if g, ok := h.games[gameID]; ok {
		return g
	}

	state := NewState(h.rand.Int63(), h.wordLists["green"])
	game := ReconstructGame(state)
	g := &game
	h.games[gameID] = g
	return g
}

// POST /game-state
func (h *handler) handleGameState(rw http.ResponseWriter, req *http.Request) {
	var body struct {
		GameID string `json:"game_id"`
	}
	err := json.NewDecoder(req.Body).Decode(&body)
	if err != nil || body.GameID == "" {
		http.Error(rw, "Error decoding request body", 400)
		return
	}

	h.mu.Lock()
	defer h.mu.Unlock()
	g := h.getGame(body.GameID)
	writeJSON(rw, g)
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
