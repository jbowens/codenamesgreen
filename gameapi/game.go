package gameapi

import (
	"encoding/json"
	"math/rand"
	"sync"
	"time"
)

type Color int

const (
	Tan Color = iota
	Green
	Black
)

func (c Color) String() string {
	switch c {
	case Green:
		return "g"
	case Black:
		return "b"
	default:
		return "t"
	}
}

func (c Color) MarshalJSON() ([]byte, error) {
	return json.Marshal(c.String())
}

// GameState encapsulates enough data to reconstruct
// a Game's state. It's used to recreate games after
// a process restart.
type GameState struct {
	mu      sync.Mutex        `json:"-"`
	changed chan struct{}     `json:"-"`
	players map[string]Player `json:"-"`
	Seed    int64             `json:"seed"`
	Events  []Event           `json:"events"`
	WordSet []string          `json:"word_set"`
}

type Event struct {
	Number   int    `json:"number"`
	Type     string `json:"type"`
	PlayerID string `json:"player_id"`
	Team     int    `json:"team"`
	Index    int    `json:"index"`
	Message  string `json:"message"`
}

type Player struct {
	Team     int       `json:"team"`
	LastSeen time.Time `json:"last_seen"`
}

func NewState(seed int64, words []string) GameState {
	return GameState{
		changed: make(chan struct{}),
		players: make(map[string]Player),
		Seed:    seed,
		Events:  []Event{},
		WordSet: words,
	}
}

type Game struct {
	GameState `json:"state"`
	CreatedAt time.Time `json:"created_at"`
	Words     []string  `json:"words"`
	OneLayout []Color   `json:"one_layout"`
	TwoLayout []Color   `json:"two_layout"`
}

func (gs *GameState) addEvent(evt Event) {
	gs.mu.Lock()
	defer gs.mu.Unlock()
	gs.addEventLocked(evt)
}

func (gs *GameState) addEventLocked(evt Event) {
	evt.Number = len(gs.Events) + 1
	gs.Events = append(gs.Events, evt)

	// Notify any waiting goroutines that the game state
	// has been updated.
	close(gs.changed)
	gs.changed = make(chan struct{})
}

func (gs *GameState) eventsSince(lastSeen int) (evts []Event, next chan struct{}) {
	gs.mu.Lock()
	defer gs.mu.Unlock()

	evts = []Event{}
	for _, e := range gs.Events {
		if e.Number > lastSeen {
			evts = append(evts, e)
		}
	}
	return evts, gs.changed
}

func (g *Game) markSeen(playerID string, team int, when time.Time) {
	g.mu.Lock()
	defer g.mu.Unlock()

	p, ok := g.players[playerID]
	if ok {
		p.LastSeen = when
		if team != 0 && p.Team != team {
			p.Team = team
			g.addEventLocked(Event{
				Type:     "set_team",
				PlayerID: playerID,
				Team:     team,
			})
		}
		g.players[playerID] = p
		return
	}

	g.players[playerID] = Player{Team: team, LastSeen: when}
	g.addEventLocked(Event{
		Type:     "new_player",
		PlayerID: playerID,
		Team:     team,
	})
}

func (g *Game) pruneOldPlayers(now time.Time) (remaining int) {
	g.mu.Lock()
	defer g.mu.Unlock()

	for id, player := range g.players {
		if player.LastSeen.Add(50 * time.Second).Before(now) {
			delete(g.players, id)
			g.addEventLocked(Event{
				Type:     "player_left",
				PlayerID: id,
				Team:     player.Team,
			})
			continue
		}
	}
	return len(g.players)
}

func ReconstructGame(state GameState) (g Game) {
	g = Game{
		GameState: state,
		OneLayout: make([]Color, len(colorDistribution)),
		TwoLayout: make([]Color, len(colorDistribution)),
	}

	rnd := rand.New(rand.NewSource(state.Seed))

	// Pick 25 random words.
	used := make(map[string]bool, len(colorDistribution))
	for len(used) < len(colorDistribution) {
		w := state.WordSet[rnd.Intn(len(state.WordSet))]
		if !used[w] {
			g.Words = append(g.Words, w)
			used[w] = true
		}
	}

	// Assign the colors for each team, according to the
	// relative distribution in the rule book.
	perm := rnd.Perm(len(colorDistribution))
	for i, colors := range colorDistribution {
		g.OneLayout[perm[i]] = colors[0]
		g.TwoLayout[perm[i]] = colors[1]
	}
	return g
}

var colorDistribution = [25][2]Color{
	{Black, Green},
	{Tan, Green},
	{Tan, Green},
	{Tan, Green},
	{Tan, Green},
	{Tan, Green},
	{Green, Green},
	{Green, Green},
	{Green, Green},
	{Green, Tan},
	{Green, Tan},
	{Green, Tan},
	{Green, Tan},
	{Green, Tan},
	{Green, Black},
	{Tan, Black},
	{Black, Black},
	{Tan, Tan},
	{Tan, Tan},
	{Tan, Tan},
	{Tan, Tan},
	{Tan, Tan},
	{Tan, Tan},
	{Tan, Tan},
	{Black, Tan},
}
