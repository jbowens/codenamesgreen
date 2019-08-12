package gameapi

import (
	"encoding/json"
	"math/rand"
)

type Color int

const (
	Neutral Color = iota
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
		return "n"
	}
}

func (c Color) MarshalJSON() ([]byte, error) {
	return json.Marshal(c.String())
}

// GameState encapsulates enough data to reconstruct
// a Game's state. It's used to recreate games after
// a process restart.
type GameState struct {
	Seed       int64          `json:"seed"`
	Round      int            `json:"round"`
	ExposedOne []bool         `json:"exposed_one"`
	ExposedTwo []bool         `json:"exposed_two"`
	Teams      map[string]int `json:"teams"`
	WordSet    []string       `json:"word_set"`
}

func NewState(seed int64, words []string) GameState {
	return GameState{
		Seed:       seed,
		Round:      0,
		ExposedOne: make([]bool, len(colorDistribution)),
		ExposedTwo: make([]bool, len(colorDistribution)),
		Teams:      make(map[string]int),
		WordSet:    words,
	}
}

type Game struct {
	GameState
	Words     []string
	OneLayout []Color
	TwoLayout []Color
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
	{Neutral, Green},
	{Neutral, Green},
	{Neutral, Green},
	{Neutral, Green},
	{Neutral, Green},
	{Green, Green},
	{Green, Green},
	{Green, Green},
	{Green, Neutral},
	{Green, Neutral},
	{Green, Neutral},
	{Green, Neutral},
	{Green, Neutral},
	{Green, Black},
	{Neutral, Black},
	{Black, Black},
	{Neutral, Neutral},
	{Neutral, Neutral},
	{Neutral, Neutral},
	{Neutral, Neutral},
	{Neutral, Neutral},
	{Neutral, Neutral},
	{Neutral, Neutral},
	{Black, Neutral},
}
