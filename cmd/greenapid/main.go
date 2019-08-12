package main

import (
	"net/http"

	"github.com/jbowens/codenamesgreen/gameapi"
)

func main() {
	wordLists, err := gameapi.DefaultWordlists()
	if err != nil {
		panic(err)
	}

	h := gameapi.Handler(wordLists)
	err = http.ListenAndServe(":8080", h)
	panic(err)
}
