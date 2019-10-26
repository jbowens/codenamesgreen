# codenames-green

Codenames Green is an app for playing the cooperative variant of the Codenames board game: Codenames Duet. It tries to implement the game faithfully, following the game's rules exactly.

Two or more players divide amongst the two sides, Side A and Side B. They alternate giving clues for their green words, trying to get the other side to guess the green words. The game is lost when either side chooses one of the opponent's black words. Players win when all green words on both sides are revealed.

[Play](https://www.codenamesgreen.com)

![Screenshot of gameplay](https://raw.githubusercontent.com/jbowens/codenamesgreen/master/screenshot.png)

## Implementation

Codenames Green is implemented as an Elm app, backed by a json API provided by a single-process Go daemon.
