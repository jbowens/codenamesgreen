import './main.css';
import { Elm } from './Main.elm';

var playerID = localStorage.getItem('player_id');
if (playerID == null) {
    // generate a random player identifier and save it.
    playerID = Math.random().toString(36).substring(2, 15) + Math.random().toString(36).substring(2, 15);
    localStorage.setItem('player_id', playerID);
}

Elm.Main.init({
  node: document.getElementById('root'),
  flags: playerID,
});
