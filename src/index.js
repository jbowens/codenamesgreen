import './main.css';
import { Elm } from './Main.elm';

// Metadata about the user is persisted in local storage.
// If there is no user in local storage, we generate one
// with a random player ID and random guest name.
var encodedUser = localStorage.getItem('user');

var parsedUser = encodedUser ? JSON.parse(encodedUser) : null;

if (parsedUser == null || !parsedUser.player_id || !parsedUser.name) {
    // generate a new user with a random identifier and save it.

    var entropy = new Uint32Array(4); // 128 bits
    window.crypto.getRandomValues(entropy);
    var playerID = entropy.join("-");

    var guestNumber = Math.floor(Math.random() * 4095);

    encodedUser = JSON.stringify({
      player_id: playerID,
      name: 'Guest '+ guestNumber.toString(16).toUpperCase(),
    });
    localStorage.setItem('user', encodedUser);
}

Elm.Main.init({
  node: document.getElementById('root'),
  flags: encodedUser,
});
