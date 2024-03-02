# Foolander
Foolander is a simple snake game created for [Autumn Lisp Game Jam 2023](https://itch.io/jam/autumn-lisp-game-jam-2023).

## Motivation
Although it *is* a game, it was actually developed as an investigation into creating real-time, interactive browser-based applications **without JavaScript** (or WebAssembly).

## Implementation
The game uses HTTP 1.1 chunked transfer encoding, along with an iframe containing an HTML form to achieve two-way communication in real(ish)-time ([more detail here](https://log.schemescape.com/posts/web-development/interactive-browser-app-without-js-2.html)).

## Links

* ~~Play the game~~ (the server has been taken offline, sorry!)
* [Game jam submission page](https://itch.io/jam/autumn-lisp-game-jam-2023/rate/2333756)
* [Initial blog post](https://log.schemescape.com/posts/web-development/interactive-browser-app-without-js.html)
* [Post-mortem blog post](https://log.schemescape.com/posts/web-development/interactive-browser-app-without-js-2.html)
* [Dev log, with day-by-day progress](https://jaredkrinke.itch.io/foolander/devlog/627024/autumn-lisp-game-jam-entry)
