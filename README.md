Example Websocket servant app that shares state with servant API app

#Setup

if using nix simply run `nix-shell` at route of repo to get access to all the ide tooling.

`cabal build` -- to build the project
`cabal run myappT-exe` -- to run the project

open `client/index.html` and open dev tools to see console.
in another tab naviagte to `http://localhost:8080/health` at which point you should see `"0"`. Refresh the page as see the number increase. Now look back on the `index.html` tab and in the console you should see `"Message from server X"` update every 2 seconds.
The `X` reflecting the number that can be seen in the other browser tab.



