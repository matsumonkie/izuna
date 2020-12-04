# izuna

Izuna is a tool to help you code review your Haskell project on Github. It shows type annotations directly in Github interface.
Izuna only works with project built with stack, GHC 8.10.2 and when browsing github with Chrome.

## Server

### How to build

build: `make build GHC=8.10.2`
run: `make run GHC=8.10.2`
devel: `make devel GHC=8.10.2`. This will start a ghcid session that will run your project and restart the server on every code changes.

## Extension


### Prerequisites

Parts of the browser extension is written in Elm so you need to install some utilities to make it work.

```
npm install elm
npm install chokidar-cli # so you can trigger a compilation on every file changes
```

### Build

```shell
cd chrome-extension/
make app # to build the app
make watch # to build the app on every code changes
```
Building the app should create a `app.js` in the `chrome-extension/` folder.

### Install

Once built, you can add the extension in chrome by going to `chrome://extensions/` and clicking `load unpacked` (then select the `chrome-extension` folder)
