all: client app

app:
	clear; echo "building front/app"; npx elm make src/Main.elm --optimize --output=./app.js

watch-test:
	clear; npx elm-test --watch

watch:
	clear; npx chokidar 'src/**/*.elm' -c 'clear; elm make src/Main.elm --output=./app.js'

debug:
	clear; echo "building front/app"; npx elm make elm/Main.elm --output=./app.js --debug

format:
	clear; npx elm-format elm/

analyze:
	clear; npx elm-analyze -s

check:
	clear; npx elm-review
