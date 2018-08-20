all: dist/colorpicker.js

.PHONY: run

elm-stuff: elm-package.json
	elm-package install

dist/colorpicker.js: elm-stuff colorpicker.elm
	elm-make --warn --output dist/colorpicker.js colorpicker.elm

run: dist/colorpicker.js
	x-www-browser colorpicker.html
