all: dist/colorpicker.js

.PHONY: run

dist/colorpicker.js: colorpicker.elm
	elm-make --warn --output dist/colorpicker.js colorpicker.elm

run: dist/colorpicker.js
	x-www-browser colorpicker.html
