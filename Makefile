all: dist/colorpicker.js

.PHONY: run

elm-stuff: elm-package.json
	elm-package install

colorpicker.js: elm-stuff colorpicker.elm
	elm-make --warn --output colorpicker.js colorpicker.elm

run: colorpicker.js
	x-www-browser colorpicker.html
