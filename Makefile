all: colorpicker.js

.PHONY: run all clean

colorpicker.js: colorpicker.elm
	elm make --optimize --output colorpicker.js colorpicker.elm

run: colorpicker.js
	x-www-browser colorpicker.html

clean:
	rm -f colorpicker.js
