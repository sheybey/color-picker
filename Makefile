CLOSURE_FLAGS += --compilation_level ADVANCED_OPTIMIZATIONS
CLOSURE_FLAGS += --language_in ECMASCRIPT5_STRICT

all: colorpicker.min.js

.PHONY: run all clean

colorpicker.js: colorpicker.elm
	elm make --optimize --output colorpicker.js colorpicker.elm

run: colorpicker.min.js
	x-www-browser colorpicker.html

clean:
	rm -f colorpicker.js colorpicker.min.js

colorpicker.min.js: colorpicker.js
	closure-compiler $(CLOSURE_FLAGS) <colorpicker.js >colorpicker.min.js
