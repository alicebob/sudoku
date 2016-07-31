.PHONY: all deps
all:
	elm-make main.elm grid.elm

deps:
	elm-package install

example.html: *.elm
	elm-make main.elm grid.elm --output example.html
