.PHONY: all deps
all:
	elm-make main.elm grid.elm

deps:
	elm-package install
