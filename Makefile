.PHONY: all deps format
all:
	elm-make main.elm grid.elm

deps:
	elm-package install

format:
	elm-format --yes .
