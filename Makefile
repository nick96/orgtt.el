CASK ?= cask
EMACS ?= emacs

all: test

test: unit

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

.PHONY: all test unit ecukes install
