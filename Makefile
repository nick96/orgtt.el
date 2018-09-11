CASK ?= cask
EMACS ?= emacs

all: test

test:
	${CASK} exec ert-runner

unit:
	${CASK} exec ert-runner test/orgtt-test.el

integration:
	${CASK} exec ert-runner test/orgtt-integration-test.el

install:
	${CASK} install

.PHONY: all test unit integration install
