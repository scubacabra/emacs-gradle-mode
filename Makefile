EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} ecukes
	${MAKE} compile
	${MAKE} unit
	${MAKE} ecukes
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes --reporter gangsta

docs:
	${CASK} exec ${EMACS} -Q --script bin/docs.el

compile:
	${CASK} build

clean-elc:
	${CASK} clean-elc
