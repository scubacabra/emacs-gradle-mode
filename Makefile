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

travis: clean-elc
	${MAKE} unit
	${MAKE} ecukes-travis
	${MAKE} compile
	${MAKE} unit
	${MAKE} ecukes-travis
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes --reporter gangsta

ecukes-travis:
	${CASK} exec ecukes --reporter gangsta --no-win

docs:
	${CASK} exec ${EMACS} -Q --script bin/docs.el

compile:
	${CASK} build

clean-elc:
	${CASK} clean-elc
