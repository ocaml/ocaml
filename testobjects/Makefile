# ocaml must be installed...

test: Tests Exemples

Tests:
	TERM=dumb ../ocaml < Tests.ml | tail +3 > Tests.proc
	- diff Tests.exp Tests.proc

Exemples:
	TERM=dumb ../ocaml < Exemples.ml | tail +3 > Exemples.proc
	- diff Exemples.exp Exemples.proc
