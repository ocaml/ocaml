# ocaml must be installed...

test: Tests Exemples

Tests:
	TERM=dumb ocaml < Tests.ml > Tests.proc
	- diff Tests.exp Tests.proc

Exemples:
	TERM=dumb ocaml < Exemples.ml > Exemples.proc
	- diff Exemples.exp Exemples.proc
