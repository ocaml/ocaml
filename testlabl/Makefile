# $Id$
# Test extensions

CAMLTOP=../boot/ocamlrun ../ocaml -I ../stdlib

test: test-poly

test-poly:
	TERM=norepeat $(CAMLTOP) < poly.ml > poly.out 2>&1
	TERM=norepeat $(CAMLTOP) -principal < poly.ml > poly.out2 2>&1
	@diff poly.exp poly.out && echo ocaml OK || echo ocaml changed
	@diff poly.exp2 poly.out2 && echo ocaml -principal OK \
	   || echo ocaml -principal changed

promote:
	mv poly.out poly.exp
	mv poly.out2 poly.exp2
