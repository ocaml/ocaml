# $Id$
# Test extensions

CAMLTOP=../boot/ocamlrun ../ocaml -I ../boot

test: test-poly

test-poly:
	TERM=character $(CAMLTOP) < poly.ml > poly.out 2>&1
	TERM=character $(CAMLTOP) -principal < poly.ml > poly.out2 2>&1
	@diff poly.out poly.exp && echo ocaml OK || echo ocaml changed
	@diff poly.out2 poly.exp2 && echo ocaml -principal OK \
	   || echo ocaml -principal changed