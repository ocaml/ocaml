CAMLC=../../../boot/ocamlrun ../../../ocamlc -I ../../../stdlib
CAMLOPT=../../../boot/ocamlrun ../../../ocamlopt -I ../../../stdlib

test: test.byt test.opt
	./test.byt
	./test.opt

TESTFILES=test.cmo \
  test_nats.cmo test_big_ints.cmo test_ratios.cmo test_nums.cmo end_test.cmo

TESTOPTFILES=$(TESTFILES:.cmo=.cmx)

test.byt: $(TESTFILES) ../nums.cma ../libnums.a
	$(CAMLC) -o test.byt -custom ../nums.cma $(TESTFILES) ../libnums.a

test.opt: $(TESTOPTFILES) ../nums.cmxa ../libnums.a
	$(CAMLOPT) -o test.opt ../nums.cmxa $(TESTOPTFILES) ../libnums.a

$(TESTOPTFILES): ../../../ocamlopt

.SUFFIXES: .ml .cmo .cmx

.ml.cmo:
	$(CAMLC) -I .. -c $<

.ml.cmx:
	$(CAMLOPT) -I .. -c $<

ocamlnum:
	ocamlmktop -o ocamlnum -custom ../nums.cma ../libnums.a

clean:
	rm -f test.byt test.opt *.o *.cm? ocamlnum

depend:
	ocamldep *.ml > .depend

include .depend
