CAMLC=../../../boot/cslrun ../../../cslc -I ../../../stdlib
CAMLOPT=../../../boot/cslrun ../../../cslopt -I ../../../stdlib

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

$(TESTOPTFILES): ../../../cslopt

.SUFFIXES: .ml .cmo .cmx

.ml.cmo:
	$(CAMLC) -I .. -c $<

.ml.cmx:
	$(CAMLOPT) -I .. -c $<

csltopnum:
	cslmktop -o csltopnum -custom ../nums.cma ../libnums.a

clean:
	rm -f test.byt test.opt *.o *.cm? csltopnum

depend:
	csldep *.ml > .depend

include .depend
