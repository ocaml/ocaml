PROGS=test1.byt test2.byt test3.byt test4.byt test5.byt test6.byt \
  test7.byt test8.byt test9.byt testA.byt sieve.byt \
  testio.byt testsocket.byt testwait.byt testsignal.byt torture.byt

include ../../../config/Makefile

VPATH=../../threads/Tests

CAMLC=../../../boot/ocamlrun ../../../ocamlc -I .. -I ../../unix -I ../../../stdlib 

CAMLOPT=../../../boot/ocamlrun ../../../ocamlopt -I .. -I ../../unix -I ../../../stdlib 

all: $(PROGS)

allopt: $(PROGS:.byt=.out)

clean:
	rm -f *.cm* *.byt *.out
	rm -f $(PROGS:.byt=.ml)

.SUFFIXES: .ml .byt .out

.ml.byt:
	cp ../../threads/Tests/$*.ml $*.ml
	$(CAMLC) -custom -o $*.byt unix.cma threads.cma $*.ml ../libthreads.a ../../unix/libunix.a -cclib -lpthread

.ml.out:
	cp ../../threads/Tests/$*.ml $*.ml
	$(CAMLOPT) -o $*.out unix.cmxa threads.cmxa $*.ml ../libthreadsnat.a ../../unix/libunix.a -cclib -lpthread

$(PROGS): ../threads.cma ../libthreads.a ../stdlib.cma
