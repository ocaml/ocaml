PROGS=test1.byt test2.byt test3.byt test4.byt test5.byt test6.byt \
  test7.byt test8.byt test9.byt testA.byt sieve.byt \
  testio.byt testsocket.byt testwait.byt testsignal.byt

include ../../../config/Makefile

VPATH=../../threads/Tests

all: $(PROGS)

clean:
	rm -f *.cm* *.byt
	rm -f $(PROGS:.byt=.ml)

.SUFFIXES: .ml .byt

.ml.byt:
	cp ../../threads/Tests/$*.ml $*.ml
	ocamlc -custom -o $*.byt -I .. -I ../../unix unix.cma threads.cma $*.ml ../libthreads.a ../../unix/libunix.a -cclib -lpthread

$(PROGS): ../threads.cma ../libthreads.a ../stdlib.cma
