PROGS=test1.byt test2.byt test3.byt test4.byt test5.byt test6.byt \
  test7.byt test8.byt test9.byt testA.byt sieve.byt \
  testio.byt testsocket.byt testwait.byt testsignal.byt

include ../../../config/Makefile

all: $(PROGS)

clean:
	rm -f *.cm* *.byt

sorts.byt: sorts.ml
	ocamlc -custom -o sorts.byt -I .. -I ../../graph threads.cma graphics.cma sorts.ml ../libthreads.a ../../graph/libgraphics.a $(X11_LINK)

.SUFFIXES: .ml .byt

.ml.byt:
	ocamlc -custom -o $*.byt -I .. -I ../../unix unix.cma threads.cma $*.ml ../libthreads.a ../../unix/libunix.a

$(PROGS): ../threads.cma ../libthreads.a
