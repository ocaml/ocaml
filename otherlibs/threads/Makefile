include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib -I ../unix

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

LIB=../../stdlib

LIB_OBJS=pervasives.cmo \
  $(LIB)/list.cmo $(LIB)/char.cmo $(LIB)/string.cmo \
  $(LIB)/array.cmo $(LIB)/sys.cmo $(LIB)/hashtbl.cmo $(LIB)/sort.cmo \
  $(LIB)/filename.cmo $(LIB)/marshal.cmo $(LIB)/obj.cmo \
  $(LIB)/lexing.cmo $(LIB)/parsing.cmo \
  $(LIB)/set.cmo $(LIB)/map.cmo $(LIB)/stack.cmo $(LIB)/queue.cmo \
  $(LIB)/stream.cmo $(LIB)/printf.cmo $(LIB)/format.cmo $(LIB)/arg.cmo \
  $(LIB)/printexc.cmo $(LIB)/gc.cmo $(LIB)/digest.cmo $(LIB)/random.cmo \
  $(LIB)/oo.cmo $(LIB)/genlex.cmo $(LIB)/callback.cmo $(LIB)/weak.cmo \
  $(LIB)/lazy.cmo 

all: libthreads.a threads.cma stdlib.cma

allopt:

libthreads.a: $(C_OBJS)
	rm -f libthreads.a
	ar rc libthreads.a $(C_OBJS)
	$(RANLIB) libthreads.a

threads.cma: $(CAML_OBJS)
	$(CAMLC) -a -o threads.cma $(CAML_OBJS)

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o stdlib.cma $(LIB_OBJS)

pervasives.cmo: pervasives.mli pervasives.cmi pervasives.ml
	$(CAMLC) -nopervasives -c pervasives.ml

pervasives.mli: $(LIB)/pervasives.mli
	ln -s $(LIB)/pervasives.mli pervasives.mli

pervasives.cmi: $(LIB)/pervasives.cmi
	ln -s $(LIB)/pervasives.cmi pervasives.cmi

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libthreads.a *.o
	rm -f pervasives.mli

install:
	cp libthreads.a $(LIBDIR)/libthreads.a
	cd $(LIBDIR); $(RANLIB) libthreads.a
	if test -d $(LIBDIR)/threads; then : ; else mkdir $(LIBDIR)/threads; fi
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi threads.cma stdlib.cma $(LIBDIR)/threads
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli $(LIBDIR)

installopt:

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
