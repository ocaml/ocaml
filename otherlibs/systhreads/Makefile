include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-I../../byterun $(BYTECCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib -I ../unix
C_OBJS=posix.o
CPP=/lib/cpp -P -traditional

LIB=../../stdlib

LIB_OBJS=mutex.cmo iolock.cmo pervasives.cmo \
  $(LIB)/list.cmo $(LIB)/char.cmo $(LIB)/string.cmo \
  $(LIB)/array.cmo $(LIB)/sys.cmo $(LIB)/hashtbl.cmo $(LIB)/sort.cmo \
  $(LIB)/filename.cmo $(LIB)/obj.cmo $(LIB)/lexing.cmo $(LIB)/parsing.cmo \
  $(LIB)/set.cmo $(LIB)/map.cmo $(LIB)/stack.cmo $(LIB)/queue.cmo \
  $(LIB)/stream.cmo $(LIB)/printf.cmo $(LIB)/format.cmo $(LIB)/arg.cmo \
  $(LIB)/printexc.cmo $(LIB)/gc.cmo $(LIB)/digest.cmo $(LIB)/random.cmo \
  $(LIB)/oo.cmo $(LIB)/genlex.cmo $(LIB)/callback.cmo $(LIB)/weak.cmo \
  $(LIB)/marshal.cmo

THREAD_OBJS=thread.cmo condition.cmo event.cmo threadUnix.cmo

GENFILES=pervasives.mli thread.ml condition.ml

all: libthreads.a threads.cma stdlib.cma

allopt:

libthreads.a: $(C_OBJS)
	rm -f libthreads.a
	ar rc libthreads.a $(C_OBJS)

threads.cma: $(THREAD_OBJS)
	$(CAMLC) -a -o threads.cma $(THREAD_OBJS)

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o stdlib.cma $(LIB_OBJS)

pervasives.cmo: pervasives.mli pervasives.cmi pervasives.ml
	$(CAMLC) -nopervasives -c pervasives.ml

pervasives.mli: $(LIB)/pervasives.mli
	cp $(LIB)/pervasives.mli pervasives.mli

pervasives.cmi: $(LIB)/pervasives.cmi
	cp $(LIB)/pervasives.cmi pervasives.cmi

iolock.cmo: iolock.ml
	$(CAMLC) -nopervasives -c iolock.ml

iolock.cmi: iolock.mli
	$(CAMLC) -nopervasives -c iolock.mli

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libthreads.a *.o *.cm*
	rm -f $(GENFILES)

install:
	cp libthreads.a $(LIBDIR)/libthreads.a
	if not exist $(LIBDIR)/threads mkdir $(LIBDIR)/threads
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi threads.cma stdlib.cma $(LIBDIR)/threads

installopt:

.SUFFIXES: .ml .mli .mlp .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $*.ml
	$(CPP) $(CPPFLAGS) $*.mlp > $*.ml
	@chmod -w $*.ml

thread.ml: thread.mlp
condition.ml: condition.mlp

depend:
	gcc -MM -I../../byterun *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
