#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# FIXME reduce redundancy by including ../Makefile

include ../../config/Makefile
include ../../Makefile.common

CAMLRUN ?= ../../boot/ocamlrun
CAMLYACC ?= ../../boot/ocamlyacc

CFLAGS += $(SHAREDCCCOMPOPTS)
CPPFLAGS += -I../../byterun
ROOTDIR=../..
CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib \
      -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/unix
MKLIB=$(CAMLRUN) ../../tools/ocamlmklib
COMPFLAGS=-w +33..39 -warn-error A -bin-annot -g -safe-string

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

LIB=../../stdlib

# Object file prefix
P=stdlib__

LIB_OBJS=$(LIB)/camlinternalFormatBasics.cmo stdlib.cmo					\
  $(LIB)/$(P)seq.cmo $(LIB)/$(P)array.cmo $(LIB)/$(P)list.cmo $(LIB)/$(P)char.cmo $(LIB)/$(P)bytes.cmo	\
  $(LIB)/$(P)string.cmo $(LIB)/$(P)sys.cmo $(LIB)/$(P)sort.cmo marshal.cmo		\
  $(LIB)/$(P)obj.cmo $(LIB)/$(P)int32.cmo $(LIB)/$(P)int64.cmo				\
  $(LIB)/$(P)nativeint.cmo $(LIB)/$(P)lexing.cmo $(LIB)/$(P)parsing.cmo			\
  $(LIB)/$(P)set.cmo $(LIB)/$(P)map.cmo $(LIB)/$(P)stack.cmo $(LIB)/$(P)queue.cmo	\
  $(LIB)/camlinternalLazy.cmo $(LIB)/$(P)lazy.cmo $(LIB)/$(P)stream.cmo			\
  $(LIB)/$(P)buffer.cmo $(LIB)/camlinternalFormat.cmo $(LIB)/$(P)printf.cmo		\
  $(LIB)/$(P)arg.cmo $(LIB)/$(P)printexc.cmo $(LIB)/$(P)gc.cmo $(LIB)/$(P)digest.cmo	\
  $(LIB)/$(P)random.cmo $(LIB)/$(P)hashtbl.cmo $(LIB)/$(P)format.cmo			\
  $(LIB)/$(P)scanf.cmo $(LIB)/$(P)callback.cmo $(LIB)/camlinternalOO.cmo		\
  $(LIB)/$(P)oo.cmo $(LIB)/camlinternalMod.cmo $(LIB)/$(P)genlex.cmo			\
  $(LIB)/$(P)weak.cmo $(LIB)/$(P)ephemeron.cmo $(LIB)/$(P)filename.cmo			\
  $(LIB)/$(P)complex.cmo $(LIB)/$(P)arrayLabels.cmo $(LIB)/$(P)listLabels.cmo		\
  $(LIB)/$(P)bytesLabels.cmo $(LIB)/$(P)stringLabels.cmo				\
  $(LIB)/$(P)moreLabels.cmo $(LIB)/$(P)stdLabels.cmo

UNIXLIB=../unix

UNIXLIB_OBJS=unix.cmo $(UNIXLIB)/unixLabels.cmo

all: libvmthreads.a threads.cma stdlib.cma unix.cma

allopt:

libvmthreads.a: $(C_OBJS)
	$(MKLIB) -o threads -oc vmthreads $(C_OBJS)

threads.cma: $(CAML_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads -oc vmthreads $(CAML_OBJS)

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o stdlib.cma $(LIB_OBJS)

unix.cma: $(UNIXLIB_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o unix -linkall $(UNIXLIB_OBJS)

stdlib.cmo: stdlib.mli stdlib.cmi stdlib.ml
	$(CAMLC) ${COMPFLAGS} -nopervasives \
	  -pp "$(AWK) -f $(LIB)/expand_module_aliases.awk" -o $@ -c stdlib.ml

stdlib.mli: $(LIB)/stdlib.mli
	ln -s $(LIB)/stdlib.mli stdlib.mli

stdlib.cmi: $(LIB)/stdlib.cmi
	rm -f stdlib.cmi
	ln -s $(LIB)/stdlib.cmi stdlib.cmi

$(P)marshal.cmo: marshal.mli $(P)marshal.cmi marshal.ml
	$(CAMLC) ${COMPFLAGS} -o$@ -c marshal.ml

marshal.mli: $(LIB)/marshal.mli
	ln -s $(LIB)/marshal.mli marshal.mli

$(P)marshal.cmi: $(LIB)/$(P)marshal.cmi
	ln -s $(LIB)/$(P)marshal.cmi $(P)marshal.cmi

unix.mli: $(UNIXLIB)/unix.mli
	ln -s -f $(UNIXLIB)/unix.mli unix.mli

unix.cmi: $(UNIXLIB)/unix.cmi
	ln -s -f $(UNIXLIB)/unix.cmi unix.cmi

unix.cmo: unix.mli unix.cmi $(UNIXLIB)/unixLabels.cmo
	$(CAMLC) ${COMPFLAGS} -c unix.ml

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libvmthreads.a dllvmthreads.so *.o
	rm -f stdlib.mli marshal.mli unix.mli

CMIFILES=thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi

install:
	if test -f dllvmthreads.so; then \
	  $(INSTALL_PROG) dllvmthreads.so "$(INSTALL_STUBLIBDIR)"; \
	fi
	mkdir -p "$(INSTALL_LIBDIR)/vmthreads"
	$(INSTALL_DATA) libvmthreads.a "$(INSTALL_LIBDIR)/vmthreads"
	cd "$(INSTALL_LIBDIR)/vmthreads"; $(RANLIB) libvmthreads.a
	$(INSTALL_DATA) \
	  $(CMIFILES) $(CMIFILES:.cmi=.mli) $(CMIFILES:.cmi=.cmti) \
	  threads.cma stdlib.cma unix.cma \
	  "$(INSTALL_LIBDIR)/vmthreads"

installopt:

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.PHONY: depend
depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CC) -MM $(CPPFLAGS) *.c > .depend
	$(CAMLRUN) ../../tools/ocamldep -slash *.mli *.ml >> .depend
endif

include .depend
