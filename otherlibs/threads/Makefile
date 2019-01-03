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

ROOTDIR = ../..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun

OC_CFLAGS += $(SHAREDLIB_CFLAGS)
OC_CPPFLAGS += -I$(ROOTDIR)/runtime
CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib \
      -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/unix
MKLIB=$(CAMLRUN) $(ROOTDIR)/tools/ocamlmklib
COMPFLAGS=-w +33..39 -warn-error A -bin-annot -g -safe-string

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

LIB=$(ROOTDIR)/stdlib

# Object file prefix
P=stdlib__

LIB_OBJS=$(LIB)/camlinternalFormatBasics.cmo stdlib.cmo $(LIB)/$(P)bool.cmo \
  $(LIB)/$(P)seq.cmo $(LIB)/$(P)option.cmo $(LIB)/$(P)result.cmo \
  $(LIB)/$(P)fun.cmo $(LIB)/$(P)unit.cmo \
  $(LIB)/$(P)array.cmo $(LIB)/$(P)list.cmo \
  $(LIB)/$(P)char.cmo $(LIB)/$(P)bytes.cmo $(LIB)/$(P)string.cmo \
  $(LIB)/$(P)sys.cmo marshal.cmo $(LIB)/$(P)obj.cmo \
  $(LIB)/$(P)int.cmo $(LIB)/$(P)int32.cmo $(LIB)/$(P)int64.cmo \
  $(LIB)/$(P)nativeint.cmo $(LIB)/$(P)lexing.cmo $(LIB)/$(P)parsing.cmo \
  $(LIB)/$(P)set.cmo $(LIB)/$(P)map.cmo $(LIB)/$(P)stack.cmo \
  $(LIB)/$(P)queue.cmo $(LIB)/camlinternalLazy.cmo $(LIB)/$(P)lazy.cmo \
  $(LIB)/$(P)stream.cmo $(LIB)/$(P)buffer.cmo $(LIB)/camlinternalFormat.cmo \
  $(LIB)/$(P)printf.cmo $(LIB)/$(P)arg.cmo $(LIB)/$(P)printexc.cmo \
  $(LIB)/$(P)gc.cmo $(LIB)/$(P)digest.cmo \
  $(LIB)/$(P)random.cmo $(LIB)/$(P)hashtbl.cmo $(LIB)/$(P)format.cmo \
  $(LIB)/$(P)scanf.cmo $(LIB)/$(P)callback.cmo $(LIB)/camlinternalOO.cmo \
  $(LIB)/$(P)oo.cmo $(LIB)/camlinternalMod.cmo $(LIB)/$(P)genlex.cmo \
  $(LIB)/$(P)weak.cmo $(LIB)/$(P)ephemeron.cmo $(LIB)/$(P)filename.cmo \
  $(LIB)/$(P)complex.cmo $(LIB)/$(P)arrayLabels.cmo $(LIB)/$(P)listLabels.cmo \
  $(LIB)/$(P)bytesLabels.cmo $(LIB)/$(P)stringLabels.cmo \
  $(LIB)/$(P)moreLabels.cmo $(LIB)/$(P)stdLabels.cmo

UNIXLIB=../unix

UNIXLIB_OBJS=unix.cmo $(UNIXLIB)/unixLabels.cmo

all: libvmthreads.a threads.cma stdlib.cma unix.cma

allopt:

libvmthreads.a: $(C_OBJS)
	$(MKLIB) -o threads -oc vmthreads $(C_OBJS)

threads.cma: $(CAML_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads -oc vmthreads $^

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o $@ $^

unix.cma: $(UNIXLIB_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o unix -linkall $^

stdlib.cmo: stdlib.mli stdlib.cmi stdlib.ml
	$(CAMLC) ${COMPFLAGS} -nopervasives \
	  -pp "$(AWK) -f $(LIB)/expand_module_aliases.awk" -o $@ -c stdlib.ml

stdlib.mli: $(LIB)/stdlib.mli
	ln -s $(LIB)/stdlib.mli stdlib.mli

stdlib.cmi: $(LIB)/stdlib.cmi
	rm -f $@
	ln -s $< $@

$(P)marshal.cmo: marshal.ml marshal.mli $(P)marshal.cmi
	$(CAMLC) ${COMPFLAGS} -o$@ -c $<

marshal.mli: $(LIB)/marshal.mli
	ln -s $< $@

$(P)marshal.cmi: $(LIB)/$(P)marshal.cmi
	ln -s $< $@

unix.mli: $(UNIXLIB)/unix.mli
	ln -s -f $< $@

unix.cmi: $(UNIXLIB)/unix.cmi
	ln -s -f $< $@

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
	  $(CMIFILES) \
	  threads.cma stdlib.cma unix.cma \
	  "$(INSTALL_LIBDIR)/vmthreads"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(CMIFILES:.cmi=.mli) $(CMIFILES:.cmi=.cmti) \
	  "$(INSTALL_LIBDIR)/vmthreads"
endif

installopt:

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

%.$(O): %.c
	$(CC) -c $(OC_CFLAGS) $(OC_CPPFLAGS) $(OUTPUTOBJ)$@ $<

.PHONY: depend
depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CC) -MM $(OC_CPPFLAGS) *.c > .depend
	$(CAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml >> .depend
endif

include .depend
