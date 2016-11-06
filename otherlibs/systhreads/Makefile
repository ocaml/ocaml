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

ROOTDIR=../..

include $(ROOTDIR)/config/Makefile

CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
CAMLYACC ?= $(ROOTDIR)/boot/ocamlyacc

ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
export OCAML_FLEXLINK:=
else
export OCAML_FLEXLINK:=$(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/flexdll/flexlink.exe
endif

LIBS = -nostdlib -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/$(UNIXLIB)

CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc $(LIBS)
CAMLOPT=$(CAMLRUN) $(ROOTDIR)/ocamlopt $(LIBS)
MKLIB=$(CAMLRUN) $(ROOTDIR)/tools/ocamlmklib
COMPFLAGS=-w +33..39 -warn-error A -g -bin-annot -safe-string
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS=-O3
else
OPTCOMPFLAGS=
endif

LIBNAME=threads

ifeq "$(UNIX_OR_WIN32)" "unix"
HEADER = st_posix.h
else # Windows
HEADER = st_win32.h
endif

BYTECODE_C_OBJS=st_stubs_b.$(O)
NATIVECODE_C_OBJS=st_stubs_n.$(O)

THREADS_SOURCES = thread.ml mutex.ml condition.ml event.ml threadUnix.ml

THREADS_BCOBJS = $(THREADS_SOURCES:.ml=.cmo)
THREADS_NCOBJS = $(THREADS_SOURCES:.ml=.cmx)

MLIFILES=thread.mli mutex.mli condition.mli event.mli threadUnix.mli
CMIFILES=$(MLIFILES:.mli=.cmi)

all: lib$(LIBNAME).$(A) $(LIBNAME).cma $(CMIFILES)

allopt: lib$(LIBNAME)nat.$(A) $(LIBNAME).cmxa $(CMIFILES)

lib$(LIBNAME).$(A): $(BYTECODE_C_OBJS)
	$(MKLIB) -o $(LIBNAME) $(BYTECODE_C_OBJS) $(PTHREAD_LINK)

lib$(LIBNAME)nat.$(A): $(NATIVECODE_C_OBJS)
	$(MKLIB) -o $(LIBNAME)nat $^

$(LIBNAME).cma: $(THREADS_BCOBJS)
ifeq "$(UNIX_OR_WIN32)" "unix"
	$(MKLIB) -o $(LIBNAME) -ocamlc '$(CAMLC)' -cclib -lunix -linkall $(PTHREAD_CAML_LINK) $^
# TODO: Figure out why -cclib -lunix is used here.
# It may be because of the threadsUnix module which is deprecated.
# It may hence be good to figure out whether this module shouldn't be
# removed, and then -cclib -lunix arguments.
else # Windows
	$(MKLIB) -o $(LIBNAME) -ocamlc "$(CAMLC)" -linkall $(PTHREAD_CAML_LINK) $^
endif

# See remark above: force static linking of libthreadsnat.a
$(LIBNAME).cmxa: $(THREADS_NCOBJS)
	$(CAMLOPT) -linkall -a -cclib -lthreadsnat $(PTHREAD_CAML_LINK) -o $@ $^

# Note: I removed "-cclib -lunix" from the line above.
# Indeed, if we link threads.cmxa, then we must also link unix.cmxa,
# which itself will pass -lunix to the C linker.  It seems more
# modular to me this way. -- Alain

# The following lines produce two object files st_stubs_b.$(O) and
# st_stubs_n.$(O) from the same source file st_stubs.c (it is compiled
# twice, each time with different of options).
# Since the source and object file have a different basename, the name of
# the object file to produce must be given to the C compiler.
# For gcc this is done with the -ofoo.$(O) option.
# For msvc it's the /Fofoo.$(O) option.

ifeq "$(TOOLCHAIN)" "msvc"
  CCOUTPUT=/Fo
else
  CCOUTPUT=-o 
endif

st_stubs_b.$(O): st_stubs.c $(HEADER)
	$(BYTECC) -I$(ROOTDIR)/byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) \
	  $(CCOUTPUT)$@ -c $<

st_stubs_n.$(O): st_stubs.c $(HEADER)
	$(NATIVECC) -I$(ROOTDIR)/asmrun -I$(ROOTDIR)/byterun \
	  $(NATIVECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -DNATIVE_CODE \
	  -DTARGET_$(ARCH) -DMODEL_$(MODEL) -DSYS_$(SYSTEM) \
	  $(CCOUTPUT)$@ -c $<

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.$(A) *.$(O)

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
THREADS_LIBDIR=$(INSTALL_LIBDIR)/$(LIBNAME)
INSTALL_STUBLIBDIR=$(DESTDIR)$(STUBLIBDIR)

install:
	cp libthreads.$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) libthreads.$(A)
	mkdir -p "$(THREADS_LIBDIR)"
	cp $(CMIFILES) $(CMIFILES:.cmi=.cmti) threads.cma "$(THREADS_LIBDIR)"
	cp $(MLIFILES) "$(INSTALL_LIBDIR)"
	cp threads.h "$(INSTALL_LIBDIR)/caml"

installopt:
	cp libthreadsnat.$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) libthreadsnat.$(A)
	cp $(THREADS_NCOBJS) threads.cmxa threads.$(A) "$(THREADS_LIBDIR)"
	cd "$(THREADS_LIBDIR)" && $(RANLIB) threads.$(A)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<

ifeq "$(UNIX_OR_WIN32)" "unix"
depend: $(GENFILES)
	-$(CC) -MM -I../../byterun *.c > .depend
	$(CAMLRUN) ../../tools/ocamldep -slash *.mli *.ml >> .depend
else # Windows
depend:
endif

include .depend
