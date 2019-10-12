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

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

OC_CFLAGS += $(SHAREDLIB_CFLAGS)

OC_CPPFLAGS += -I$(ROOTDIR)/runtime

NATIVE_CPPFLAGS = \
  -DNATIVE_CODE -DTARGET_$(ARCH) -DMODEL_$(MODEL) -DSYS_$(SYSTEM)

CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun

LIBS = -nostdlib -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/$(UNIXLIB)

CAMLC=$(BEST_OCAMLC) $(LIBS)
CAMLOPT=$(BEST_OCAMLOPT) $(LIBS)

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

# Note: the header on which object files produced from st_stubs.c
# should actually depend is known for sure only at compile-time.
# That's why this dependency is handled in the Makefile directly
# and removed from the output of the C compiler during make depend

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
	$(MKLIB_CMD) -o $(LIBNAME) $(BYTECODE_C_OBJS) $(PTHREAD_LINK)

lib$(LIBNAME)nat.$(A): $(NATIVECODE_C_OBJS)
	$(MKLIB_CMD) -o $(LIBNAME)nat $^

$(LIBNAME).cma: $(THREADS_BCOBJS)
ifeq "$(UNIX_OR_WIN32)" "unix"
	$(MKLIB) -o $(LIBNAME) -ocamlc '$(CAMLC)' -cclib -lunix -linkall \
	  $(PTHREAD_CAML_LINK) $^
# TODO: Figure out why -cclib -lunix is used here.
# It may be because of the threadsUnix module which is deprecated.
# It may hence be good to figure out whether this module shouldn't be
# removed, and then -cclib -lunix arguments.
else # Windows
	$(MKLIB) -o $(LIBNAME) -ocamlc "$(CAMLC)" -linkall \
	  $(PTHREAD_CAML_LINK) $^
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
# twice, each time with different options).

st_stubs_n.$(O): OC_CPPFLAGS += $(NATIVE_CPPFLAGS)

st_stubs_b.$(O): st_stubs.c $(HEADER)
	$(CC) -c $(OC_CFLAGS) $(OC_CPPFLAGS) $(OUTPUTOBJ)$@ $<

st_stubs_n.$(O): st_stubs.c $(HEADER)
	$(CC) -c $(OC_CFLAGS) $(OC_CPPFLAGS) $(OUTPUTOBJ)$@ $<

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f dllthreads*$(EXT_DLL) *.$(A) *.$(O)

INSTALL_THREADSLIBDIR=$(INSTALL_LIBDIR)/$(LIBNAME)

install:
	if test -f dllthreads$(EXT_DLL); then \
	  $(INSTALL_PROG) \
	    dllthreads$(EXT_DLL) "$(INSTALL_STUBLIBDIR)/dllthreads$(EXT_DLL)"; \
	fi
	$(INSTALL_DATA) libthreads.$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) libthreads.$(A)
	mkdir -p "$(INSTALL_THREADSLIBDIR)"
	$(INSTALL_DATA) \
	  $(CMIFILES) threads.cma \
	  "$(INSTALL_THREADSLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(CMIFILES:.cmi=.cmti) \
	  "$(INSTALL_THREADSLIBDIR)"
	$(INSTALL_DATA) $(MLIFILES) "$(INSTALL_THREADSLIBDIR)"
endif
	$(INSTALL_DATA) threads.h "$(INSTALL_LIBDIR)/caml"

installopt:
	$(INSTALL_DATA) libthreadsnat.$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) libthreadsnat.$(A)
	$(INSTALL_DATA) \
	  $(THREADS_NCOBJS) threads.cmxa threads.$(A) \
	  "$(INSTALL_THREADSLIBDIR)"
	cd "$(INSTALL_THREADSLIBDIR)" && $(RANLIB) threads.$(A)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<

.PHONY: depend
ifeq "$(TOOLCHAIN)" "msvc"
depend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
depend:
	$(CC) -MM $(OC_CPPFLAGS) st_stubs.c \
	  | sed -e 's/st_stubs\.o/st_stubs_b.$$(O)/' \
	  -e 's/ st_\(posix\|win32\)\.h//g' > .depend
	$(CC) -MM $(OC_CPPFLAGS) $(NATIVE_CPPFLAGS) \
	  st_stubs.c | sed -e 's/st_stubs\.o/st_stubs_n.$$(O)/' \
	  -e 's/ st_\(posix\|win32\)\.h//g' >> .depend
	$(CAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml >> .depend
endif

include .depend
