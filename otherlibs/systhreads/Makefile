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

include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

OC_BYTECODE_CFLAGS += $(SHAREDLIB_CFLAGS)
OC_NATIVE_CFLAGS += $(SHAREDLIB_CFLAGS)

LIBS = $(STDLIBFLAGS) -I $(ROOTDIR)/otherlibs/unix

CAMLC=$(BEST_OCAMLC) $(LIBS)
CAMLOPT=$(BEST_OCAMLOPT) $(LIBS)

MKLIB=$(OCAMLRUN) $(ROOTDIR)/tools/ocamlmklib$(EXE)
COMPFLAGS=-w +33..39 -warn-error +A -g -bin-annot
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS += -O3
endif

LIBNAME=threads

# Note: the header on which object files produced from st_stubs.c
# should actually depend is known for sure only at compile-time.
# That's why this dependency is handled in the Makefile directly
# and removed from the output of the C compiler during make depend

BYTECODE_C_OBJS=st_stubs.b.$(O)
NATIVECODE_C_OBJS=st_stubs.n.$(O)

THREADS_SOURCES = thread.ml event.ml

THREADS_BCOBJS = $(THREADS_SOURCES:.ml=.cmo)
THREADS_NCOBJS = $(THREADS_SOURCES:.ml=.cmx)

MLIFILES=thread.mli event.mli

CMIFILES=$(MLIFILES:.mli=.cmi)

all: lib$(LIBNAME).$(A) $(LIBNAME).cma $(CMIFILES)

allopt: lib$(LIBNAME)nat.$(A) $(LIBNAME).cmxa $(CMIFILES)

lib$(LIBNAME).$(A): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)

lib$(LIBNAME).$(A): $(BYTECODE_C_OBJS)
	$(V_OCAMLMKLIB)$(MKLIB) -o $(LIBNAME) $(BYTECODE_C_OBJS)

lib$(LIBNAME)nat.$(A): OC_CFLAGS = $(OC_NATIVE_CFLAGS)

lib$(LIBNAME)nat.$(A): $(NATIVECODE_C_OBJS)
	$(V_OCAMLMKLIB)$(MKLIB) -o $(LIBNAME)nat $^

$(LIBNAME).cma: $(THREADS_BCOBJS)
	$(V_OCAMLMKLIB)$(MKLIB) -o $(LIBNAME) -ocamlc '$(CAMLC)' -linkall $^

# See remark above: force static linking of libthreadsnat.a
$(LIBNAME).cmxa: $(THREADS_NCOBJS)
	$(V_LINKOPT)$(CAMLOPT) -linkall -a -cclib -lthreadsnat -o $@ $^

# The following lines produce two object files st_stubs.b.$(O) and
# st_stubs.n.$(O) from the same source file st_stubs.c (it is compiled
# twice, each time with different options).

ifeq "$(COMPUTE_DEPS)" "true"
st_stubs.%.$(O): st_stubs.c
else
st_stubs.%.$(O): st_stubs.c $(RUNTIME_HEADERS) $(wildcard *.h)
endif
	$(V_CC)$(CC) $(OC_CFLAGS) $(CFLAGS) $(OC_CPPFLAGS) $(CPPFLAGS) \
	  $(OUTPUTOBJ)$@ -c $<

.PHONY: partialclean
partialclean:
	rm -f *.cm*

.PHONY: clean
clean: partialclean
	rm -f dllthreads*.so dllthreads*.dll *.a *.lib *.o *.obj
	rm -rf $(DEPDIR)

.PHONY: distclean
distclean: clean
	rm -f META

INSTALL_THREADSLIBDIR=$(INSTALL_LIBDIR)/$(LIBNAME)

install:
	if test -f dllthreads$(EXT_DLL); then \
	  $(INSTALL_PROG) dllthreads$(EXT_DLL) "$(INSTALL_STUBLIBDIR)"; \
	fi
	$(INSTALL_DATA) libthreads.$(A) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_THREADSLIBDIR)"
	$(INSTALL_DATA) \
	  $(CMIFILES) threads.cma META \
	  "$(INSTALL_THREADSLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(CMIFILES:.cmi=.cmti) \
	  "$(INSTALL_THREADSLIBDIR)"
	$(INSTALL_DATA) $(MLIFILES) "$(INSTALL_THREADSLIBDIR)"
endif
	$(INSTALL_DATA) caml/threads.h "$(INSTALL_INCDIR)"

installopt:
	$(INSTALL_DATA) libthreadsnat.$(A) "$(INSTALL_LIBDIR)"
	$(INSTALL_DATA) \
	  $(THREADS_NCOBJS) threads.cmxa threads.$(A) \
	  "$(INSTALL_THREADSLIBDIR)"

%.cmi: %.mli
	$(V_OCAMLC)$(CAMLC) -c $(COMPFLAGS) $<

%.cmo: %.ml
	$(V_OCAMLC)$(CAMLC) -c $(COMPFLAGS) $<

%.cmx: %.ml
	$(V_OCAMLOPT)$(CAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<

DEP_FILES := st_stubs.b.$(D)
ifeq "$(NATIVE_COMPILER)" "true"
DEP_FILES += st_stubs.n.$(D)
endif

ifeq "$(COMPUTE_DEPS)" "true"
include $(addprefix $(DEPDIR)/, $(DEP_FILES))
endif

%.b.$(D): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS)
%.n.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS)
%.n.$(D): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS)

define GEN_RULE
$(DEPDIR)/%.$(1).$(D): %.c | $(DEPDIR)
	$$(V_CCDEPS)$$(DEP_CC) $$(OC_CPPFLAGS) $$(CPPFLAGS) $$< -MT '$$*.$(1).$(O)' -MF $$@
endef

$(foreach object_type, b n, $(eval $(call GEN_RULE,$(object_type))))

.PHONY: depend
depend:
	$(V_GEN)$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml > .depend

include .depend
