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

# Makefile for the parser generator.

ROOTDIR = ..

include $(ROOTDIR)/Makefile.common

OC_CPPFLAGS += -I$(ROOTDIR)/runtime

ifeq "$(UNIX_OR_WIN32)" "win32"
WSTR_OBJ = wstr
else
WSTR_OBJ =
endif

ocamlyacc_SOURCES := $(addsuffix .c,\
  $(WSTR_OBJ) closure error lalr lr0 main mkpar output reader skeleton \
  symtab verbose warshall)

ocamlyacc_OBJECTS := $(ocamlyacc_SOURCES:.c=.$(O))

generated_files := ocamlyacc$(EXE) $(ocamlyacc_OBJECTS)

all: ocamlyacc$(EXE)

ocamlyacc$(EXE): $(ocamlyacc_OBJECTS)
	$(MKEXE) -o $@ $^ $(EXTRALIBS)

.PHONY: clean
clean:
	rm -f ocamlyacc ocamlyacc.exe wstr.o wstr.obj \
        $(ocamlyacc_SOURCES:.c=.o) $(ocamlyacc_SOURCES:.c=.obj)

depend:

closure.$(O): defs.h
error.$(O): defs.h
lalr.$(O): defs.h
lr0.$(O): defs.h
main.$(O): defs.h
mkpar.$(O): defs.h
output.$(O): defs.h
reader.$(O): defs.h
skeleton.$(O): defs.h
symtab.$(O): defs.h
verbose.$(O): defs.h
warshall.$(O): defs.h
