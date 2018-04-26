#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Jeremie Dimino, Jane Street Europe                     *
#*                                                                        *
#*   Copyright 2016 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=..
expect_MAIN=expect_test
expect_PROG=$(expect_MAIN)$(EXE)
expect_COMPFLAGS=-I $(OTOPDIR)/parsing -I $(OTOPDIR)/utils \
          -I $(OTOPDIR)/driver -I $(OTOPDIR)/toplevel
expect_LIBRARIES=../../compilerlibs/ocamlcommon \
          ../../compilerlibs/ocamlbytecomp \
          ../../compilerlibs/ocamltoplevel

$(expect_PROG): $(expect_LIBRARIES:=.cma) $(expect_MAIN).cmo
	$(OCAMLC) -linkall -o $@ $^

include $(BASEDIR)/makefiles/Makefile.common

.PHONY: clean
clean: defaultclean
	rm -f $(expect_PROG)

expect_test.cmo: COMPFLAGS=$(expect_COMPFLAGS)
