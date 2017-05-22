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

# Makefile for the "num" (exact rational arithmetic) library

LIBNAME=nums
EXTRACFLAGS=-DBNG_ARCH_$(BNG_ARCH) -DBNG_ASM_LEVEL=$(BNG_ASM_LEVEL)
CAMLOBJS=int_misc.cmo nat.cmo big_int.cmo arith_flags.cmo \
  ratio.cmo num.cmo arith_status.cmo
CMIFILES=big_int.cmi nat.cmi num.cmi ratio.cmi arith_status.cmi
COBJS=bng.$(O) nat_stubs.$(O)

include ../Makefile

clean::
	rm -f *~

bng.$(O): bng.h bng_digit.c \
       bng_amd64.c bng_ia32.c bng_ppc.c bng_sparc.c

.PHONY: depend
depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CPP) -MM $(CPPFLAGS) *.c | sed -e 's/\.o/.$$(O)/g' > .depend
	$(CAMLRUN) $(ROOTDIR)/tools/ocamldep -slash *.mli *.ml >> .depend
endif

include .depend
