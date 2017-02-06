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

# At the moment, the following rule only works with gcc
# It is not a big deal since the .depend file it produces is stored
# in the repository
depend:
	$(CC) -MM $(CFLAGS) *.c > .depend
	$(CAMLRUN) $(ROOTDIR)/tools/ocamldep -slash *.mli *.ml >> .depend

ifeq "$(TOOLCHAIN)" "msvc"

.depend.nt: .depend
	sed -e 's/\.o/.$(O)/g' $< > $@

include .depend.nt
else
include .depend
endif
