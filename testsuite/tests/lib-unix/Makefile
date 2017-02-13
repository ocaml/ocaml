#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..
LIBRARIES=unix bigarray
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix \
              -I $(OTOPDIR)/otherlibs/bigarray
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix:$(TOPDIR)/otherlibs/bigarray

ifeq ($(OS),Windows_NT)
ADD_BYTERUN_FLAGS="-I $(OTOPDIR)/otherlibs/win32unix"
endif

default:
	@$(MAKE) cmdline_prog.exe
	@$(SET_LD_PATH) $(MAKE) run-all

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common

cmdline_prog.exe: cmdline_prog.$(O)
	@$(CC) $^ -o $@

%.obj: %.c
	@$(CC) -c $*.c > /dev/null
