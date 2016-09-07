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
LIBRARIES=unix
ifeq ($(OS),Windows_NT)
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/win32unix
LD_PATH=$(TOPDIR)/otherlibs/win32unix
ADD_BYTERUN_FLAGS="-I $(OTOPDIR)/otherlibs/win32unix"
else
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/unix
LD_PATH=$(TOPDIR)/otherlibs/unix
endif

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common
