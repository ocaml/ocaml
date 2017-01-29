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
ifeq "$(wildcard $(BASEDIR)/../config/Makefile)" ""
COMPFLAGS=-I +compiler-libs
LIBRARIES=ocamlcommon
else
COMPFLAGS=-I $(OTOPDIR)/parsing
LIBRARIES=../../../compilerlibs/ocamlcommon
endif
MODULES=
MAIN_MODULE=test

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common
