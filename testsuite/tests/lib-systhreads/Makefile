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
LIBRARIES=unix threads

ifeq "$(wildcard $(BASEDIR)/../config/Makefile)" ""
ADD_COMPFLAGS=-thread
LD_PATH=$(TOPDIR)/stublibs
else
ADD_COMPFLAGS=-thread -I $(OTOPDIR)/otherlibs/systhreads \
	      -I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LD_PATH=$(TOPDIR)/otherlibs/systhreads:$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix
endif

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common
