#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Gabriel Scherer, projet Parsifal, INRIA Saclay              *
#*                                                                        *
#*   Copyright 2018 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

ROOTDIR=..
include $(ROOTDIR)/Makefile.common

OTHERLIBRARIES ?= bigarray dynlink str systhreads \
                  unix win32unix

# $1: target name to dispatch to all otherlibs/*/Makefile
define dispatch_
$1:
	@for lib in $$(OTHERLIBRARIES); do \
	  ($$(MAKE) -C $$$$lib $1) || exit $$$$?; \
	done
endef
define dispatch
$(eval $(call dispatch_,$1))
endef

.PHONY: all allopt clean partialclean
$(call dispatch,all)
$(call dispatch,allopt)
$(call dispatch,clean)
$(call dispatch,partialclean)
