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

# Although the OTHERLIBS variable is defined in ../Makefile.config,
# its following conditional definition needs to be kept because,
# at the moment, the clean targets depend on this variable but
# when they are invoked ../Makefile.config is not included, so that
# OTHERLIBS would be empty and the clean target would thus not work.
OTHERLIBS ?= str systhreads unix runtime_events

# $1: target name to dispatch to all otherlibs/*/Makefile
define dispatch_
$1:
	for lib in $$(OTHERLIBS); do \
	  ($$(MAKE) -C $$$$lib $1) || exit $$$$?; \
	done
endef
define dispatch
$(eval $(call dispatch_,$1))
endef

.PHONY: all allopt clean distclean partialclean
$(call dispatch,all)
$(call dispatch,allopt)
$(call dispatch,clean)
$(call dispatch,distclean)
$(call dispatch,partialclean)
