#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

include ../config/Makefile

DIR=$(MANDIR)/man$(MANEXT)

install:
	for i in *.m; do cp $$i $(DIR)/`basename $$i .m`.$(MANEXT); done
	echo '.so man$(MANEXT)/jocamlc.$(MANEXT)' > $(DIR)/jocamlc.opt.$(MANEXT)
	echo '.so man$(MANEXT)/jocamlopt.$(MANEXT)' > $(DIR)/jocamlopt.opt.$(MANEXT)
