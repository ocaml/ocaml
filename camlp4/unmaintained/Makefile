#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#                                Camlp4                                 #
#                                                                       #
#   Copyright 2004 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################
#
# The unmaintained directory
#

DIRS=format lefteval ocamllex olabl scheme sml

all:
	for dir in $(DIRS); do \
		cd $$dir && $(MAKE) all && cd .. ; \
	done

opt:
	for dir in $(DIRS); do \
		cd $$dir && $(MAKE) opt && cd .. ; \
	done

depend:
	for dir in $(DIRS); do \
		cd $$dir && $(MAKE)  depend && cd .. ; \
	done

clean:
	for dir in $(DIRS); do \
		cd $$dir && $(MAKE)  clean && cd .. ; \
	done

install:
