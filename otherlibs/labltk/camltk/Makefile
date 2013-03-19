#######################################################################
#                                                                     #
#                 MLTk, Tcl/Tk interface of OCaml                     #
#                                                                     #
#    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    #
#               projet Cristal, INRIA Rocquencourt                    #
#            Jacques Garrigue, Kyoto University RIMS                  #
#                                                                     #
#  Copyright 2002 Institut National de Recherche en Informatique et   #
#  en Automatique and Kyoto University.  All rights reserved.         #
#  This file is distributed under the terms of the GNU Library        #
#  General Public License, with the special exception on linking      #
#  described in file LICENSE found in the OCaml source tree.          #
#                                                                     #
#######################################################################

include ../support/Makefile.common

COMPFLAGS= -I ../support -I $(OTHERS)/win32unix -I $(OTHERS)/unix

all: camltkobjs

opt: camltkobjsx

include ./modules

CAMLTKOBJS = $(CWIDGETOBJS) cTk.cmo camltk.cmo
CAMLTKOBJSX = $(CAMLTKOBJS:.cmo=.cmx)

camltkobjs: $(CAMLTKOBJS)

camltkobjsx: $(CAMLTKOBJSX)

install:
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(CAMLTKOBJS:.cmo=.cmi) $(INSTALLDIR)
	cp $(CWIDGETOBJS:.cmo=.mli) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmi

installopt:
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(CAMLTKOBJSX) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmx

clean:
	$(MAKE) -f Makefile.gen clean

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
