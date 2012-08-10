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

COMPFLAGS=-I ../camltk -I ../support -I $(OTHERS)/unix 

OBJS= frx_misc.cmo frx_widget.cmo frx_font.cmo frx_entry.cmo frx_text.cmo \
      frx_listbox.cmo frx_req.cmo frx_fillbox.cmo frx_focus.cmo \
      frx_dialog.cmo frx_mem.cmo frx_rpc.cmo frx_synth.cmo frx_selection.cmo \
      frx_after.cmo frx_fit.cmo frx_ctext.cmo frx_color.cmo

OBJSX = $(OBJS:.cmo=.cmx)

all: frxlib.cma

opt: frxlib.cmxa

frxlib.cma: $(OBJS)
	$(CAMLLIBR) -o frxlib.cma $(OBJS)

frxlib.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o frxlib.cmxa $(OBJSX)

install: frxlib.cma
	cp *.cmi *.mli frxlib.cma $(INSTALLDIR)

installopt: frxlib.cmxa
	cp frxlib.cmxa frxlib.a $(INSTALLDIR)

clean:
	rm -f *.cm* *.o *.a

$(OBJS) $(OBJS:.cmo=.cmi): ../lib/$(LIBNAME).cma

$(OBJSX): ../lib/$(LIBNAME).cmxa

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -nojoin -c $(COMPFLAGS) $<


depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
