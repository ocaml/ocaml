#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                          Florian Angeletti                             *
#*                                                                        *
#*   Copyright 2017                                                       *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

TOPDIR=../..
include $(TOPDIR)/Makefile.tools

.SUFFIXES:

OCAMLDEP= $(OCAMLRUN) $(TOPDIR)/tools/ocamldep -slash
OCAMLC_SNP= $(OCAMLRUN) $(TOPDIR)/ocamlc -nostdlib -nopervasives

pervasives.cmi: pervasives.mli camlinternalFormatBasics.cmi
	$(OCAMLC_SNP) -c $<

camlinternalFormatBasics.cmi: \
camlinternalFormatBasics.mli
	$(OCAMLC_SNP) -c $<

%.cmi: %.mli pervasives.cmi
	$(OCAMLC_SNP) -c -open Pervasives $<

depend:
	$(OCAMLDEP) *.mli > .depend

include .depend

clean:
	rm *.mli *.cmi
