#######################################################################
#                             ocamlbuild                              #
#                                                                     #
#  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt #
#                                                                     #
#  Copyright 2007 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the Q Public License version 1.0.               #
#                                                                     #
#######################################################################

# Makefile

all: manual.pdf manual.html

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

%.html: %.tex manual.hva
	hevea -fix -O manual.hva $<

.PHONY: clean

clean:
	rm -f *.pdf *.log *.aux *.ps *.dvi manual.h{tml,aux,toc}
