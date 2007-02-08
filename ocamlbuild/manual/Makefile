# Makefile

all: manual.pdf manual.html

%.pdf: %.tex
	pdflatex $<

%.html: %.tex manual.hva
	hevea -fix -O manual.hva $<

.PHONY: clean

clean:
	rm -f *.pdf *.log *.aux *.ps *.dvi manual.h{tml,aux,toc}
