# Makefile

manual.pdf: manual.tex

%.pdf: %.tex
	pdflatex $<

.PHONY: clean

clean:
	rm -f *.pdf *.log *.aux *.ps *.dvi
