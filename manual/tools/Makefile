CFLAGS=-g -O

all: texquote2 transf htmlquote htmlgen dvi2txt

dvi2txt:
	cd dvi_to_txt; make

transf: transf.zo htmltransf.zo transfmain.zo
	camlc -o transf -g transf.zo htmltransf.zo transfmain.zo

transf.ml: transf.mll
	camllex transf.mll

htmltransf.ml: htmltransf.mll
	camllex htmltransf.mll

htmlgen: latexmacros.zo latexscan.zo latexmain.zo
	camlc -o htmlgen -g latexmacros.zo latexscan.zo latexmain.zo

latexscan.ml: latexscan.mll
	camllex latexscan.mll

.SUFFIXES:
.SUFFIXES: .ml .zo .mli .zi .c

.ml.zo:
	camlc -c $<

.mli.zi:
	camlc -c $<

.c:
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f texquote2 transf htmlquote htmlgen dvi2txt
	rm -f transf.ml latex.ml
	rm -f *.o *.zi *.zo *.zix
	rm -f *~ #*#
	cd dvi_to_txt; make clean

latexmacros.zo: latexmacros.zi 
latexmain.zo: latexscan.zo 
latexscan.zo: latexmacros.zi 
transfmain.zo: transf.zo htmltransf.zo 
