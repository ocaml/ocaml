SOURCES = A.ml B.ml
OBJECTS = $(SOURCES:%.ml=Lib%.cmo)
NOBJECTS = $(OBJECTS:%.cmo=%.cmx)

byte: b.cmo
clean:
	rm b.cmo a.cmo

a.cmo: A.ml
	$(OCAMLC) -c -o $@ $<

b.cmo: B.ml a.cmo
	$(OCAMLC) -c -open A.M -o $@ $<

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
