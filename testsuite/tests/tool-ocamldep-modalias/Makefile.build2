# Makefile using -no-alias-deps for all files, no need to link lib.cmo

# Note: not using pattern rules here is intended.
# This is to be as portable as possible since this Makefile
# will not necessarily be ran by GNU make
# The same holds for $< and $@

.NOTPARALLEL:

SOURCES = A.ml B.ml C.ml
OBJECTS = $(SOURCES:%.ml=Lib%.cmo)
NOBJECTS = $(OBJECTS:%.cmo=%.cmx)

byte: main.byt2
opt: clean main.opt2

main.byt2: lib2.cma main.cmo
	$(OCAMLC) -no-alias-deps lib2.cma main.cmo -o main.byt2

lib2.cma: $(OBJECTS)
	$(OCAMLC) -no-alias-deps -a -o lib2.cma $(OBJECTS)

lib.cmi: lib.mli
	$(OCAMLC) -no-alias-deps -c -w -49 lib.mli

LibA.cmo: A.ml
	$(OCAMLC) -no-alias-deps -c -open Lib -o LibA.cmo A.ml

LibB.cmo: B.ml
	$(OCAMLC) -no-alias-deps -c -open Lib -o LibB.cmo B.ml

LibC.cmo: C.ml
	$(OCAMLC) -no-alias-deps -c -open Lib -o LibC.cmo C.ml

main.opt2: lib.cmxa main.cmx
	$(OCAMLOPT) -no-alias-deps lib.cmxa main.cmx -o main.opt2

lib.cmxa: $(NOBJECTS)
	$(OCAMLOPT) -no-alias-deps -a -o lib.cmxa $(NOBJECTS)

lib.cmx: lib.ml
	$(OCAMLOPT) -no-alias-deps -c -w -49 lib.ml

LibA.cmx: A.ml
	$(OCAMLOPT) -no-alias-deps -c -open Lib -o LibA.cmx A.ml

LibB.cmx: B.ml
	$(OCAMLOPT) -no-alias-deps -c -open Lib -o LibB.cmx B.ml

LibC.cmx: C.ml
	$(OCAMLOPT) -no-alias-deps -c -open Lib -o LibC.cmx C.ml

include depend.mk2

.PHONY: clean
clean:
	rm -f *.cm* lib.ml

%.cmo: %.ml
	$(OCAMLC) -no-alias-deps -c $<

%.cmx: %.ml
	$(OCAMLOPT) -no-alias-deps -c $<
