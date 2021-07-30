# Makefile using -no-alias-deps only for lib.ml/mli

# Note: not using pattern rules here is intended.
# This is to be as portable as possible since this Makefile
# will not necessarily be ran by GNU make
# The same holds for $< and $@

.NOTPARALLEL:

SOURCES = A.ml B.ml C.ml D.ml
OBJECTS = lib.cmo $(SOURCES:%.ml=Lib%.cmo)
NOBJECTS = $(OBJECTS:%.cmo=%.cmx)

byte: main.byt
opt: clean main.opt

main.byt: lib.cma main.cmo
	$(OCAMLC) lib.cma main.cmo -o $@

lib.ml: lib_impl.ml
	cp lib_impl.ml lib.ml

lib.cma: $(OBJECTS)
	$(OCAMLC) -a -o $@ $(OBJECTS)

lib.cmi: lib.mli
	$(OCAMLC) -c -no-alias-deps -w -49 lib.mli

lib.cmo: lib.ml
	$(OCAMLC) -c -no-alias-deps -w -49 lib.ml

LibA.cmo: A.ml
	$(OCAMLC) -c -open Lib -o LibA.cmo A.ml

LibB.cmo: B.ml
	$(OCAMLC) -c -open Lib -o LibB.cmo B.ml

LibC.cmo: C.ml
	$(OCAMLC) -c -open Lib -o LibC.cmo C.ml

LibD.cmo: D.ml
	$(OCAMLC) -c -open Lib -o LibD.cmo D.ml

main.opt: lib.cmxa main.cmx
	$(OCAMLOPT) lib.cmxa main.cmx -o $@

lib.cmxa: $(NOBJECTS)
	$(OCAMLOPT) -a -o $@ $(NOBJECTS)

lib.cmx: lib.ml
	$(OCAMLOPT) -c -no-alias-deps -w -49 $<

LibA.cmx: A.ml
	$(OCAMLOPT) -c -open Lib -o LibA.cmx A.ml

LibB.cmx: B.ml
	$(OCAMLOPT) -c -open Lib -o LibB.cmx B.ml

LibC.cmx: C.ml
	$(OCAMLOPT) -c -open Lib -o LibC.cmx C.ml

LibD.cmx: D.ml
	$(OCAMLOPT) -c -open Lib -o LibD.cmx D.ml

include depend.mk

.PHONY: clean
clean:
	rm -f *.cm* lib.ml

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<
