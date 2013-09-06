ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -I $(ROOT)/tools -I $(ROOT)/typing -I $(ROOT)/driver -I $(ROOT)/toplevel -w A-4-9
COMMON=$(ROOT)/compilerlibs/ocamlcommon.cma
BYTECMP=$(ROOT)/compilerlibs/ocamlbytecomp.cma
TOPLVL=$(ROOT)/compilerlibs/ocamltoplevel.cma

clean:
	rm -f *.exe *.cm* *~


## Conditional compilation based on environment variables

.PHONY: ifdef
ifdef:
	$(OCAMLC) -o ifdef.exe -w +A-4 $(COMMON) ifdef.ml
	$(OCAMLC) -o test_ifdef.exe -ppx ./ifdef.exe -dsource test_ifdef.ml
	./test_ifdef.exe

## A proposal for replacing js_of_ocaml Camlp4 syntax extension with
## a -ppx filter

.PHONY: js_syntax
js_syntax:
	$(OCAMLC) -o js_syntax.exe -w +A-4 $(COMMON) js_syntax.ml
	$(OCAMLC) -o test_ifdef.exe -i -ppx ./js_syntax.exe test_js.ml


## A "toy" ocamldoc clone based on .cmti files

.PHONY: minidoc
minidoc:
	$(OCAMLC) -custom -o minidoc.exe $(COMMON) minidoc.ml
	$(OCAMLC) -c -bin-annot testdoc.mli
	./minidoc.exe testdoc.cmti

## Lifting the OCaml AST, used for:
## (i)   creating a printer for Parsetree values
## (ii)  quasi-quotations

.PHONY: lifter
lifter:
	$(OCAMLC) -w +A-4-44-45 -custom -o genlifter.exe $(COMMON) genlifter.ml
	./genlifter.exe -I ../../parsing -I ../../stdlib Parsetree.expression > ast_lifter.ml
	$(OCAMLC) -c -w +A-17 ast_lifter.ml
	$(OCAMLC) -c dumpast.ml
	$(OCAMLC) -o dumpast.exe $(COMMON) ast_lifter.cmo dumpast.cmo
	./dumpast.exe "fun x -> 1 + 3 * x" -p "x as y"
	$(OCAMLC) -custom -o metaquot.exe -w +A-4 $(COMMON) ast_lifter.cmo metaquot.ml
	$(OCAMLC) -custom -o metaquot_test.exe -w +A -ppx ./metaquot.exe $(COMMON) metaquot_test.ml
	./metaquot_test.exe

## Using the OCaml toplevel to evaluate expression during compilation

.PHONY: eval
eval:
	$(OCAMLC) -linkall -o eval.exe -w +A-4 $(COMMON) $(BYTECMP) $(TOPLVL) eval.ml
	$(OCAMLC) -o test_eval.exe -w +A -ppx ./eval.exe test_eval.ml

## Example of code generation based on type declarations

.PHONY: ppx_builder
ppx_builder:
	$(OCAMLC) -linkall -o ppx_builder.exe -w +A-4 $(COMMON) ppx_builder.ml
	$(OCAMLC) -o test_builder.exe -w +A -ppx ./ppx_builder.exe -dsource test_builder.ml

## Import type definitions from other source files (e.g. to avoid code
## duplication between the .ml and .mli files)

.PHONY: copy_typedef
copy_typedef:
	$(OCAMLC) -linkall -o copy_typedef.exe -w +A-4 $(COMMON) copy_typedef.ml
	$(OCAMLC) -c -ppx ./copy_typedef.exe test_copy_typedef.mli
	$(OCAMLC) -o test_copy_typedef.exe -w +A -ppx ./copy_typedef.exe -dsource test_copy_typedef.ml


## Create mli files from ml files

.PHONY: nomli
nomli:
	$(OCAMLC) -linkall -o nomli.exe -w +A-4-9 $(COMMON) $(BYTECMP) ../../tools/untypeast.cmo ../../tools/tast_iter.cmo nomli.ml
	./nomli.exe test_nomli.ml

## A port of pa_matches

.PHONY: matches
matches:
	$(OCAMLC) -linkall -o ppx_matches.exe $(COMMON) ppx_matches.ml
	$(OCAMLC) -c -dsource -ppx ./ppx_matches.exe test_matches.ml
