ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -I $(ROOT)/tools -I $(ROOT)/typing -I $(ROOT)/driver -I $(ROOT)/toplevel -w A-4-9
COMMON=$(ROOT)/compilerlibs/ocamlcommon.cma
BYTECMP=$(ROOT)/compilerlibs/ocamlbytecomp.cma
TOPLVL=$(ROOT)/compilerlibs/ocamltoplevel.cma

.PHONY: ifdef

ifdef:
	$(OCAMLC) -o ifdef.exe -w +A-4 $(COMMON) ifdef.ml
	$(OCAMLC) -o test_ifdef.exe -ppx ./ifdef.exe -dsource test_ifdef.ml
	./test_ifdef.exe

.PHONY: js_syntax
js_syntax:
	$(OCAMLC) -o js_syntax.exe -w +A-4 $(COMMON) js_syntax.ml
	$(OCAMLC) -o test_ifdef.exe -i -ppx ./js_syntax.exe test_js.ml


clean:
	rm -f *.exe *.cm*


.PHONY: minidoc
minidoc:
	$(OCAMLC) -custom -o minidoc.exe $(COMMON) minidoc.ml
	$(OCAMLC) -c -bin-annot testdoc.mli
	./minidoc.exe testdoc.cmti

.PHONY: lifter
lifter:
	$(OCAMLC) -w +A-4 -custom -o genlifter.exe $(COMMON) genlifter.ml
	./genlifter.exe -I ../../parsing -I ../../stdlib Parsetree.expression > ast_lifter.ml
	$(OCAMLC) -c -w +A-17 ast_lifter.ml
	$(OCAMLC) -c dumpast.ml
	$(OCAMLC) -o dumpast.exe $(COMMON) ast_lifter.cmo dumpast.cmo
	./dumpast.exe "fun x -> 1 + 3 * x" -p "x as y"
	$(OCAMLC) -custom -o metaquot.exe -w +A-4 $(COMMON) ast_lifter.cmo metaquot.ml
	$(OCAMLC) -custom -o metaquot_test.exe -w +A -ppx ./metaquot.exe $(COMMON) metaquot_test.ml
	./metaquot_test.exe

.PHONY: eval
eval:
	$(OCAMLC) -linkall -o eval.exe -w +A-4 $(COMMON) $(BYTECMP) $(TOPLVL) eval.ml
	$(OCAMLC) -o test_eval.exe -w +A -ppx ./eval.exe test_eval.ml

.PHONY: ppx_builder
ppx_builder:
	$(OCAMLC) -linkall -o ppx_builder.exe -w +A-4 $(COMMON) ppx_builder.ml
	$(OCAMLC) -o test_builder.exe -w +A -ppx ./ppx_builder.exe -dsource test_builder.ml

.PHONY: copy_typedef
copy_typedef:
	$(OCAMLC) -linkall -o copy_typedef.exe -w +A-4 $(COMMON) copy_typedef.ml
	$(OCAMLC) -c -ppx ./copy_typedef.exe test_copy_typedef.mli
	$(OCAMLC) -o test_copy_typedef.exe -w +A -ppx ./copy_typedef.exe -dsource test_copy_typedef.ml
