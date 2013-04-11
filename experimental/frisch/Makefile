ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -I $(ROOT)/tools -I $(ROOT)/typing -I $(ROOT)/driver -I $(ROOT)/toplevel -w A-4-9
COMMON=$(ROOT)/compilerlibs/ocamlcommon.cma
BYTECMP=$(ROOT)/compilerlibs/ocamlbytecomp.cma
TOPLVL=$(ROOT)/compilerlibs/ocamltoplevel.cma

tracer.exe: tracer.ml
	$(OCAMLC) -o $@ $(COMMON) $(ROOT)/tools/ast_mapper.cmo tracer.ml

ifdef.exe: ifdef.ml
	$(OCAMLC) -o $@ $(COMMON) $(ROOT)/tools/ast_mapper.cmo ifdef.ml

js_syntax.exe: js_syntax.ml
	$(OCAMLC) -o $@ $(COMMON) $(ROOT)/tools/ast_mapper.cmo js_syntax.ml


test_trace.exe: tracer.exe test_trace.ml
	$(OCAMLC) -o test_trace.exe -ppx ./tracer.exe test_trace.ml

test_ifdef.exe: ifdef.exe test_ifdef.ml
	$(OCAMLC) -o test_ifdef.exe -ppx ./ifdef.exe test_ifdef.ml

test_js.exe: js_syntax.exe test_js.ml
	$(OCAMLC) -o test_ifdef.exe -i -ppx ./js_syntax.exe test_js.ml

clean:
	rm -f *.exe *.cm*


.PHONY: minidoc
minidoc: minidoc.ml
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
