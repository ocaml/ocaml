ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -I $(ROOT)/tools -I $(ROOT)/typing -w A-4-9

tracer.exe: tracer.ml
	$(OCAMLC) -o $@ $(ROOT)/compilerlibs/ocamlcommon.cma $(ROOT)/tools/ast_mapper.cmo tracer.ml

ifdef.exe: ifdef.ml
	$(OCAMLC) -o $@ $(ROOT)/compilerlibs/ocamlcommon.cma $(ROOT)/tools/ast_mapper.cmo ifdef.ml

js_syntax.exe: js_syntax.ml
	$(OCAMLC) -o $@ $(ROOT)/compilerlibs/ocamlcommon.cma $(ROOT)/tools/ast_mapper.cmo js_syntax.ml


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
	$(OCAMLC) -custom -o minidoc.exe $(ROOT)/compilerlibs/ocamlcommon.cma minidoc.ml
	$(OCAMLC) -c -bin-annot testdoc.mli
	./minidoc.exe testdoc.cmti

.PHONY: lifter
lifter: print_gen.ml
	$(OCAMLC) -w +A-4 -custom -o print_gen.exe $(ROOT)/compilerlibs/ocamlcommon.cma print_gen.ml
	./print_gen.exe -I ../../parsing -I ../../stdlib Parsetree.expression > lifter.ml
	$(OCAMLC) -c -w +A-17 lifter.ml
	$(OCAMLC) -c lifter_ex.ml
	$(OCAMLC) -o lifter_ex.exe $(ROOT)/compilerlibs/ocamlcommon.cma lifter.cmo lifter_ex.cmo
	./lifter_ex.exe
