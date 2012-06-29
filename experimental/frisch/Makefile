ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -w A-4-9

ast_mapper.cmo: ast_mapper.ml
	$(OCAMLC) -c $<

tracer.exe: ast_mapper.cmo tracer.ml
	$(OCAMLC) -o $@ $(ROOT)/compilerlibs/ocamlcommon.cma ast_mapper.cmo tracer.ml

ifdef.exe: ast_mapper.cmo ifdef.ml
	$(OCAMLC) -o $@ $(ROOT)/compilerlibs/ocamlcommon.cma ast_mapper.cmo ifdef.ml

test_trace.exe: tracer.exe test_trace.ml
	$(OCAMLC) -o test_trace.exe -ppx ./tracer.exe test_trace.ml

test_ifdef.exe: ifdef.exe test_ifdef.ml
	$(OCAMLC) -o test_ifdef.exe -ppx ./ifdef.exe test_ifdef.ml

clean:
	rm -f *.exe *.cm*
