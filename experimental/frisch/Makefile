ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -w A-4

tracer.exe: tracer.ml
	$(OCAMLC) -o tracer.exe $(ROOT)/compilerlibs/ocamlcommon.cma tracer.ml

test_trace.exe: tracer.exe test_trace.ml
	$(OCAMLC) -o test_trace.exe -ppx ./tracer.exe test_trace.ml

clean:
	rm -f *.exe *.cm*
