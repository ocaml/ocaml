include ../../config/Makefile

OCAMLC = ../../ocamlc
OCAMLOPT = ../../ocamlopt

test:
	$(OCAMLC) -c main.ml
	$(OCAMLC) -c entry.c
	$(OCAMLC) -c plugin.ml
	$(OCAMLC) -output-obj -o main_obj.$(O) dynlink.cma main.cmo
	$(MKDLL) -maindll -o main.dll main_obj.$(O) entry.$(O) ../../byterun/libcamlrun.$(A)  $(BYTECCLIBS) -v
	csc /out:main.exe main.cs
	./main.exe

test2:
	$(OCAMLC) -c main.ml
	$(OCAMLC) -c plugin.ml
	$(OCAMLC) -output-obj -o main.dll dynlink.cma main.cmo entry.c
	csc /out:main.exe main.cs
	./main.exe

testopt:
	$(OCAMLOPT) -c main.ml
	$(OCAMLC) -c entry.c
	$(OCAMLOPT) -o plugin.cmxs -shared plugin.ml
	$(OCAMLOPT) -output-obj -o main_obj.$(O) dynlink.cmxa main.cmx
	flexlink -merge-manifest -maindll -o main.dll main_obj.$(O) entry.$(O) ../../asmrun/libasmrun.lib -v
	csc /out:main.exe main.cs
	./main.exe

clean:
	rm -f *.exe *~ *.cm* *.obj *.o *.dll
