include ../../config/Makefile

OCAMLC = ../../ocamlc
OCAMLOPT = ../../ocamlopt

test:
	$(OCAMLC) -c plugin.ml
	$(OCAMLC) -output-obj -o main_obj.$(O) dynlink.cma entry.c main.ml
	$(MKDLL) -maindll -o main.dll main_obj.$(O) entry.$(O) ../../byterun/libcamlrun.$(A)  $(BYTECCLIBS) -v
	csc /out:main.exe main.cs
	./main.exe

test2:
	$(OCAMLC) -c plugin.ml
	$(OCAMLC) -output-obj -o main.dll dynlink.cma main.ml entry.c
	csc /out:main.exe main.cs
	./main.exe

testopt:
	$(OCAMLOPT) -o plugin.cmxs -shared plugin.ml
	$(OCAMLOPT) -output-obj -o main_obj.$(O) dynlink.cmxa entry.c main.ml
	$(MKDLL) -maindll -o main.dll main_obj.$(O) entry.$(O) ../../asmrun/libasmrun.lib -v
	csc /out:main.exe main.cs
	./main.exe

testopt2:
	$(OCAMLOPT) -o plugin.cmxs -shared plugin.ml
	$(OCAMLOPT) -output-obj -o main.dll dynlink.cmxa entry.c main.ml
	csc /out:main.exe main.cs
	./main.exe

clean:
	rm -f *.exe *~ *.cm* *.obj *.o *.dll
