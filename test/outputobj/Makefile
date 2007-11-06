OCAMLC = ../../ocamlc
OCAMLOPT = ../../ocamlopt

test:
	$(OCAMLC) -c main.ml
	$(OCAMLC) -c entry.c
	$(OCAMLC) -c plugin.ml
	$(OCAMLC) -output-obj -o main_obj.obj dynlink.cma main.cmo
	flexlink -merge-manifest -maindll -o main.dll main_obj.obj entry.obj ../../byterun/libcamlrun.lib -v
	csc /out:main.exe main.cs
	./main.exe

testopt:
	$(OCAMLOPT) -c main.ml
	$(OCAMLC) -c entry.c
	$(OCAMLOPT) -o plugin.cmxs -shared plugin.ml
	$(OCAMLOPT) -output-obj -o main_obj.obj dynlink.cmxa main.cmx
	flexlink -merge-manifest -maindll -o main.dll main_obj.obj entry.obj ../../asmrun/libasmrun.lib -v
	csc /out:main.exe main.cs
	./main.exe

clean:
	rm -f *.exe *~ *.cm* *.obj *.o *.dll
