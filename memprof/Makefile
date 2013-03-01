OCAMLC=../byterun/ocamlrun ../ocamlc
OCAMLDEP=../byterun/ocamlrun ../boot/ocamldep
BOOT_OCAMLC=../boot/ocamlc	
BOOT_OCAMLRUM=../boot/ocamlrun
SOURCESML=heap_types.ml utils.ml retype.ml  load_heap.ml profiler.ml #hPCompute.ml hPScanHeap.ml 
SOURCESMLI=retype.mli
TESTSOURCES=test2.ml test1.ml test.ml
LIBS=-I +compiler-libs ocamlbytecomp.cma ocamlcommon.cma ocamltoplevel.cma -I ../tools/ untypeast.cmo pprintast.cmo typedtreeIter.cmo cmt2annot.cmo str.cma 
FLAGS= -bin-annot #-g
EXE=bin/profiler


all: hp profiler test 
	$(OCAMLC) -bin-annot -o exn test2.cmo test_exn.ml 

.SUFFIXES: .ml .mli .cmo .cmi .cmx

test: $(TESTSOURCES:.ml=.cmo)
	$(OCAMLC) $(FLAGS) unix.cma $(TESTSOURCES:.ml=.cmo) -o test

profiler: $(SOURCESML:.mli=.cmi) $(SOURCESML:.ml=.cmo)
	$(OCAMLC) $(FLAGS) $(LIBS) $(SOURCESML:.ml=.cmo) -o $(EXE)

hp:
	$(MAKE) -C hp2ps

.ml.cmo:
	$(OCAMLC) $(FLAGS) $(LIBS) -c $<

.mli.cmi:
	$(OCAMLC) $(FLAGS) $(LIBS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(FLAGS) $(LIBS:.cma=.cmxa) -c $<

clean:
	rm -rf *.cm* 

cleanall: clean
	rm -f  *.prof  heap.dump.* *.hp *.aux *.ps
	rm -f bin/*
	$(MAKE) -C hp2ps clean

depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend