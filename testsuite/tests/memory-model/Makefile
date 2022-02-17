OCAMLRUN=../../../boot/ocamlrun
OPTS=-nostdlib -I ../../../stdlib
OCAMLC=$(OCAMLRUN) ../../../ocamlc $(OPTS)
OCAMLOPT=$(OCAMLRUN) ../../../ocamlopt $(OPTS)


PGM=forbidden.exe publish.exe
MOREPGM=
MOREBYT=$(MOREPGM:.exe=.byt)
BYT=$(PGM:.exe=.byt)
CMO=$(PGM:.exe=.cmo)
CMX=$(PGM:.exe=.cmx)
OBJ=opt.cmx barrier.cmx hist.cmx shared.cmx run.cmx outcome.cmx
BBJ=$(OBJ:.cmx=.cmo)
SRC=$(OBJ:.cmx=.ml) $(PGM:.exe=.ml) $(MOREPGM:.exe=.ml)
MLI=$(OBJ:.cmx=.mli)

all: native byte

native: $(PGM)

byte: $(BYT)

clean::
	/bin/rm -f $(MOREPGM) $(MOREBYT) $(BYT) $(PGM) *.cmo *.cmi *.cmx *.o *~ *.s

%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

%.s: %.ml
	$(OCAMLOPT) -S -c $<

%.exe: %.cmx $(OBJ)
	$(OCAMLOPT) -o $@ $(OBJ) $<

%.byt: %.cmo $(BBJ)
	$(OCAMLC) -o $@ $(BBJ) $<

depend:
	ocamldep $(SRC)  $(MLI) > .depend

-include .depend
