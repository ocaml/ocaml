CAMLC=../boot/camlrun ../boot/camlc -I ../boot
INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: dumpobj

DUMPOBJ=opnames.zo dumpobj.zo

dumpobj: $(DUMPOBJ)
	$(CAMLC) $(LINKFLAGS) -o dumpobj misc.zo tbl.zo config.zo ident.zo opcodes.zo runtimedef.zo $(DUMPOBJ)

clean::
	rm -f dumpobj

opnames.ml: ../byterun/instruct.h
	sed -e '/\/\*/d' \
            -e 's/enum \(.*\) {/let names_of_\1 = [|/' \
            -e 's/};$$/ |]/' \
            -e 's/\([A-Z][A-Z_0-9a-z]*\)/"\1"/g' \
            -e 's/,/;/g' \
        ../byterun/instruct.h > opnames.ml

clean::
	rm -f opnames.ml

beforedepend:: opnames.ml

.SUFFIXES:
.SUFFIXES: .ml .zo .mli .zi

.ml.zo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.zi:
	$(CAMLC) -c $(COMPFLAGS) $<

clean::
	rm -f *.zo *.zi

depend: beforedepend
	camldep $(INCLUDES) *.mli *.ml > .depend

include .depend
