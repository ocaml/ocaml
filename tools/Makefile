CAMLRUN=../boot/camlrun
CAMLC=$(CAMLRUN) ../boot/camlc -I ../boot
CAMLLEX=$(CAMLRUN) ../boot/camllex
INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../asmcomp
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: camldep

CAMLDEP=camldep.cmo

camldep: $(CAMLDEP)
	$(CAMLC) $(LINKFLAGS) -o camldep misc.cmo $(CAMLDEP)

clean::
	rm -f camldep

camldep.ml: camldep.mll
	$(CAMLLEX) camldep.mll

clean::
	rm -f camldep.ml

beforedepend:: camldep

DUMPOBJ=opnames.cmo dumpobj.cmo

dumpobj: $(DUMPOBJ)
	$(CAMLC) $(LINKFLAGS) -o dumpobj misc.cmo tbl.cmo config.cmo ident.cmo opcodes.cmo runtimedef.cmo $(DUMPOBJ)

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

dumpapprox: dumpapprox.cmo
	$(CAMLC) $(LINKFLAGS) -o dumpapprox config.cmo dumpapprox.cmo

clean::
	rm -f dumpapprox

objinfo: objinfo.cmo
	$(CAMLC) $(LINKFLAGS) -o objinfo config.cmo objinfo.cmo

clean::
	rm -f objinfo

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

clean::
	rm -f *.cmo *.cmi

depend: beforedepend
	$(CAMLRUN) camldep $(INCLUDES) *.mli *.ml > .depend

include .depend
