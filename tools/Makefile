include ../Makefile.config

CAMLRUN=../boot/cslrun
CAMLC=$(CAMLRUN) ../boot/cslc -I ../boot
CAMLLEX=$(CAMLRUN) ../boot/csllex
INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../asmcomp
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: csldep cslprof

# The dependency generator

CAMLDEP=csldep.cmo

csldep: $(CAMLDEP)
	$(CAMLC) $(LINKFLAGS) -o csldep misc.cmo $(CAMLDEP)

clean::
	rm -f csldep

csldep.ml: csldep.mll
	$(CAMLLEX) csldep.mll

clean::
	rm -f csldep.ml

install::
	cp csldep $(BINDIR)/csldep

beforedepend:: csldep

# The profiler

CSLPROF=cslprof.cmo
CSLPROF_IMPORTS=misc.cmo config.cmo clflags.cmo terminfo.cmo \
  location.cmo longident.cmo parser.cmo lexer.cmo parse.cmo

cslprof: $(CSLPROF)
	$(CAMLC) $(LINKFLAGS) -o cslprof $(CSLPROF_IMPORTS) $(CSLPROF)

install::
	cp cslprof $(BINDIR)/cslprof
	cp cslcp $(BINDIR)/cslcp

clean::
	rm -f cslprof

# To make custom toplevels

install::
	cp cslmktop $(BINDIR)/cslmktop

# The bytecode disassembler

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

# Dump .cmx files

dumpapprox: dumpapprox.cmo
	$(CAMLC) $(LINKFLAGS) -o dumpapprox config.cmo dumpapprox.cmo

clean::
	rm -f dumpapprox

# Print imported interfaces for .cmo files

objinfo: objinfo.cmo
	$(CAMLC) $(LINKFLAGS) -o objinfo config.cmo objinfo.cmo

clean::
	rm -f objinfo

# Common stuff

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

clean::
	rm -f *.cmo *.cmi

depend: beforedepend
	$(CAMLRUN) ./csldep $(INCLUDES) *.mli *.ml > .depend

include .depend
