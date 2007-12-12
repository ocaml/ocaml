include ../support/Makefile.common

COMPFLAGS= -I ../support -I $(OTHERS)/win32unix -I $(OTHERS)/unix

all: camltkobjs

opt: camltkobjsx

include ./modules

CAMLTKOBJS= $(CWIDGETOBJS) cTk.cmo camltk.cmo
CAMLTKOBJSX = $(CAMLTKOBJS:.cmo=.cmx)

camltkobjs: $(CAMLTKOBJS)

camltkobjsx: $(CAMLTKOBJSX)

clean:
	$(MAKE) -f Makefile.gen clean

install:
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(CAMLTKOBJS:.cmo=.cmi) $(CWIDGETOBJS:.cmo=.mli) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmi

installopt:
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(CAMLTKOBJSX) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmx

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
