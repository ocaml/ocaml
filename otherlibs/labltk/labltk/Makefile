include ../support/Makefile.common

COMPFLAGS= -I ../support

all: labltkobjs

opt: labltkobjsx

include ./modules

LABLTKOBJS = $(WIDGETOBJS) tk.cmo labltk.cmo
LABLTKOBJSX = $(LABLTKOBJS:.cmo=.cmx)

labltkobjs: $(LABLTKOBJS)

labltkobjsx: $(LABLTKOBJSX)

# All .{ml,mli} files are generated in this directory
clean: 
	rm -f *.cm* *.ml *.mli *.o *.a
	$(MAKE) -f Makefile.gen clean

install: $(LABLTKOBJS)
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(LABLTKOBJS:.cmo=.cmi) $(WIDGETOBJS:.cmo=.mli) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmi

installopt: $(LABLTKOBJSX)
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(LABLTKOBJSX) $(INSTALLDIR)
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
