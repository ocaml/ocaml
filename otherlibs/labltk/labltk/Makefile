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
	if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(LABLTKOBJS:.cmo=.cmi) $(WIDGETOBJS:.cmo=.mli) labltk.mli $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/*.cmi

installopt: $(LABLTKOBJSX)
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(LABLTKOBJSX) $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/*.cmx

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
