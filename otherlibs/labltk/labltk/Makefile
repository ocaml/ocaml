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
	if test -d $(MLTKDIR); then : ; else mkdir $(MLTKDIR); fi
	cp $(LABLTKOBJS:.cmo=.cmi) $(WIDGETOBJS:.cmo=.mli) $(MLTKDIR)
	chmod 644 $(MLTKDIR)/*.cmi

installopt: $(LABLTKOBJSX)
	@if test -d $(MLTKDIR); then : ; else mkdir $(MLTKDIR); fi
	cp $(LABLTKOBJSX) $(MLTKDIR)
	chmod 644 $(MLTKDIR)/*.cmx

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
