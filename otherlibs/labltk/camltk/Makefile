include ../support/Makefile.common

COMPFLAGS= -I ../support

TOPDEPS = $(TOPDIR)/toplevel/toplevellib.cma $(TOPDIR)/toplevel/topmain.cmo

all: camltkobjs

opt: camltkobjsx

include ./modules

CAMLTKOBJS= $(CWIDGETOBJS) cTk.cmo camltk.cmo
CAMLTKOBJSX = $(CAMLTKOBJS:.cmo=.cmx)

camltkobjs: $(CAMLTKOBJS)

camltkobjsx: $(CAMLTKOBJSX)

# All .{ml,mli} files are generated in this directory
clean: 
	rm -f *.cm* *.ml *.mli *.o *.a
	$(MAKE) -f Makefile.gen clean

install: $(CAMLTKOBJS)
	if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(CAMLTKOBJS:.cmo=.cmi) $(CWIDGETOBJS:.cmo=.mli) $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/*.cmi

installopt: $(CAMLTKOBJSX)
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(CAMLTKOBJSX) $(LABLTKDIR)
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
