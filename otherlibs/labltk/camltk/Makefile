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
	if test -d $(MLTKDIR); then : ; else mkdir $(MLTKDIR); fi
	cp $(CAMLTKOBJS:.cmo=.cmi) $(CWIDGETOBJS:.cmo=.mli) $(MLTKDIR)
	chmod 644 $(MLTKDIR)/*.cmi

installopt: $(CAMLTKOBJSX)
	@if test -d $(MLTKDIR); then : ; else mkdir $(MLTKDIR); fi
	cp $(CAMLTKOBJSX) $(MLTKDIR)
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
