include ../support/Makefile.common

COMPFLAGS= -I ../support

SUPPORT=../support/support.cmo ../support/widget.cmo ../support/protocol.cmo \
        ../support/textvariable.cmo ../support/timer.cmo \
        ../support/fileevent.cmo

SUPPORTX = $(SUPPORT:.cmo=.cmx)

TOPDEPS = $(TOPDIR)/toplevel/toplevellib.cma $(TOPDIR)/toplevel/topmain.cmo

all : labltk.cma labltktop$(EXE) labltk

opt : labltk.cmxa

include ./modules
WIDGETOBJSX = $(WIDGETOBJS:.cmo=.cmx)

labltk.cma : $(SUPPORT) $(WIDGETOBJS) tk.cmo 
	$(MKLIB) -ocamlc '$(LABLC)' -o labltk -oc labltk41 \
          $(SUPPORT) tk.cmo $(WIDGETOBJS) \
          $(TK_LINK) $(X11_LINK)

labltk.cmxa : $(SUPPORTX) $(WIDGETOBJSX) tk.cmx
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o labltk -oc labltk41 \
          $(SUPPORTX) tk.cmx $(WIDGETOBJSX) \
          $(TK_LINK) $(X11_LINK)

labltktop$(EXE) : $(TOPDEPS) $(WIDGETOBJS) $(SUPPORT) ../support/liblabltk41.a
	$(LABLC) -linkall -o labltktop$(EXE) -I ../support \
	       -I $(TOPDIR)/toplevel toplevellib.cma labltk.cma \
	       -I $(OTHERS)/unix unix.cma \
	       -I $(OTHERS)/str str.cma \
	       topmain.cmo

labltk: Makefile $(TOPDIR)/config/Makefile
	@echo Generate $@
	@echo "#!/bin/sh" > $@
	@echo 'exec $(LABLTKDIR)/labltktop$(EXE) -I $(LABLTKDIR) $$*' >> $@

# All .{ml,mli} files are generated in this directory
clean : 
	rm -f *.cm* *.ml *.mli *.o *.a labltktop$(EXE)

install: labltk.cma labltktop$(EXE) labltk
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(WIDGETOBJS:.cmo=.cmi) tk.cmi $(LABLTKDIR)
	cp labltk.cma labltktop$(EXE) $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/*.cmi
	chmod 644 $(LABLTKDIR)/labltk.cma
	chmod 755 $(LABLTKDIR)/labltktop$(EXE)
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	cp labltk $(BINDIR)
	chmod 755 $(BINDIR)/labltk


installopt: labltk.cmxa
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(SUPPORTX) $(WIDGETOBJSX) tk.cmx $(LABLTKDIR)
	cp labltk.cmxa labltk.a $(LABLTKDIR)
	cd $(LABLTKDIR); $(RANLIB) labltk.a
	chmod 644 $(LABLTKDIR)/*.cmx
	chmod 644 $(LABLTKDIR)/labltk.cmxa
	chmod 644 $(LABLTKDIR)/labltk.a
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
