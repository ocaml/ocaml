include ../support/Makefile.common

COMPFLAGS= -I ../support

SUPPORT=../support/support.cmo ../support/widget.cmo ../support/protocol.cmo \
        ../support/textvariable.cmo ../support/timer.cmo \
        ../support/fileevent.cmo

SUPPORTX = $(SUPPORT:.cmo=.cmx)

TOPDEPS = $(TOPDIR)/toplevel/toplevellib.cma $(TOPDIR)/toplevel/topmain.cmo

all : labltk.cma labltktop labltk

opt : labltk.cmxa

include ./modules
WIDGETOBJSX = $(WIDGETOBJS:.cmo=.cmx)

labltk.cma : $(SUPPORT) $(WIDGETOBJS) tk.cmo 
	$(LABLLIBR) -custom -o labltk.cma $(SUPPORT) tk.cmo $(WIDGETOBJS) \
          -cclib -llabltk41 $(TK_LINK) $(X11_LINK)

labltk.cmxa : $(SUPPORTX) $(WIDGETOBJSX) tk.cmx
	$(CAMLOPTLIBR) -o labltk.cmxa $(SUPPORTX) tk.cmx $(WIDGETOBJSX) \
          -cclib -llabltk41 $(TK_LINK) $(X11_LINK)

labltktop : $(TOPDEPS) $(WIDGETOBJS) $(SUPPORT)
	$(LABLC) -custom -linkall -o labltktop -I ../support \
	       -I $(TOPDIR)/toplevel toplevellib.cma labltk.cma \
	       -I $(OTHERS)/unix unix.cma \
	       -I $(OTHERS)/str str.cma \
	       topmain.cmo

labltk: Makefile $(TOPDIR)/config/Makefile
	@echo Generate $@
	@echo "#!/bin/sh" > $@
	@echo 'exec $(LABLTKDIR)/labltktop -I $(LABLTKDIR) $$*' >> $@

# All .{ml,mli} files are generated in this directory
clean : 
	rm -f *.cm* *.ml *.mli *.o *.a labltktop

install: labltk.cma labltktop labltk
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp *.cmi labltk.cma labltktop $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/*.cmi
	chmod 644 $(LABLTKDIR)/labltk.cma
	chmod 755 $(LABLTKDIR)/labltktop
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	cp labltk $(BINDIR)
	chmod 755 $(BINDIR)/labltk


installopt: labltk.cmxa
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp labltk.cmxa labltk.a $(LABLTKDIR)
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
