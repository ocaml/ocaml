include ../support/Makefile.common

COMPFLAGS= -I ../support

SUPPORT=../support/support.cmo ../support/widget.cmo ../support/protocol.cmo \
        ../support/textvariable.cmo ../support/timer.cmo \
        ../support/fileevent.cmo

SUPPORTX = $(SUPPORT:.cmo=.cmx)

TOPDEPS = $(TOPDIR)/toplevel/toplevellib.cma $(TOPDIR)/toplevel/topmain.cmo

all : tk41.cma labltktop labltk labltklink labltkopt

opt : tk41.cmxa

include ./modules
WIDGETOBJSX = $(WIDGETOBJS:.cmo=.cmx)

tk41.cma : $(SUPPORT) $(WIDGETOBJS) tk.cmo 
	$(LABLLIBR) -custom -o tk41.cma $(SUPPORT) tk.cmo $(WIDGETOBJS) \
          -cclib -llabltk41 $(TK_LINK) $(X11_LINK)

tk41.cmxa : $(SUPPORTX) $(WIDGETOBJSX) tk.cmx
	$(CAMLOPTLIBR) -o tk41.cmxa $(SUPPORTX) tk.cmx $(WIDGETOBJSX) \
          -cclib -llabltk41 $(TK_LINK) $(X11_LINK)

labltktop : $(TOPDEPS) $(WIDGETOBJS) $(SUPPORT)
	$(LABLC) -custom -linkall -o labltktop -I ../support $(TKLINKOPT) \
	       -I $(TOPDIR)/toplevel toplevellib.cma tk41.cma topmain.cmo

labltk: Makefile $(TOPDIR)/config/Makefile
	@echo Generate $@
	@echo "#!/bin/sh" > $@
	@echo 'exec $(LABLTKDIR)/labltktop -I $(LABLTKDIR) "$$@"' >> $@

labltklink: Makefile $(TOPDIR)/config/Makefile
	@echo Generate $@
	@echo "#!/bin/sh" > $@
	@echo 'exec ocamlc -I $(LABLTKDIR) -cclib -L$(LABLTKDIR) tk41.cma "$$@"' >> $@

labltkopt: Makefile $(TOPDIR)/config/Makefile
	@echo Generate $@
	@echo "#!/bin/sh" > $@
	@echo 'exec ocamlopt -I $(LABLTKDIR) -cclib -L$(LABLTKDIR) tk41.cmxa "$$@" ' >> $@

# All .{ml,mli} files are generated in this directory
clean : 
	rm -f *.cm* *.ml *.mli *.o *.a labltktop

install: tk41.cma labltktop labltk labltklink
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp *.cmi tk41.cma labltktop $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/*.cmi
	chmod 644 $(LABLTKDIR)/tk41.cma
	chmod 755 $(LABLTKDIR)/labltktop
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	cp labltk labltklink $(BINDIR)
	chmod 755 $(BINDIR)/labltk $(BINDIR)/labltklink


installopt: tk41.cmxa
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp tk41.cmxa tk41.a $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/tk41.cmxa
	chmod 644 $(LABLTKDIR)/tk41.a
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	cp labltkopt $(BINDIR)
	chmod 755 $(BINDIR)/labltkopt

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
