include ../Makefile.config

COMPFLAGS= -I ../support

TKLINKOPT=$(STATIC) \
          -ccopt -L../support -cclib -llabltk41 \
	  $(TKLIBS) $(X11_LIBS)


SUPPORT=../support/support.cmo ../support/widget.cmo ../support/protocol.cmo \
        ../support/textvariable.cmo ../support/timer.cmo \
        ../support/fileevent.cmo

SUPPORTX = $(SUPPORT:.cmo=.cmx)

all : tk41.cma labltktop

opt : tk41.cmxa

include ./modules
WIDGETOBJSX = $(WIDGETOBJS:.cmo=.cmx)

tk41.cma : $(SUPPORT) $(WIDGETOBJS) tk.cmo 
	$(LABLLIBR) -o tk41.cma $(SUPPORT) tk.cmo $(WIDGETOBJS)

tk41.cmxa : $(SUPPORTX) $(WIDGETOBJSX) tk.cmx
	$(CAMLOPTLIBR) -o tk41.cmxa $(SUPPORTX) tk.cmx $(WIDGETOBJSX)

## Until olabltktop is fixed (next release), we in-line it
## (otherwise our trick with -ccopt is broken)

labltktop : $(WIDGETOBJS) $(SUPPORT)
	$(LABLC) -custom -linkall -o labltktop -I ../support $(TKLINKOPT) \
	       toplevellib.cma tk41.cma topmain.cmo

# All .{ml,mli} files are generated in this directory
clean : 
	rm -f *.cm* *.ml *.mli *.o *.a labltktop

install: tk41.cma labltktop
	cp *.cmi tk41.cma labltktop $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmi
	chmod 644 $(INSTALLDIR)/tk41.cma
	chmod 755 $(INSTALLDIR)/labltktop


installopt: tk41.cmxa
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp tk41.cmxa tk41.a $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/tk41.cmxa
	chmod 644 $(INSTALLDIR)/tk41.a

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
