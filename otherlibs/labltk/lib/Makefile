include ../support/Makefile.common

all: labltk.cma labltktop$(EXE) labltk

opt: labltk.cmxa

clean: 
	rm -f labltk.cma labltk.cmxa labltktop$(EXE) labltk

include ../labltk/modules
LABLTKOBJS=tk.cmo $(WIDGETOBJS)

include ../camltk/modules
CAMLTKOBJS=cTk.cmo $(CWIDGETOBJS) labltk.cmo camltk.cmo

SUPPORT=../support/support.cmo ../support/rawwidget.cmo \
	../support/widget.cmo ../support/protocol.cmo \
        ../support/textvariable.cmo ../support/timer.cmo \
        ../support/fileevent.cmo ../support/camltkwrap.cmo

TKOBJS=$(SUPPORT) $(LABLTKOBJS) $(CAMLTKOBJS)

TOPDEPS = $(TOPDIR)/toplevel/toplevellib.cma $(TOPDIR)/toplevel/topmain.cmo

labltk.cma: $(SUPPORT)
	cd ../labltk; make
	cd ../camltk; make
	$(MKLIB) -ocamlc '$(LABLC)' -o labltk -oc labltk41 \
          -I ../labltk -I ../camltk $(TKOBJS) \
          $(TK_LINK) $(X11_LINK)

labltk.cmxa: $(SUPPORT:.cmo=.cmx)
	cd ../labltk; make opt
	cd ../camltk; make opt
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o labltk -oc labltk41 \
          -I ../labltk -I ../camltk $(TKOBJS:.cmo=.cmx) \
          $(TK_LINK) $(X11_LINK)

labltktop$(EXE) : $(TOPDEPS) labltk.cma ../support/liblabltk41.a
	$(LABLC) -linkall -o labltktop$(EXE) -I ../support \
	       -I $(TOPDIR)/toplevel toplevellib.cma \
	       -I ../labltk -I ../camltk labltk.cma \
	       -I $(OTHERS)/unix unix.cma \
	       -I $(OTHERS)/str str.cma \
	       $(DLLPATH) \
	       topmain.cmo

labltk: Makefile $(TOPDIR)/config/Makefile
	@echo Generate $@
	@echo "#!/bin/sh" > $@
	@echo 'exec $(LABLTKDIR)/labltktop$(EXE) -I $(LABLTKDIR) $$*' >> $@

install: all
	if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	if test `grep -s -c '^$(LABLTKDIR)$$' $(LIBDIR)/ld.conf || :` = 0; \
        then echo $(LABLTKDIR) >> $(LIBDIR)/ld.conf; fi
	cp labltk.cma labltktop$(EXE) $(LABLTKDIR)
	chmod 644 $(LABLTKDIR)/labltk.cma
	chmod 755 $(LABLTKDIR)/labltktop$(EXE)
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	cp labltk $(BINDIR)
	chmod 755 $(BINDIR)/labltk

installopt: opt
	@if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp labltk.cmxa labltk.a $(LABLTKDIR)
	cd $(LABLTKDIR); $(RANLIB) labltk.a
	chmod 644 $(LABLTKDIR)/labltk.cmxa
	chmod 644 $(LABLTKDIR)/labltk.a
