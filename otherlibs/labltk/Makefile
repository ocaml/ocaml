include Makefile.config

SUBDIRS=compiler support lib jpf example browser

all:    Makefile.config
	cd support; $(MAKE)
	cd compiler; $(MAKE)
	cd lib; $(MAKE) -f Makefile.gen; $(MAKE)
	cd jpf; $(MAKE)

opt:	Makefile.config
	cd support; $(MAKE) opt
	cd lib; $(MAKE) -f Makefile.gen; $(MAKE) opt
	cd jpf; $(MAKE) opt

Makefile.config:
	@echo "You must configure first. Read INSTALL."
	exit 1

lib: Widgets.src
	compiler/tkcompiler
	cd lib; $(MAKE)

example: example/all

example/all:
	cd example; $(MAKE) all

browser: browser/all

browser/all:
	cd browser; \
	$(MAKE) LINKER='ocamlc -custom -ccopt -L../support -cclib -llabltk41 $(TKLIBS) $(X11_LIBS)' \
		LABLTKLIB="-I ../lib -I ../support"

install: 
	if test -d $(INSTALLBINDIR); then : ; \
		else mkdir -p $(INSTALLBINDIR); fi
	cp labltklink labltk $(INSTALLBINDIR)
	chmod 755 $(INSTALLBINDIR)/labltklink $(INSTALLBINDIR)/labltk
	if test -d $(INSTALLDIR); then : ; else mkdir -p $(INSTALLDIR); fi
	cd lib; $(MAKE) install
	cd support; $(MAKE) install
	cd compiler; $(MAKE) install
	cd jpf; $(MAKE) install
	cd browser; $(MAKE) install
	if test -f lib/tk41.cmxa; then $(MAKE) installopt; fi

installopt:
	cp labltkopt $(INSTALLBINDIR)
	chmod 755 $(INSTALLBINDIR)/labltkopt
	cd lib; $(MAKE) installopt
	cd jpf; $(MAKE) installopt

clean : 
	-rm -f config.cache
	for d in $(SUBDIRS); do \
	cd $$d; $(MAKE) clean; cd ..; \
	done

distclean:  clean
	-rm -f config.log config.status config.cache
	-rm -f Makefile.config labltklink labltk labltkopt
