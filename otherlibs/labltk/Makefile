# Top Makefile for LablTk

SUBDIRS=compiler support lib jpf example browser

all:
	cd support; $(MAKE)
	cd compiler; $(MAKE)
	cd lib; $(MAKE) -f Makefile.gen; $(MAKE)
	cd jpf; $(MAKE)
	cd browser; $(MAKE)

allopt:
	cd support; $(MAKE) opt
	cd lib; $(MAKE) -f Makefile.gen; $(MAKE) opt
	cd jpf; $(MAKE) opt

lib: Widgets.src
	compiler/tkcompiler
	cd lib; $(MAKE)

example: example/all

example/all:
	cd example; $(MAKE) all

install: 
	cd lib; $(MAKE) install
	cd support; $(MAKE) install
	cd compiler; $(MAKE) install
	cd jpf; $(MAKE) install
	cd browser; $(MAKE) install

installopt:
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
