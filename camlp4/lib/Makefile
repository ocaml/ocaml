# $Id$

include ../config/Makefile

INCLUDES=
OCAMLCFLAGS=-warn-error A $(INCLUDES)
OBJS=stdpp.cmo token.cmo plexer.cmo gramext.cmo grammar.cmo extfold.cmo extfun.cmo fstream.cmo
SHELL=/bin/sh
TARGET=gramlib.cma

all: $(TARGET)
opt: opt$(PROFILING)

optnoprof: $(TARGET:.cma=.cmxa)
optprof: optnoprof $(TARGET:.cma=.p.cmxa)

$(TARGET): $(OBJS)
	$(OCAMLC) $(OBJS) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(OBJS:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET:.cma=.p.cmxa): $(OBJS:.cmo=.p.cmx)
	$(OCAMLOPT) $(OBJS:.cmo=.p.cmx) -a -o $(TARGET:.cma=.p.cmxa)

clean::
	rm -f *.cm[ioax] *.cmxa *.pp[io] *.$(O) *.$(A) *.bak .*.bak $(TARGET)

depend:
	cp .depend .depend.bak
	> .depend
	@for i in *.mli *.ml; do \
	  ../tools/apply.sh pr_depend.cmo -- $(INCLUDES) $$i >> .depend; \
	done

promote:
	cp $(OBJS) $(OBJS:.cmo=.cmi) ../boot/.

compare:
	@for j in $(OBJS) $(OBJS:.cmo=.cmi); do \
		if cmp $$j ../boot/$$j; then :; else exit 1; fi; \
	done

install:
	-$(MKDIR) "$(LIBDIR)/camlp4"
	cp $(TARGET) *.mli "$(LIBDIR)/camlp4/."
	cp *.cmi "$(LIBDIR)/camlp4/."
	test -f $(TARGET:.cma=.cmxa) && $(MAKE) installopt LIBDIR="$(LIBDIR)" || true

installopt:
	for f in $(TARGET:.cma=.cmxa) $(TARGET:.cma=.p.cmxa) *.cmx ; do \
		test -f $$f && cp $$f "$(LIBDIR)/camlp4/." || true ; \
	done
	# Special treatment for this one: some versions of make don't like $(A) in $(TARGET:.cma=.$(A)) 
	target="`echo $(TARGET) | sed -e 's/\.cma$$/.$(A)/'`" ; \
	   if test -f $$target ; then \
	      cp $$target "$(LIBDIR)/camlp4/." && ( cd "$(LIBDIR)/camlp4/." && $(RANLIB) $$target ) \
	  fi

include .depend
