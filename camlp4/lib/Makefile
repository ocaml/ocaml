# $Id$

include ../config/Makefile

INCLUDES=
OCAMLCFLAGS=-warn-error A $(INCLUDES)
OBJS=stdpp.cmo token.cmo plexer.cmo gramext.cmo grammar.cmo extfold.cmo extfun.cmo fstream.cmo
SHELL=/bin/sh
TARGET=gramlib.cma

all: $(TARGET)
opt: $(TARGET:.cma=.cmxa)

$(TARGET): $(OBJS)
	$(OCAMLC) $(OBJS) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(OBJS:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

clean::
	rm -f *.cm[ioax] *.cmxa *.pp[io] *.o *.a *.bak .*.bak $(TARGET)

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
	if test -f $(TARGET:.cma=.cmxa); then $(MAKE) installopt LIBDIR="$(LIBDIR)"; fi

installopt:
	cp $(TARGET:.cma=.cmxa) *.cmx "$(LIBDIR)/camlp4/."
	if test -f $(TARGET:.cma=.lib); then \
	  cp $(TARGET:.cma=.lib) "$(LIBDIR)/camlp4/."; \
	else \
	  tar cf - $(TARGET:.cma=.a) | (cd "$(LIBDIR)/camlp4/."; tar xf -); \
	fi

include .depend
