# $Id$

include ../config/Makefile.cnf

EXECUTABLES=ocpp
OBJS=ocpp.cmo
INCLUDES=-I ../camlp4 -I ../lib -I ../odyl -I $(OTOP)/otherlibs/dynlink
OCPPM=../lib/debug.cmo ../lib/loc.cmo ../lib/stdpp.cmo ../camlp4/quotation.cmo

include ../config/Makefile.base

ocpp$(EXE): $(OBJS)
	$(OCAMLC) $(LINKFLAGS) $(OCPPM) ../odyl/odyl.cma $(OBJS) ../odyl/odyl.cmo -linkall -o $@

ocpp.opt: $(OBJSX)
	$(OCAMLOPT) $(LINKFLAGS) $(OCPPM:.cmo=.cmx) ../odyl/odyl.cmxa $(OBJSX) ../odyl/odyl.cmx -linkall -o $@

install-local:
	-$(MKDIR) "$(LIBDIR)/camlp4" "$(BINDIR)"
	cp $(OBJS) "$(LIBDIR)/camlp4/."
	cp ocpp$(EXE) "$(BINDIR)/."
	if test -f ocpp.opt ; then \
	  cp ocpp.opt "$(LIBDIR)/camlp4/." ; \
	fi
