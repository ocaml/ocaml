# $Id$

include ../config/Makefile

SHELL=/bin/sh

INCLUDES=-I ../camlp4 -I ../boot -I ../odyl -I $(OTOP)/otherlibs/dynlink
OCAMLCFLAGS=-warn-error A $(INCLUDES)
LINKFLAGS=$(INCLUDES)
OBJS=ocpp.cmo

all: ocpp$(EXE)

ocpp$(EXE): $(OBJS)
	$(OCAMLC) $(LINKFLAGS) ../boot/stdpp.cmo ../camlp4/reloc.cmo ../camlp4/quotation.cmo ../odyl/odyl.cma $(OBJS) ../odyl/odyl.cmo -linkall -o ocpp$(EXE)

clean::
	rm -f *.cm[ioa] *.pp[io] *.o *.out *.bak .*.bak ocpp$(EXE)

install:
	-$(MKDIR) "$(LIBDIR)/camlp4" "$(BINDIR)"
	cp $(OBJS) "$(LIBDIR)/camlp4/."
	cp ocpp$(EXE) "$(BINDIR)/."

depend:
