# $Id$

include ../config/Makefile

SHELL=/bin/sh

INCLUDES=-I ../camlp4 -I ../boot -I ../odyl -I $(OTOP)/otherlibs/dynlink
OCAMLCFLAGS=-warn-error A $(INCLUDES)
LINKFLAGS=$(INCLUDES)
OBJS=crc.cmo ocpp.cmo
INTERFACES=-I $(OLIBDIR) Arg Array Callback Char Digest Filename Format Gc Genlex Hashtbl Lexing List Map Obj Oo Parsing Pervasives Printexc Printf Queue Random Set Sort Stack Stream String Sys Weak -I ../boot Gramext Grammar Plexer Stdpp Token -I ../camlp4 MLast Quotation

all: ocpp$(EXE)

ocpp$(EXE): $(OBJS)
	$(OCAMLC) $(LINKFLAGS) ../boot/stdpp.cmo ../camlp4/quotation.cmo ../odyl/odyl.cma $(OBJS) ../odyl/odyl.cmo -linkall -o ocpp$(EXE)

crc.cmo:
	@OTOP=$(OTOP) EXE=$(EXE) ../tools/extract_crc.sh $(INTERFACES) > crc.ml
	echo "let _ = Dynlink.add_available_units crc_unit_list" >> crc.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c crc.ml

clean::
	rm -f *.cm[ioa] *.pp[io] *.o *.out *.bak .*.bak crc.ml ocpp$(EXE)

install:
	-$(MKDIR) $(LIBDIR)/camlp4 $(BINDIR)
	cp $(OBJS) $(LIBDIR)/camlp4/.
	cp ocpp$(EXE) $(BINDIR)/.

depend:
