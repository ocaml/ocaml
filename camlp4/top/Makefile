# $Id$

include ../config/Makefile

INCLUDES=-I ../camlp4 -I ../boot -I $(OTOP)/utils -I $(OTOP)/parsing -I $(OTOP)/typing -I $(OTOP)/toplevel
OCAMLCFLAGS=-warn-error A $(INCLUDES)

CAMLP4_OBJS=$(OTOP)/utils/config.cmo ../boot/stdpp.cmo ../boot/token.cmo ../boot/plexer.cmo ../boot/gramext.cmo ../boot/grammar.cmo ../boot/extfold.cmo ../boot/extfun.cmo ../boot/fstream.cmo ../camlp4/quotation.cmo  ../camlp4/reloc.cmo ../camlp4/spretty.cmo ../camlp4/pcaml.cmo ../camlp4/ast2pt.cmo
TOP=camlp4_top.cmo
ROBJS=$(CAMLP4_OBJS) ../meta/pa_r.cmo ../meta/pa_rp.cmo rprint.cmo $(TOP)
OOBJS=$(CAMLP4_OBJS) ../etc/pa_o.cmo ../etc/pa_op.cmo $(TOP)
OOOBJS=$(CAMLP4_OBJS) ../etc/pa_o.cmo ../etc/pa_oop.cmo $(TOP)
OBJS=$(OTOP)/utils/config.cmo ../camlp4/quotation.cmo ../camlp4/reloc.cmo ../camlp4/spretty.cmo ../camlp4/pcaml.cmo ../camlp4/ast2pt.cmo camlp4_top.cmo

TARGET=camlp4o.cma camlp4r.cma camlp4_top.cma

all: $(TARGET)

camlp4oo.cma: $(OOOBJS)
	$(OCAMLC) $(OOOBJS) -linkall -a -o camlp4oo.cma

camlp4o.cma: $(OOBJS)
	$(OCAMLC) $(OOBJS) -linkall -a -o camlp4o.cma

camlp4r.cma: $(ROBJS)
	$(OCAMLC) $(ROBJS) -linkall -a -o camlp4r.cma

camlp4_top.cma: $(OBJS)
	$(OCAMLC) $(OBJS) -a -o camlp4_top.cma

clean::
	rm -f *.cm[ioa] *.pp[io] *.o *.bak .*.bak $(TARGET)

depend:
	cp .depend .depend.bak
	> .depend
	@for i in *.mli *.ml; do \
	  ../tools/apply.sh pr_depend.cmo -- $(INCLUDES) $$i | \
	  sed -e 's| \.\./\.\.| $$(OTOP)|g' >> .depend; \
	done

get_promote:

install:
	-$(MKDIR) "$(LIBDIR)/camlp4"
	cp $(TARGET) "$(LIBDIR)/camlp4/."

include .depend
