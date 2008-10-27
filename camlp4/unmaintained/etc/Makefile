

include ../config/Makefile.cnf

INCLUDES=-I ../camlp4 -I ../lib -I ../meta -I $(OTOP)/lex -I $(OTOP)/parsing
OBJS=q_phony.cmo pa_o.cmo pa_op.cmo pa_oop.cmo pa_ru.cmo pa_extfun.cmo pa_fstream.cmo pa_ifdef.cmo pr_r.cmo pr_rp.cmo pr_o.cmo pr_op.cmo pr_extend.cmo pr_extfun.cmo pr_null.cmo pr_depend.cmo genTraversal.cmo fi_exc_tracer.cmo
INTF=pa_o.cmi
CAMLP4OM=pa_o.cmo pa_op.cmo ../meta/pr_dump.cmo
CAMLP4OMX=$(CAMLP4OM:.cmo=.cmx)
EXECUTABLES=camlp4o

include ../config/Makefile.base

all-local: mkcamlp4.sh

pr_rp.cmo: parserify.cmo pr_rp_main.cmo
	$(OCAMLC) parserify.cmo pr_rp_main.cmo -a -o $@

pr_op.cmo: parserify.cmo pr_op_main.cmo
	$(OCAMLC) parserify.cmo pr_op_main.cmo -a -o $@

pr_rp.cmx: parserify.cmx pr_rp_main.cmx
	$(OCAMLOPT) parserify.cmx pr_rp_main.cmx -a -o pr_rp.cmxa
	mv pr_rp.cmxa pr_rp.cmx
	mv pr_rp.$(A) pr_rp.$(O)

pr_op.cmx: parserify.cmx pr_op_main.cmx
	$(OCAMLOPT) parserify.cmx pr_op_main.cmx -a -o pr_op.cmxa
	mv pr_op.cmxa pr_op.cmx
	mv pr_op.$(A) pr_op.$(O)

camlp4o$(EXE): ../camlp4/camlp4$(EXE) $(CAMLP4OM)
	rm -f camlp4o$(EXE)
	cd ../camlp4; $(MAKE) CAMLP4=../etc/camlp4o$(EXE) CAMLP4M="-I ../etc $(CAMLP4OM)"

camlp4o.opt: $(CAMLP4OMX)
	rm -f camlp4o.opt
	cd ../camlp4; $(MAKE) ../etc/camlp4o.opt CAMLP4OPT=../etc/camlp4o.opt CAMLP4M="-I ../etc $(CAMLP4OMX)"

mkcamlp4.sh: mkcamlp4.sh.tpl version.sh
	sed -e "s!LIBDIR!$(LIBDIR)!g" -e "/define VERSION/r version.sh" \
	    mkcamlp4.sh.tpl > mkcamlp4.sh

version.sh : $(OTOP)/VERSION
	echo "VERSION=\"`head -1 ../VERSION`\"" >version.sh

install-local:
	-$(MKDIR) "$(LIBDIR)/camlp4" "$(BINDIR)"
	cp $(OBJS) "$(LIBDIR)/camlp4/."
	cp $(INTF) "$(LIBDIR)/camlp4/."
	cp camlp4o$(EXE) "$(BINDIR)/."
	if test -f camlp4o.opt; then \
	  cp camlp4o.opt "$(BINDIR)/camlp4o.opt$(EXE)"; \
	  cp $(OBJSX) "$(LIBDIR)/camlp4/."; \
	  for file in $(OBJSX); do \
	    cp "`echo $$file | sed -e 's/\.cmx$$/.$(O)/'`" "$(LIBDIR)/camlp4/."; \
	  done ; \
	fi
	cp mkcamlp4.sh "$(BINDIR)/mkcamlp4"
	chmod a+x "$(BINDIR)/mkcamlp4"

include .depend
