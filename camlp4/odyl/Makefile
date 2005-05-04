# $Id$

include ../config/Makefile

SHELL=/bin/sh

INCLUDES=-I $(OTOP)/otherlibs/dynlink
OCAMLCFLAGS=-warn-error A $(INCLUDES)
LINKFLAGS=$(INCLUDES)

OBJS=odyl_config.cmo odyl_main.cmo

all: odyl$(EXE)

opt: opt$(PROFILING)

optnoprof: odyl.cmx odyl.cmxa

optprof: optnoprof odyl.p.cmx odyl.p.cmxa

odyl$(EXE): odyl.cma odyl.cmo
	$(OCAMLC) odyl.cma odyl.cmo -o $@

odyl.cma: $(OBJS)
	$(OCAMLC) $(LINKFLAGS) dynlink.cma $(OBJS) -a -o $@

odyl.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(LINKFLAGS) $(OBJS:.cmo=.cmx) -a -o $@

odyl.p.cmxa: $(OBJS:.cmo=.p.cmx)
	$(OCAMLOPT) $(LINKFLAGS) $(OBJS:.cmo=.p.cmx) -a -o $@

odyl_main.cmx: odyl_main.ml
	$(CAMLP4_COMM) -nolib -DOPT -o odyl_main.ppo odyl_main.ml
	$(OCAMLOPT) -c -impl odyl_main.ppo
	rm -f odyl_main.ppo

odyl_main.p.cmx: odyl_main.ml
	$(CAMLP4_COMM) -nolib -DOPT -o odyl_main.ppo odyl_main.ml
	$(OCAMLOPT) -p -c -o $@ -impl odyl_main.ppo
	rm -f odyl_main.ppo

odyl_config.ml:
	(echo 'let standard_library ='; \
	 echo '  try Sys.getenv "CAMLP4LIB" with Not_found -> '; \
	 echo '  try Sys.getenv "OCAMLLIB" ^ "/camlp4" with Not_found -> '; \
	 echo '  try Sys.getenv "CAMLLIB" ^ "/camlp4" with Not_found -> '; \
         echo '  "$(LIBDIR)/camlp4"') \
        | sed -e 's|\\|/|g' > odyl_config.ml

clean::
	rm -f *.cm* *.pp[io] *.$(O) *.bak .*.bak *.out *.opt *.$(A)
	rm -f odyl_config.ml odyl$(EXE)

depend:
	cp .depend .depend.bak
	> .depend
	@for i in *.mli *.ml; do \
	  ../tools/apply.sh pr_depend.cmo -- $(INCLUDES) $$i | \
	  sed -e 's| $(OTOP)/otherlibs/dynlink/dynlink.cmx||' | \
	  sed -e 's| \.\./\.\.| $$(OTOP)|g' >> .depend; \
	done

promote:

compare:

install:
	-$(MKDIR) "$(LIBDIR)/camlp4" "$(BINDIR)"
	cp odyl.cmo odyl.cma odyl_main.cmi $(LIBDIR)/camlp4/.
	for f in odyl.$(A) odyl.p.$(A)  ; do \
	   if test -f $$f ; then \
		cp $$f "$(LIBDIR)/camlp4/."  && ( cd "$(LIBDIR)/camlp4/." && $(RANLIB) $$f ) ; \
	   fi ; \
	done
	for f in odyl.cmx odyl.o odyl.p.cmx odyl.p.o odyl.cmxa odyl.p.cmxa ; do \
	    if test -f $$f ; then \
	       cp $$f "$(LIBDIR)/camlp4/." ; \
	    fi ; \
	done

include .depend
