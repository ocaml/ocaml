# $Id$

include ../config/Makefile.cnf

EXECUTABLES=odyl
INCLUDES=-I $(OTOP)/otherlibs/dynlink
OBJS=odyl_config.cmo odyl_main.cmo
OBJSX=odyl.cmx odyl.cmxa
CLEANFILES=odyl_config.ml

include ../config/Makefile.base

odyl$(EXE): odyl.cma odyl.cmo
	$(OCAMLC) odyl.cma odyl.cmo -o $@

odyl.opt: odyl.cmxa odyl.cmx
	$(OCAMLOPT) odyl.cmxa odyl.cmx -o $@

odyl.cma: $(OBJS)
	$(OCAMLC) $(LINKFLAGS) dynlink.cma $(OBJS) -a -o $@

odyl.cmxa: $(OBJSX)
	$(OCAMLOPT) $(LINKFLAGS) $(OBJSX) -a -o $@

odyl.p.cmxa: $(OBJSP)
	$(OCAMLOPT) $(LINKFLAGS) $(OBJSP) -a -o $@

odyl_main.cmx: odyl_main.ml
	$(CAMLP4BOOT) -nolib -DOPT -o odyl_main.ppo odyl_main.ml
	$(OCAMLOPT) -c -impl odyl_main.ppo
	rm -f odyl_main.ppo

odyl_main.p.cmx: odyl_main.ml
	$(CAMLP4BOOT) -nolib -DOPT -o odyl_main.ppo odyl_main.ml
	$(OCAMLOPT) -p -c -o $@ -impl odyl_main.ppo
	rm -f odyl_main.ppo

odyl_config.ml:
	(echo '(* camlp4r *)'; \
	 echo 'value standard_library ='; \
	 echo '  try Sys.getenv "CAMLP4LIB" with [ Not_found -> '; \
	 echo '  try Sys.getenv "OCAMLLIB" ^ "/camlp4" with [ Not_found -> '; \
	 echo '  try Sys.getenv "CAMLLIB" ^ "/camlp4" with [ Not_found -> '; \
         echo '  "$(LIBDIR)/camlp4"]]];') \
        | sed -e 's|\\|/|g' > odyl_config.ml

install-local:
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
