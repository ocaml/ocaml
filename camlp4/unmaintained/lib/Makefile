

include ../config/Makefile.cnf

INCLUDES=-I $(OTOP)/parsing -I $(OTOP)/utils
LIBRARIES=gramlib.cma
OBJS=$(OTOP)/utils/misc.cmo $(OTOP)/parsing/linenum.cmo		\
     $(OTOP)/utils/warnings.cmo $(OTOP)/parsing/location.cmo	\
     $(OTOP)/utils/config.cmo debug.cmo loc.cmo			\
     token.cmo lexer_token.cmo lexer_error.cmo			\
     plexer.cmo grammar.cmo extfun.cmo				\
     fstream.cmo
CLEANFILES=plexer.ml

include ../config/Makefile.base

debug.cmo: debug.cmi
debug.cmo: debug.ml
	$(OCAMLC) -pp '$(CAMLP4BOOT)' -rectypes $(OCAMLCFLAGS) $< -c -o $@

plexer.cmo: plexer.ml plexer.cmi
	$(OCAMLC) $(OCAMLCFLAGS) $< -c -o $@

plexer.cmx: plexer.ml plexer.cmi
	$(OCAMLOPT) $(OCAMLCFLAGS) $< -c -o $@

$(LIBRARIES): $(OBJS)
	$(OCAMLC) -linkall $(OBJS) -a -o $(LIBRARIES)

$(LIBRARIESX): $(OBJSX)
	$(OCAMLOPT) -linkall $(OBJSX) -a -o $(LIBRARIESX)

$(LIBRARIESP): $(OBJSP)
	$(OCAMLOPT) -linkall $(OBJSP) -a -o $(LIBRARIESP)

install-local:
	-$(MKDIR) "$(LIBDIR)/camlp4"
	cp $(LIBRARIES) *.mli "$(LIBDIR)/camlp4/."
	cp *.cmi "$(LIBDIR)/camlp4/."
	test -f $(LIBRARIESX) && $(MAKE) installopt LIBDIR="$(LIBDIR)" || true

installopt:
	for f in $(LIBRARIESX) $(LIBRARIESP) *.cmx ; do \
		test -f $$f && cp $$f "$(LIBDIR)/camlp4/." || true ; \
	done
	# Special treatment for this one: some versions of make don't like $(A) in $(TARGET:.cma=.$(A)) 
	target="`echo $(LIBRARIES) | sed -e 's/\.cma$$/.$(A)/'`" ; \
	   if test -f $$target ; then \
	      cp $$target "$(LIBDIR)/camlp4/." && ( cd "$(LIBDIR)/camlp4/." && $(RANLIB) $$target ) \
	  fi

include .depend
