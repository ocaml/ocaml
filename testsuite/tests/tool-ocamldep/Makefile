#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                 Xavier Clerc, SED, INRIA Rocquencourt                 #
#                                                                       #
#   Copyright 2010 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../..
COMPFLAGS=-I $(OTOPDIR)/ocamldoc
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix:$(TOPDIR)/otherlibs/str
DOCFLAGS=-I $(OTOPDIR)/stdlib $(COMPFLAGS)

.PHONY: run
run:
	@for file in *.ml; do \
	  for option in "one-line" "native" "sort" "modules" "paths"; do \
	    printf " ... testing 'ocamldep -$$option $$file'"; \
	    F="`basename $$file .ml`"; \
	    $(OCAMLDEP) -$$option $$file > $$F.$$option.result; \
	    $(DIFF) $$F.$$option.reference $$F.$$option.result >/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  done; \
	done

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result

include $(BASEDIR)/makefiles/Makefile.common
