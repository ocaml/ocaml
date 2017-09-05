# Check ocamlc -i

SOURCES = pr7620_bad.ml

all: 
	@printf " ... testing '$(SOURCES)'"
	@$(OCAMLC) $(ADD_COMPFLAGS) -i 2>&1 > /dev/null \
	 && echo " => failed" || echo " => passed"

clean: defaultclean
	@rm -f *~

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common

