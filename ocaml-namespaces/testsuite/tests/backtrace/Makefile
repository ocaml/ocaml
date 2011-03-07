EXECNAME=./program

run-all:
	@for file in *.ml; do \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  for arg in a b c d ''; do \
	    printf " ... testing '$$file' (with argument '$$arg'):"; \
	    OCAMLRUNPARAM=b=1 $(EXECNAME) $$arg > `basename $$file ml`$$arg.result 2>&1; \
	    diff -q `basename $$file ml`$$arg.reference `basename $$file ml`$$arg.result > /dev/null && echo " => passed" || (echo " => failed" && exit 1); \
	  done; \
	done

clean: defaultclean
	@rm -f *.result $(EXECNAME)

include ../../makefiles/Makefile.common
