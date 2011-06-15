BASEDIR=../..
SHOULD_FAIL=t060-raise.ml

compile: lib.cmo
	@for file in t*.ml; do \
	  printf " ... testing '$$file'"; \
	  if [ `echo $(SHOULD_FAIL) | grep $$file` ]; then \
	    $(OCAML) -w a lib.cmo $$file 2> /dev/null && (echo " => failed" && exit 1) || echo " => passed"; \
	  else \
	    $(OCAML) -w a lib.cmo $$file 2> /dev/null && echo " => passed" || (echo " => failed" && exit 1); \
	  fi; \
	done

clean: defaultclean
	@rm -f ./a.out

include $(BASEDIR)/makefiles/Makefile.common
