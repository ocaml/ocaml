BASEDIR=../..
CUSTOM_MODULE=odoc_test
ADD_COMPFLAGS=-I +ocamldoc

run: $(CUSTOM_MODULE).cmo
	@for file in t*.ml; do \
	  printf " ... testing '$$file'"; \
	  $(OCAMLDOC) -g $(CUSTOM_MODULE).cmo -o `basename $$file ml`result $$file; \
	  diff -q `basename $$file ml`reference `basename $$file ml`result > /dev/null && echo " => passed" || (echo " => failed" && exit 1); \
	done;
	@$(OCAMLDOC) -html t*.ml 2>&1 | grep -v test_types_display || true
	@$(OCAMLDOC) -latex t*.ml 2>&1 | grep -v test_types_display || true

promote: defaultpromote

clean: defaultclean
	@rm -f *.result *.html *.tex *.log *.out *.sty *.toc *.css *.aux

include $(BASEDIR)/makefiles/Makefile.common
