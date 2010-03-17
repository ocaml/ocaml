CUSTOM_MODULE=odoc_test
ADD_COMPFLAGS=-I +ocamldoc

run: $(CUSTOM_MODULE).cmo
	@for file in t*.ml; do \
	  printf " ... testing '$$file'"; \
	  $(OCAMLDOC) -g $(CUSTOM_MODULE).cmo -o `basename $$file ml`result $$file; \
	  diff -q `basename $$file ml`reference `basename $$file ml`result > /dev/null && echo " => passed" || (echo " => failed" && exit 1); \
	done

clean: defaultclean
	@rm -f *.result

include ../../makefiles/Makefile.common
