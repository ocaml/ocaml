BASEDIR=../..

compile:
	printf " ... testing -compat-32"
	@$(OCAMLC) -compat-32 -c a.ml 2>&1 | xargs echo ${} > test.result  
	@$(OCAMLC) -c a.ml
	@$(OCAMLC) -compat-32 -a a.cmo -o a.cma 2>&1 | xargs echo ${} >> test.result 
	@$(OCAMLC) -a a.cmo -o a.cma
	@$(OCAMLC) -compat-32    a.cma -o a.byte -linkall 2>&1 | xargs echo ${} >> test.result
	@$(DIFF) test.reference test.result >/dev/null \
	&& echo " => passed" || echo " => failed"; \

promote: defaultpromote

clean: defaultclean

include $(BASEDIR)/makefiles/Makefile.common
