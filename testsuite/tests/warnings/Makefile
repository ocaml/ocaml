BASEDIR=../..
FLAGS=-w A
EXECNAME=./program

run-all:
	@for file in *.ml; do \
	  printf " ... testing '$$file':"; \
	  $(OCAMLC) $(FLAGS) -o $(EXECNAME) $$file 2> `basename $$file ml`result; \
	  diff -q `basename $$file ml`reference `basename $$file ml`result > /dev/null && echo " => passed" || echo " => failed"; \
	done;

promote: defaultpromote

clean: defaultclean
	@rm -f *.result $(EXECNAME)

include $(BASEDIR)/makefiles/Makefile.common
