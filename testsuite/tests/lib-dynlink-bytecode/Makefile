BASEDIR=../..

default: compile run

compile:
	@$(OCAMLC) -c registry.ml
	@for file in stub*.c; do \
	  $(OCAMLC) -c $$file; \
	  $(OCAMLMKLIB) -o `echo $$file | sed -e 's/stub/plug/' | sed -e 's/\.c//'` `basename $$file c`o; \
	done
	@for file in plug*.ml; do \
	  $(OCAMLC) -c $$file; \
	  $(OCAMLMKLIB) -o `basename $$file .ml` `basename $$file ml`cmo; \
	done
	@$(OCAMLC) -c main.ml
	@$(OCAMLC) -o main dynlink.cma registry.cmo main.cmo
	@$(OCAMLC) -o static -linkall registry.cmo plug1.cma plug2.cma -use-runtime $(PREFIX)/bin/ocamlrun
	@$(OCAMLC) -o custom -custom -linkall registry.cmo plug2.cma plug1.cma -I .

run:
	@printf " ... testing 'main'"
	@export LD_LIBRARY_PATH=`pwd` && ./main plug1.cma plug2.cma > main.result
	@$(DIFF) main.reference main.result > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

	@printf " ... testing 'static'"
	@export LD_LIBRARY_PATH=`pwd` && ./static > static.result
	@$(DIFF) static.reference static.result > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

	@printf " ... testing 'custom'"
	@export LD_LIBRARY_PATH=`pwd` && ./custom > custom.result
	@$(DIFF) custom.reference custom.result > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

promote: defaultpromote

clean: defaultclean
	@rm -f ./main ./static ./custom *.result marshal.data

include $(BASEDIR)/makefiles/Makefile.common
