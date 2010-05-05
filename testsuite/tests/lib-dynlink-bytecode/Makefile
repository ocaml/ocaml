default: compile run

compile:
	@for file in stub*.c; do \
	  $(OCAMLC) -c $$file; \
	  $(OCAMLMKLIB) -o `echo $$file | sed -e 's/stub/plug/' | sed -e 's/\.c//'` `basename $$file c`o; \
	done
	@for file in plug*.ml; do \
	  $(OCAMLC) -c $$file; \
	  $(OCAMLMKLIB) -o `basename $$file .ml` `basename $$file ml`cmo; \
	done
	@$(OCAMLC) -c main.ml
	@$(OCAMLC) -o main dynlink.cma main.cmo
	@$(OCAMLC) -o static -linkall plug1.cma plug2.cma -use-runtime $(PREFIX)/bin/ocamlrun
	@$(OCAMLC) -o custom -custom -linkall plug2.cma plug1.cma -I .

run:
	@printf " ... testing 'main'"
	@export LD_LIBRARY_PATH=`pwd` && ./main plug1.cma plug2.cma > main.result
	@diff -q main.reference main.result > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

	@printf " ... testing 'static'"
	@export LD_LIBRARY_PATH=`pwd` && ./static > static.result
	@diff -q static.reference static.result > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

	@printf " ... testing 'custom'"
	@export LD_LIBRARY_PATH=`pwd` && ./custom > custom.result
	@diff -q custom.reference custom.result > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

clean: defaultclean
	@rm -f ./main ./static ./custom *.result

include ../../makefiles/Makefile.common
