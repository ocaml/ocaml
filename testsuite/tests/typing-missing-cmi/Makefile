
.PHONY: default
default: subdir/m.ml a.ml b.ml main.ml
	@printf " ... testing 'main.ml'";
	@$(OCAMLC) -c subdir/m.ml;
	@$(OCAMLC) -c -I subdir a.ml;
	@$(OCAMLC) -c -I subdir b.ml;
	@$(OCAMLC) -c main.ml > main.ml.result 2>&1 || :
	@$(DIFF) main.ml.result main.ml.reference >/dev/null \
	&& echo " => passed" || echo " => failed"

.PHONY: clean
clean:
	@rm -f subdir/m.cm[io] *.cm[io] main.ml.result

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
