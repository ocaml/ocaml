
default: subdir/m.ml a.ml b.ml main.ml
	@printf " ... testing 'main.ml'";
	@$(OCAMLC) -c subdir/m.ml;
	@$(OCAMLC) -c -I subdir a.ml;
	@$(OCAMLC) -c -I subdir b.ml;
	@$(OCAMLC) -c main.ml 2>&1 | cat > main.ml.result;
	@$(DIFF) main.ml.result main.ml.reference >/dev/null \
	&& echo " => passed" || echo " => failed"

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
