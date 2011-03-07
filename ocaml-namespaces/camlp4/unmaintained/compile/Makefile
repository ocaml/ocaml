

include ../config/Makefile.cnf

INCLUDES=-I ../camlp4 -I ../lib
SRC=../etc/pa_o.ml ../etc/pa_op.ml # FIXME and why not pa_$D.ml?
D=o
COMP_OPT=-strict_parsing -e "Grammar.Entry.obj Pcaml.interf" -e "Grammar.Entry.obj Pcaml.implem" -e "Grammar.Entry.obj Pcaml.top_phrase" -e "Grammar.Entry.obj Pcaml.use_file"
CLEANFILES=pa_*_fast.ml *_fast.ml

# FIXME
EXECUTABLES=#camlp4$D.fast

include ../config/Makefile.base

WARNINGS=Ay

camlp4$D.fast: pa_$D_fast.cmo
	rm -f camlp4$D.fast
	cd ../camlp4; $(MAKE) CAMLP4=../compile/camlp4$D.fast CAMLP4M="../compile/pa_$D_fast.cmo ../meta/pr_dump.cmo"

camlp4$D.fast.opt: pa_$D_fast.cmx
	rm -f camlp4$D.fast.opt
	cd ../camlp4; $(MAKE) ../compile/camlp4$D.fast.opt CAMLP4OPT=../compile/camlp4$D.fast.opt CAMLP4M="../compile/pa_$D_fast.cmx ../meta/pr_dump.cmx"

pa_$D_fast.ml: comp_head.ml $D_fast.ml comp_trail.ml
	cat $(SRC) | sed -e "s/Plexer.make_lexer *()/P.lexer_pos/" -e "/EXTEND/,/END/d" -e "/Grammar.Entry.of_parser/d" -e "/Grammar.Entry.gcreate/d" | cat comp_head.ml - $D_fast.ml comp_trail.ml > pa_$D_fast.ml

$D_fast.ml: compile.cmo $(SRC)
	echo '(* camlp4r *)' >$D_fast.ml
	OTOP=$(OTOP) EXE=$(EXE) ./compile.sh $(COMP_OPT) $(SRC) >> $D_fast.ml

install-local:
	if test -f camlp4$D.fast.opt; then cp camlp4$D.fast.opt $(BINDIR)/camlp4$D.opt$(EXE); fi
	for TARG in  pa_$D_fast.cmi  pa_$D_fast.cmo pa_$D_fast.cmx ; do if test -f $$TARG; then cp $$TARG "$(LIBDIR)/camlp4/."; fi; done

include .depend
