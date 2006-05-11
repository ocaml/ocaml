# Build the OCamlDuce tools using an existing OCaml installation

VERSION=3.09.2pl1

OCAMLDUCELIBDIR=$(shell ocamlfind printconf destdir)/ocamlduce

all: ocamlducec ocamlduce ocamlducedep ocamlducedoc cduce/ocamlduce.cma ocamlducemktop
opt: all ocamlduceopt ocamlducec.opt ocamlduceopt.opt ocamlducedep.opt ocamlducedoc.opt cduce/ocamlduce.cmxa


# The OCaml tools to use

CAMLC=ocamlc.opt
CAMLOPT=ocamlopt.opt
CAMLYACC=ocamlyacc
YACCFLAGS=-v
CAMLLEX=ocamllex
CAMLDEP=ocamldep.opt
DEPFLAGS=$(INCLUDES)

OCAML_CONFIG=$(shell $(CAMLC) -where)/Makefile.config

include $(OCAML_CONFIG)
include stdlib/StdlibModules

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver \
         -I toplevel -I tools

COMPFLAGS=-warn-error A $(INCLUDES)
LINKFLAGS=

UTILS=cduce_types.cmo utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/consistbl.cmo

OPTUTILS=$(UTILS)

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/xparser.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo

TYPING=typing/unused_var.cmo typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/env.cmo \
  typing/typedtree.cmo typing/ctype.cmo  \
  typing/printtyp.cmo typing/typeext.cmo \
  typing/includeclass.cmo \
  typing/mtype.cmo typing/includecore.cmo \
  typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/stypes.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typeclass.cmo \
  typing/typemod.cmo

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/typeopt.cmo bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translobj.cmo bytecomp/translext.cmo bytecomp/translcore.cmo \
  bytecomp/translclass.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo

BYTECOMP=bytecomp/meta.cmo bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo bytecomp/symtable.cmo \
  bytecomp/bytelink.cmo bytecomp/bytelibrarian.cmo bytecomp/bytepackager.cmo

ASMCOMP=asmcomp/arch.cmo asmcomp/cmm.cmo asmcomp/printcmm.cmo \
  asmcomp/reg.cmo asmcomp/mach.cmo asmcomp/proc.cmo \
  asmcomp/clambda.cmo asmcomp/compilenv.cmo \
  asmcomp/closure.cmo asmcomp/cmmgen.cmo \
  asmcomp/printmach.cmo asmcomp/selectgen.cmo asmcomp/selection.cmo \
  asmcomp/comballoc.cmo asmcomp/liveness.cmo \
  asmcomp/spill.cmo asmcomp/split.cmo \
  asmcomp/interf.cmo asmcomp/coloring.cmo \
  asmcomp/reloadgen.cmo asmcomp/reload.cmo \
  asmcomp/printlinear.cmo asmcomp/linearize.cmo \
  asmcomp/schedgen.cmo asmcomp/scheduling.cmo \
  asmcomp/emitaux.cmo asmcomp/emit.cmo asmcomp/asmgen.cmo \
  asmcomp/asmlink.cmo asmcomp/asmlibrarian.cmo asmcomp/asmpackager.cmo

DRIVER=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo driver/main.cmo

OPTDRIVER= driver/pparse.cmo driver/opterrors.cmo driver/optcompile.cmo \
  driver/optmain.cmo

TOPLEVEL=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  toplevel/genprintval.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo

TOPLEVELLIB=toplevel/toplevelducelib.cma
TOPLEVELSTART=toplevel/topstart.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)

TOPLIB=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(TOPLEVEL) cduce/ocamlduce.cma

TOPOBJS=$(TOPLEVELLIB) $(TOPLEVELSTART)

OPTOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) $(OPTDRIVER)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/path.cmo typing/types.cmo typing/btype.cmo \
  typing/predef.cmo bytecomp/runtimedef.cmo bytecomp/bytesections.cmo \
  bytecomp/dll.cmo bytecomp/meta.cmo bytecomp/symtable.cmo toplevel/expunge.cmo

CDUCE=\
  cduce/src/custom.cmo cduce/src/encodings.cmo \
  cduce/src/imap.cmo cduce/src/upool.cmo \
  cduce/src/ns.cmo cduce/src/sortedList.cmo cduce/src/atoms.cmo \
  cduce/src/bool.cmo cduce/src/chars.cmo cduce/src/ident.cmo \
  cduce/src/intervals.cmo cduce/src/inttbl.cmo cduce/src/normal.cmo \
  cduce/src/pretty.cmo cduce/src/stats.cmo \
  cduce/src/compunit.cmo \
  cduce/src/types.cmo cduce/src/sequence.cmo cduce/src/sample.cmo \
  cduce/src/auto_pat.cmo cduce/src/patterns.cmo \
  cduce/src/value.cmo cduce/src/run_dispatch.cmo cduce/src/explain.cmo \
  cduce/src/typepat.cmo \
  cduce/src/serial.cmo

# The compiler

ocamlducec: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlducec nums.cma $(COMPOBJS)

clean::
	rm -f ocamlducec

# The native-code compiler

ocamlduceopt: $(OPTOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlduceopt nums.cma $(OPTOBJS)

clean::
	rm -f ocamlduceopt

# The toplevel

ocamlduce: $(TOPOBJS) expungeduce
	sed -e "s|%%OCAMLC_DIR%%|$(BINDIR)|" \
	    -e "s|%%EXPUNGEDUCE_DIR%%|.|" \
	    -e "s|%%LIBDIR%%|toplevel/|" \
	    tools/ocamlducemktop.tpl > mk_top
	chmod +x mk_top
	./mk_top -o ocamlduce
	rm mk_top

toplevel/toplevelducelib.cma: $(TOPLIB)
	$(CAMLC) -a -o $@ $(TOPLIB)

ocamlducemktop: tools/ocamlmktop.tpl $(TOPOBJS) expungeduce
	sed -e "s|%%OCAMLC_DIR%%|$(BINDIR)|" \
	    -e "s|%%EXPUNGEDUCE_DIR%%|$(BINDIR)|" \
	    -e "s|%%LIBDIR%%|$(OCAMLDUCELIBDIR)/|" \
	    tools/ocamlducemktop.tpl > ocamlducemktop
	chmod +x ocamlducemktop

clean::
	rm -f ocamlduce toplevel/toplevelducelib.cma ocamlducemktop

# The configuration file

utils/config.ml: utils/config.mlp
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
	    -e 's|%%OCAMLDUCELIBDIR%%|$(OCAMLDUCELIBDIR)|' \
	    -e 's|%%VERSION%%|$(VERSION)|' \
            -e 's|%%BYTERUN%%|$(BINDIR)/gcamlrun|' \
            -e 's|%%CCOMPTYPE%%|cc|' \
            -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
            -e 's|%%BYTELINK%%|$(BYTECC) $(BYTECCLINKOPTS)|' \
            -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
            -e 's|%%NATIVELINK%%|$(NATIVECC) $(NATIVECCLINKOPTS)|' \
            -e 's|%%PARTIALLD%%|ld -r $(NATIVECCLINKOPTS)|' \
            -e 's|%%PACKLD%%|ld -r $(NATIVECCLINKOPTS)|' \
            -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
            -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
            -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
            -e 's|%%CC_PROFILE%%|$(CC_PROFILE)|' \
            -e 's|%%ARCH%%|$(ARCH)|' \
            -e 's|%%MODEL%%|$(MODEL)|' \
            -e 's|%%SYSTEM%%|$(SYSTEM)|' \
            -e 's|%%EXT_OBJ%%|.o|' \
            -e 's|%%EXT_ASM%%|.s|' \
            -e 's|%%EXT_LIB%%|.a|' \
            -e 's|%%EXT_DLL%%|.so|' \
            -e 's|%%SYSTHREAD_SUPPORT%%|$(SYSTHREAD_SUPPORT)|' \
            utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

clean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly

clean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

clean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The auxiliary lexer for counting line numbers

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll

clean::
	rm -f parsing/linenum.ml

beforedepend:: parsing/linenum.ml

# The bytecode compiler compiled with the native-code compiler

ocamlducec.opt: $(COMPOBJS:.cmo=.cmx)
	cd asmrun; $(MAKE) meta.o dynlink.o
	$(CAMLOPT) $(LINKFLAGS) -ccopt "$(BYTECCLINKOPTS)" -o ocamlducec.opt \
          nums.cmxa $(COMPOBJS:.cmo=.cmx) \
          asmrun/meta.o asmrun/dynlink.o -cclib "$(BYTECCLIBS)"

clean::
	rm -f ocamlducec.opt

# The native-code compiler compiled with itself

ocamlduceopt.opt: $(OPTOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o ocamlduceopt.opt nums.cmxa $(OPTOBJS:.cmo=.cmx)

clean::
	rm -f ocamlduceopt.opt

# The numeric opcodes

bytecomp/opcodes.ml: byterun/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
        awk -f tools/make-opcodes > bytecomp/opcodes.ml

clean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# The predefined exceptions and primitives

byterun/primitives:
	cd byterun; $(MAKE) primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' byterun/fail.h | \
	 sed -e '$$s/;$$//'; \
         echo '|]'; \
         echo 'let builtin_primitives = [|'; \
         sed -e 's/.*/  "&";/' -e '$$s/;$$//' byterun/primitives; \
	 echo '|]') > bytecomp/runtimedef.ml

clean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	ln -s $(ARCH)/arch.ml asmcomp/arch.ml

clean::
	rm -f asmcomp/arch.ml

beforedepend:: asmcomp/arch.ml

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	ln -s $(ARCH)/proc.ml asmcomp/proc.ml

clean::
	rm -f asmcomp/proc.ml

beforedepend:: asmcomp/proc.ml

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	ln -s $(ARCH)/selection.ml asmcomp/selection.ml

clean::
	rm -f asmcomp/selection.ml

beforedepend:: asmcomp/selection.ml

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	ln -s $(ARCH)/reload.ml asmcomp/reload.ml

clean::
	rm -f asmcomp/reload.ml

beforedepend:: asmcomp/reload.ml

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	ln -s $(ARCH)/scheduling.ml asmcomp/scheduling.ml

clean::
	rm -f asmcomp/scheduling.ml

beforedepend:: asmcomp/scheduling.ml

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/cvt_emit
	tools/cvt_emit < asmcomp/$(ARCH)/emit.mlp > asmcomp/emit.ml \
        || { rm -f asmcomp/emit.ml; exit 2; }

clean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

tools/cvt_emit: tools/cvt_emit.mll
	cd tools; $(MAKE) CAMLC="$(CAMLC)" CAMLLEX="$(CAMLLEX)" cvt_emit

# The "expunge" utility

expungeduce: $(EXPUNGEOBJS)
	$(CAMLC) $(LINKFLAGS) -o expungeduce $(EXPUNGEOBJS)

clean::
	rm -f expungeduce

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

clean::
	rm -f utils/*.cm[iox] utils/*.[so] utils/*~
	rm -f parsing/*.cm[iox] parsing/*.[so] parsing/*~
	rm -f typing/*.cm[iox] typing/*.[so] typing/*~
	rm -f bytecomp/*.cm[iox] bytecomp/*.[so] bytecomp/*~
	rm -f asmcomp/*.cm[iox] asmcomp/*.[so] asmcomp/*~
	rm -f driver/*.cm[iox] driver/*.[so] driver/*~
	rm -f toplevel/*.cm[iox] toplevel/*.[so] toplevel/*~
	rm -f tools/*.cm[iox] tools/*.[so] tools/*~
	rm -f cduce/*.cm[aiox] cduce/*.[so] cduce/*.a cduce/*.cmxa cduce/*~
	rm -f cduce/src/*.cm[iox] cduce/src/*.[so] cduce/src/*~
	rm -f *~
	(cd asmrun; make clean)
	(cd tools; make clean)
	rm -f tools/*.bak

depend: beforedepend
	touch cduce_types.mli
	touch cduce_types.ml
	(for d in utils parsing typing bytecomp asmcomp driver toplevel; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend
	$(CAMLDEP) -I cduce/src cduce/src/*.ml* >> .depend

FORCE:

include .depend

# The pack'ed modules from CDuce

cduce_types.cmi cduce_types.cmo:
	$(MAKE) INCLUDES="-I cduce/src" \
           CAMLC="$(CAMLC) -for-pack Cduce_types" \
           $(CDUCE)
	rm -f cduce_types.mli cduce_types.cmi
	$(CAMLC) -pack -o cduce_types.cmo -I cduce/src $(CDUCE)

cduce_types.cmx:
	$(MAKE) INCLUDES="-I cduce/src" \
           CAMLC="$(CAMLC) -for-pack Cduce_types" \
           CAMLOPT="$(CAMLOPT) -for-pack Cduce_types" \
           $(CDUCE:.cmo=.cmx)
	rm -f cduce_types.mli cduce_types.cmi
	$(CAMLOPT) -pack -o cduce_types.cmx -I cduce/src $(CDUCE:.cmo=.cmx)

clean::
	rm -f cduce_types.*

# The OCamlDuce library

cduce/ocamlduce.cma: ocamlducec cduce_types.cmo cduce/ocamlduce.cmo
	./ocamlducec -a -o cduce/ocamlduce.cma cduce_types.cmo cduce/ocamlduce.cmo

cduce/ocamlduce.cmo: ocamlducec cduce/ocamlduce.ml cduce/ocamlduce.cmi
	./ocamlducec -c -I cduce cduce/ocamlduce.ml

cduce/ocamlduce.cmi: ocamlducec cduce/ocamlduce.mli
	./ocamlducec -c cduce/ocamlduce.mli

cduce/ocamlduce.cmxa: ocamlduceopt cduce_types.cmx cduce/ocamlduce.cmx
	./ocamlduceopt -a -o cduce/ocamlduce.cmxa cduce_types.cmx cduce/ocamlduce.cmx

cduce/ocamlduce.cmx: ocamlduceopt cduce/ocamlduce.ml cduce/ocamlduce.cmi
	./ocamlduceopt -c -I cduce cduce/ocamlduce.ml

clean::
	rm -f cduce/ocamlduce.cm* cduce/*.o cduce/*.a

# The dependency generator

CAMLDEP_OBJ=tools/depend.cmo tools/ocamldep.cmo
CAMLDEP_IMPORTS=cduce_types.cmo \
  utils/misc.cmo utils/config.cmo utils/clflags.cmo \
  utils/terminfo.cmo \
  parsing/linenum.cmo utils/warnings.cmo parsing/location.cmo \
  parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/xparser.cmo \
  parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo

ocamlducedep: tools/depend.cmi $(CAMLDEP_OBJ)
	$(CAMLC) $(LINKFLAGS) -o ocamlducedep nums.cma $(CAMLDEP_IMPORTS) $(CAMLDEP_OBJ)

ocamlducedep.opt: tools/depend.cmi $(CAMLDEP_OBJ:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o ocamlducedep.opt nums.cmxa \
		   $(CAMLDEP_IMPORTS:.cmo=.cmx) \
	           $(CAMLDEP_OBJ:.cmo=.cmx)


clean::
	rm -f ocamlducedep*


# OCamldoc

ocamlducedoc: ocamlducedep
	cd ocamldoc && $(MAKE) OCAMLC=$(CAMLC) OCAMLOPT=$(CAMLOPT) OCAMLLEX=$(CAMLLEX) OCAMLYACC=$(CAMLYACC) LINKFLAGS="nums.cma ../cduce_types.cmo" ocamldoc
	cp ocamldoc/ocamldoc ./ocamlducedoc

ocamlducedoc.opt: ocamlducedep.opt
	cd ocamldoc && $(MAKE) OCAMLC=$(CAMLC) OCAMLOPT=$(CAMLOPT) OCAMLLEX=$(CAMLLEX) OCAMLYACC=$(CAMLYACC) LINKFLAGS="nums.cmxa ../cduce_types.cmx" ocamldoc.opt
	cp ocamldoc/ocamldoc.opt ./ocamlducedoc.opt


clean::
	rm -f ocamlducedoc*
	cd ocamldoc && $(MAKE) clean

# HTML documentation

htdoc: cduce/ocamlduce.cmi ocamlducedoc
	mkdir -p htdoc
	./ocamlducedoc -html -d htdoc cduce/ocamlduce.mli

clean::
	rm -Rf htdoc


# Findlib installation

INSTALL_LIB_FILES= \
 cduce/ocamlduce.cma cduce/ocamlduce.cmi cduce_types.cmi cduce/ocamlduce.mli \
 cduce/ocamlduce.cmxa cduce/ocamlduce.o cduce_types.o cduce/ocamlduce.a \
 $(TOPOBJS)  

INSTALL_BINARIES= \
 ocamlducec ocamlduce ocamlducedep ocamlducedoc \
 ocamlduceopt \
 ocamlducemktop expungeduce ocamlducefind

OPT_VARIANTS= \
 ocamlducec ocamlduceopt ocamlducedep ocamlducedoc

INSTALL_DOC_FILES= \
 README LICENSE htdoc

install: FORCE META
	for i in $(INSTALL_BINARIES); do \
	  cp $$i $(BINDIR)/; \
	done
	for i in $(OPT_VARIANTS); do \
	  cp $$i.opt $(BINDIR)/$$i; \
	done
	ocamlfind install ocamlduce META -optional $(INSTALL_LIB_FILES)

uninstall: FORCE
	for i in $(INSTALL_BINARIES); do \
	  rm $(BINDIR)/$$i; \
	done
	ocamlfind remove ocamlduce

META: META.in
	sed "s/%VER%/$(VERSION)/" META.in > META

clean::
	rm -f META

# Build package

PACKAGE_FILES= \
  Makefile .depend META.in ocamlducefind \
  asmcomp asmrun bytecomp byterun cduce driver ocamldoc parsing \
  toplevel typing utils tools


package: clean
	(cd cduce; make copy)
	rm -Rf ocamlduce-$(VERSION)
	mkdir ocamlduce-$(VERSION)
	cp -aR $(PACKAGE_FILES) ocamlduce-$(VERSION)/
	(cd ocamlduce-$(VERSION);  \
	mkdir config; \
	mkdir stdlib; \
	cp ../stdlib/StdlibModules stdlib/; \
	cp ../README.cduce README; \
	cp ../LICENSE.cduce LICENSE)
	tar czf ocamlduce-$(VERSION).tar.gz \
          --exclude CVS --exclude ".#*" ocamlduce-$(VERSION)
	rm -Rf ocamlduce-$(VERSION)
