#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

# The main Makefile

include config/Makefile
include stdlib/StdlibModules

CAMLC=boot/ocamlrun boot/ocamlc -nostdlib -I boot
CAMLOPT=boot/ocamlrun ./ocamlopt -nostdlib -I stdlib -I otherlibs/dynlink
COMPFLAGS=-strict-sequence -warn-error A $(INCLUDES)
LINKFLAGS=

CAMLYACC=boot/ocamlyacc
YACCFLAGS=-v
CAMLLEX=boot/ocamlrun boot/ocamllex
CAMLDEP=boot/ocamlrun tools/ocamldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/ocamlrun
SHELL=/bin/sh
MKDIR=mkdir -p

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver \
	 -I toplevel

UTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/consistbl.cmo

OPTUTILS=$(UTILS)

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo

TYPING=typing/unused_var.cmo typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/env.cmo \
  typing/typedtree.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/includecore.cmo \
  typing/includemod.cmo typing/typetexp.cmo typing/parmatch.cmo \
  typing/stypes.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typeclass.cmo \
  typing/typemod.cmo

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/typeopt.cmo bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translobj.cmo bytecomp/translcore.cmo \
  bytecomp/translclass.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo

BYTECOMP=bytecomp/meta.cmo bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo bytecomp/symtable.cmo \
  bytecomp/bytelink.cmo bytecomp/bytelibrarian.cmo bytecomp/bytepackager.cmo

ASMCOMP=asmcomp/arch.cmo asmcomp/debuginfo.cmo \
  asmcomp/cmm.cmo asmcomp/printcmm.cmo \
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
  driver/main_args.cmo driver/optmain.cmo

TOPLEVEL=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo toplevel/genprintval.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo

TOPLEVELLIB=toplevel/toplevellib.cma
TOPLEVELSTART=toplevel/topstart.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)

TOPLIB=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(TOPLEVEL)

TOPOBJS=$(TOPLEVELLIB) $(TOPLEVELSTART)

NATTOPOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) \
  driver/pparse.cmo driver/opterrors.cmo driver/optcompile.cmo \
  driver/main_args.cmo \
  toplevel/genprintval.cmo toplevel/opttoploop.cmo toplevel/opttopdirs.cmo \
  toplevel/opttopmain.cmo toplevel/opttopstart.cmo

OPTOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) $(OPTDRIVER)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/path.cmo typing/types.cmo typing/btype.cmo \
  typing/predef.cmo bytecomp/runtimedef.cmo bytecomp/bytesections.cmo \
  bytecomp/dll.cmo bytecomp/meta.cmo bytecomp/symtable.cmo toplevel/expunge.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

# For users who don't read the INSTALL file
defaultentry:
	@echo "Please refer to the installation instructions in file INSTALL."
	@echo "If you've just unpacked the distribution, something like"
	@echo "	./configure"
	@echo "	make world"
	@echo "	make opt"
	@echo "	make install"
	@echo "should work.  But see the file INSTALL for more details."

# Recompile the system using the bootstrap compiler
all: runtime ocamlc ocamllex ocamlyacc ocamltools library ocaml \
  otherlibraries ocamlbuild.byte camlp4out $(DEBUGGER) ocamldoc

# Compile everything the first time
world:
	$(MAKE) coldstart
	$(MAKE) all

# Compile also native code compiler and libraries, fast
world.opt:
	$(MAKE) coldstart
	$(MAKE) opt.opt

# Hard bootstrap how-to:
# (only necessary in some cases, for example if you remove some primitive)
#
# make coreboot     [old system -- you were in a stable state]
# <change the source>
# make core         [cross-compiler]
# make partialclean [if you get "inconsistent assumptions"]
# <debug your changes>
# make core         [cross-compiler]
# make coreboot     [new system -- now you are in a stable state]

# Core bootstrapping cycle
coreboot:
# Save the original bootstrap compiler
	$(MAKE) backup
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# byterun/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on byterun/ocamlrun)
	$(MAKE) partialclean
	$(MAKE) ocamlc ocamllex ocamltools
# Rebuild the library (using byterun/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) promote
# Rebuild the core system
	$(MAKE) partialclean
	$(MAKE) core
# Check if fixpoint reached
	$(MAKE) compare

# Bootstrap and rebuild the whole system.
# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
bootstrap:
	$(MAKE) coreboot
	$(MAKE) all
	$(MAKE) compare

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

# Start up the system from the distribution compiler
coldstart:
	cd byterun; $(MAKE) all
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	cd yacc; $(MAKE) all
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	cd stdlib; $(MAKE) COMPILER=../boot/ocamlc all
	cd stdlib; cp $(LIBFILES) ../boot
	if test -f boot/libcamlrun.a; then :; else \
	  ln -s ../byterun/libcamlrun.a boot/libcamlrun.a; fi
	if test -d stdlib/caml; then :; else \
	  ln -s ../byterun stdlib/caml; fi

# Build the core system: the minimum needed to make depend and bootstrap
core: coldstart ocamlc ocamllex ocamlyacc ocamltools library

# Recompile the core system using the bootstrap compiler
coreall: ocamlc ocamllex ocamlyacc ocamltools library

# Save the current bootstrap compiler
MAXSAVED=boot/Saved/Saved.prev/Saved.prev/Saved.prev/Saved.prev/Saved.prev
backup:
	if test -d boot/Saved; then : ; else mkdir boot/Saved; fi
	if test -d $(MAXSAVED); then rm -r $(MAXSAVED); else : ; fi
	mv boot/Saved boot/Saved.prev
	mkdir boot/Saved
	mv boot/Saved.prev boot/Saved/Saved.prev
	cp boot/ocamlrun$(EXE) boot/Saved
	mv boot/ocamlc boot/ocamllex boot/ocamlyacc$(EXE) boot/ocamldep \
	   boot/Saved
	cd boot; cp $(LIBFILES) Saved

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
promote-cross:
	cp ocamlc boot/ocamlc
	cp lex/ocamllex boot/ocamllex
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	cp tools/ocamldep boot/ocamldep
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
promote: promote-cross
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Restore the saved bootstrap compiler if a problem arises
restore:
	mv boot/Saved/* boot
	rmdir boot/Saved
	mv boot/Saved.prev boot/Saved

# Check if fixpoint reached
compare:
	@if cmp boot/ocamlc ocamlc && cmp boot/ocamllex lex/ocamllex \
	    && cmp boot/ocamldep tools/ocamldep; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

# Remove old bootstrap compilers
cleanboot:
	rm -rf boot/Saved/Saved.prev/*

# Compile the native-code compiler
opt-core:
	$(MAKE) runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt

opt:
	$(MAKE) runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamlbuildlib.native

# Native-code versions of the tools
opt.opt: checkstack runtime core ocaml opt-core ocamlc.opt otherlibraries \
	 ocamlbuild.byte camlp4out $(DEBUGGER) ocamldoc ocamlopt.opt \
	 otherlibrariesopt \
	 ocamllex.opt ocamltoolsopt.opt ocamlbuild.native camlp4opt ocamldoc.opt

base.opt: checkstack runtime core ocaml opt-core ocamlc.opt otherlibraries \
	 ocamlbuild.byte camlp4out $(DEBUGGER) ocamldoc ocamlopt.opt \
	 otherlibrariesopt

# Installation
install:
	if test -d $(BINDIR); then : ; else $(MKDIR) $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else $(MKDIR) $(LIBDIR); fi
	if test -d $(STUBLIBDIR); then : ; else $(MKDIR) $(STUBLIBDIR); fi
	if test -d $(MANDIR)/man$(MANEXT); then : ; \
	  else $(MKDIR) $(MANDIR)/man$(MANEXT); fi
	cd $(LIBDIR); rm -f dllbigarray.so dlllabltk.so dllnums.so \
	  dllthreads.so dllunix.so dllgraphics.so dllmldbm.so dllstr.so \
	  dlltkanim.so
	cd byterun; $(MAKE) install
	cp ocamlc $(BINDIR)/ocamlc$(EXE)
	cp ocaml $(BINDIR)/ocaml$(EXE)
	cd stdlib; $(MAKE) install
	cp lex/ocamllex $(BINDIR)/ocamllex$(EXE)
	cp yacc/ocamlyacc$(EXE) $(BINDIR)/ocamlyacc$(EXE)
	cp toplevel/toplevellib.cma $(LIBDIR)/toplevellib.cma
	cp expunge $(LIBDIR)/expunge$(EXE)
	cp typing/outcometree.cmi typing/outcometree.mli $(LIBDIR)
	cp toplevel/topstart.cmo $(LIBDIR)
	cp toplevel/toploop.cmi toplevel/topdirs.cmi toplevel/topmain.cmi \
	   $(LIBDIR)
	cd tools; $(MAKE) install
	-cd man; $(MAKE) install
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) install) || exit $$?; \
	done
	cd ocamldoc; $(MAKE) install
	if test -f ocamlopt; then $(MAKE) installopt; else :; fi
	if test -f debugger/ocamldebug; then (cd debugger; $(MAKE) install); \
	   else :; fi
	cp config/Makefile $(LIBDIR)/Makefile.config
	BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) PREFIX=$(PREFIX) \
	  ./build/partial-install.sh

# Installation of the native-code compiler
installopt:
	cd asmrun; $(MAKE) install
	cp ocamlopt $(BINDIR)/ocamlopt$(EXE)
	cd stdlib; $(MAKE) installopt
	cd ocamldoc; $(MAKE) installopt
	for i in $(OTHERLIBRARIES); \
	  do (cd otherlibs/$$i; $(MAKE) installopt) || exit $$?; done
	if test -f ocamlc.opt; \
	  then cp ocamlc.opt $(BINDIR)/ocamlc.opt$(EXE); else :; fi
	if test -f ocamlopt.opt; \
	  then cp ocamlopt.opt $(BINDIR)/ocamlopt.opt$(EXE); else :; fi
	if test -f lex/ocamllex.opt; \
	  then cp lex/ocamllex.opt $(BINDIR)/ocamllex.opt$(EXE); else :; fi

clean:: partialclean

# The compiler

ocamlc: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlc $(COMPOBJS)
	@sed -e 's|@compiler@|$$topdir/boot/ocamlrun $$topdir/ocamlc|' \
	  driver/ocamlcomp.sh.in > ocamlcomp.sh
	@chmod +x ocamlcomp.sh

partialclean::
	rm -f ocamlc ocamlcomp.sh

# The native-code compiler

ocamlopt: $(OPTOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlopt $(OPTOBJS)
	@sed -e 's|@compiler@|$$topdir/boot/ocamlrun $$topdir/ocamlopt|' \
	  driver/ocamlcomp.sh.in > ocamlcompopt.sh
	@chmod +x ocamlcompopt.sh

partialclean::
	rm -f ocamlopt ocamlcompopt.sh

# The toplevel

ocaml: $(TOPOBJS) expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o ocaml.tmp $(TOPOBJS)
	- $(CAMLRUN) ./expunge ocaml.tmp ocaml $(PERVASIVES)
	rm -f ocaml.tmp

toplevel/toplevellib.cma: $(TOPLIB)
	$(CAMLC) -a -o $@ $(TOPLIB)

partialclean::
	rm -f ocaml toplevel/toplevellib.cma

# The native toplevel

ocamlnat: ocamlopt otherlibs/dynlink/dynlink.cmxa $(NATTOPOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) otherlibs/dynlink/dynlink.cmxa -o ocamlnat \
	           $(NATTOPOBJS:.cmo=.cmx) -linkall

toplevel/opttoploop.cmx: otherlibs/dynlink/dynlink.cmxa

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/natdynlink.ml
	cd otherlibs/dynlink && make allopt

# The configuration file

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
	    -e 's|%%BYTERUN%%|$(BINDIR)/ocamlrun|' \
	    -e 's|%%CCOMPTYPE%%|cc|' \
	    -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
	    -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
	    -e 's|%%PACKLD%%|$(PACKLD)|' \
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
	    -e 's|%%ASM%%|$(ASM)|' \
	    -e 's|%%MKDLL%%|$(MKDLL)|' \
	    -e 's|%%MKEXE%%|$(MKEXE)|' \
	    -e 's|%%MKMAINDLL%%|$(MKMAINDLL)|' \
	    utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

partialclean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The auxiliary lexer for counting line numbers

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll

partialclean::
	rm -f parsing/linenum.ml

beforedepend:: parsing/linenum.ml

# The bytecode compiler compiled with the native-code compiler

ocamlc.opt: $(COMPOBJS:.cmo=.cmx)
	cd asmrun; $(MAKE) meta.o dynlink.o
	$(CAMLOPT) $(LINKFLAGS) -ccopt "$(BYTECCLINKOPTS)" -o ocamlc.opt \
	  $(COMPOBJS:.cmo=.cmx) \
	  asmrun/meta.o asmrun/dynlink.o -cclib "$(BYTECCLIBS)"
	@sed -e 's|@compiler@|$$topdir/ocamlc.opt|' \
	  driver/ocamlcomp.sh.in > ocamlcomp.sh
	@chmod +x ocamlcomp.sh

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

ocamlopt.opt: $(OPTOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o ocamlopt.opt $(OPTOBJS:.cmo=.cmx)
	@sed -e 's|@compiler@|$$topdir/ocamlopt.opt|' \
	  driver/ocamlcomp.sh.in > ocamlcompopt.sh
	@chmod +x ocamlcompopt.sh

partialclean::
	rm -f ocamlopt.opt

$(OPTOBJS:.cmo=.cmx): ocamlopt

# The numeric opcodes

bytecomp/opcodes.ml: byterun/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
	awk -f tools/make-opcodes > bytecomp/opcodes.ml

partialclean::
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

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	ln -s $(ARCH)/arch.ml asmcomp/arch.ml

partialclean::
	rm -f asmcomp/arch.ml

beforedepend:: asmcomp/arch.ml

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	ln -s $(ARCH)/proc.ml asmcomp/proc.ml

partialclean::
	rm -f asmcomp/proc.ml

beforedepend:: asmcomp/proc.ml

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	ln -s $(ARCH)/selection.ml asmcomp/selection.ml

partialclean::
	rm -f asmcomp/selection.ml

beforedepend:: asmcomp/selection.ml

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	ln -s $(ARCH)/reload.ml asmcomp/reload.ml

partialclean::
	rm -f asmcomp/reload.ml

beforedepend:: asmcomp/reload.ml

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	ln -s $(ARCH)/scheduling.ml asmcomp/scheduling.ml

partialclean::
	rm -f asmcomp/scheduling.ml

beforedepend:: asmcomp/scheduling.ml

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/cvt_emit
	$(CAMLRUN) tools/cvt_emit < asmcomp/$(ARCH)/emit.mlp > asmcomp/emit.ml \
	|| { rm -f asmcomp/emit.ml; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

tools/cvt_emit: tools/cvt_emit.mll
	cd tools; \
	$(MAKE) CAMLC="../$(CAMLRUN) ../boot/ocamlc -I ../stdlib" cvt_emit

# The "expunge" utility

expunge: $(EXPUNGEOBJS)
	$(CAMLC) $(LINKFLAGS) -o expunge $(EXPUNGEOBJS)

partialclean::
	rm -f expunge

# The runtime system for the bytecode compiler

runtime:
	cd byterun; $(MAKE) all
	if test -f stdlib/libcamlrun.a; then :; else \
	  ln -s ../byterun/libcamlrun.a stdlib/libcamlrun.a; fi

clean::
	cd byterun; $(MAKE) clean
	rm -f stdlib/libcamlrun.a
	rm -f stdlib/caml

alldepend::
	cd byterun; $(MAKE) depend

# The runtime system for the native-code compiler

runtimeopt: makeruntimeopt
	cp asmrun/libasmrun.a stdlib/libasmrun.a

makeruntimeopt:
	cd asmrun; $(MAKE) all

clean::
	cd asmrun; $(MAKE) clean
	rm -f stdlib/libasmrun.a

alldepend::
	cd asmrun; $(MAKE) depend

# The library

library: ocamlc
	cd stdlib; $(MAKE) all

library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/ocamlrun all

libraryopt:
	cd stdlib; $(MAKE) allopt

partialclean::
	cd stdlib; $(MAKE) clean

alldepend::
	cd stdlib; $(MAKE) depend

# The lexer and parser generators

ocamllex: ocamlyacc ocamlc
	cd lex; $(MAKE) all

ocamllex.opt: ocamlopt
	cd lex; $(MAKE) allopt

partialclean::
	cd lex; $(MAKE) clean

alldepend::
	cd lex; $(MAKE) depend

ocamlyacc:
	cd yacc; $(MAKE) all

clean::
	cd yacc; $(MAKE) clean

# Tools

ocamltools: ocamlc ocamlyacc ocamllex asmcomp/cmx_format.cmi
	cd tools; $(MAKE) all

ocamltoolsopt.opt: ocamlc.opt ocamlyacc ocamllex asmcomp/cmx_format.cmi
	cd tools; $(MAKE) opt.opt

partialclean::
	cd tools; $(MAKE) clean

alldepend::
	cd tools; $(MAKE) depend

# OCamldoc

ocamldoc: ocamlc ocamlyacc ocamllex otherlibraries
	cd ocamldoc && $(MAKE) all

ocamldoc.opt: ocamlc.opt ocamlyacc ocamllex
	cd ocamldoc && $(MAKE) opt.opt

partialclean::
	cd ocamldoc && $(MAKE) clean

alldepend::
	cd ocamldoc && $(MAKE) depend

# The extra libraries

otherlibraries: ocamltools
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) RUNTIME=$(RUNTIME) all) || exit $$?; \
	done

otherlibrariesopt:
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) allopt) || exit $$?; \
	done

partialclean::
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) partialclean); \
	done

clean::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) clean); done

alldepend::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) depend); done

# The replay debugger

ocamldebugger: ocamlc ocamlyacc ocamllex otherlibraries
	cd debugger; $(MAKE) all

partialclean::
	cd debugger; $(MAKE) clean

alldepend::
	cd debugger; $(MAKE) depend

# Camlp4

camlp4out: ocamlc otherlibraries ocamlbuild-mixed-boot ocamlbuild.byte
	./build/camlp4-byte-only.sh

camlp4opt: ocamlopt otherlibrariesopt ocamlbuild-mixed-boot ocamlbuild.native
	./build/camlp4-native-only.sh

# Ocamlbuild

ocamlbuild.byte: ocamlc otherlibraries ocamlbuild-mixed-boot
	./build/ocamlbuild-byte-only.sh

ocamlbuild.native: ocamlopt otherlibrariesopt ocamlbuild-mixed-boot
	./build/ocamlbuild-native-only.sh
ocamlbuildlib.native: ocamlopt otherlibrariesopt ocamlbuild-mixed-boot
	./build/ocamlbuildlib-native-only.sh

ocamlbuild-mixed-boot: ocamlc otherlibraries
	./build/mixed-boot.sh

partialclean::
	rm -rf _build

# Check that the stack limit is reasonable.

checkstack:
	@if $(BYTECC) -o tools/checkstack tools/checkstack.c; \
	  then tools/checkstack; \
	  else :; \
	fi
	@rm -f tools/checkstack

# Make MacOS X package

package-macosx:
	sudo rm -rf package-macosx/root
	make PREFIX="`pwd`"/package-macosx/root install
	tools/make-package-macosx
	sudo rm -rf package-macosx/root

clean::
	rm -rf package-macosx/*.pkg package-macosx/*.dmg

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp driver toplevel tools; \
	  do rm -f $$d/*.cm[iox] $$d/*.annot $$d/*.[so] $$d/*~; done
	rm -f *~

depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp driver toplevel; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend

alldepend:: depend

distclean:
	./build/distclean.sh

.PHONY: all backup bootstrap camlp4opt camlp4out checkstack clean
.PHONY: partialclean beforedepend alldepend cleanboot coldstart
.PHONY: compare core coreall
.PHONY: coreboot defaultentry depend distclean install installopt
.PHONY: library library-cross libraryopt ocamlbuild-mixed-boot
.PHONY: ocamlbuild.byte ocamlbuild.native ocamldebugger ocamldoc
.PHONY: ocamldoc.opt ocamllex ocamllex.opt ocamltools ocamltools.opt
.PHONY: ocamlyacc opt-core opt opt.opt otherlibraries
.PHONY: otherlibrariesopt package-macosx promote promote-cross
.PHONY: restore runtime runtimeopt makeruntimeopt world world.opt

include .depend
