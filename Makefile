# The main Makefile

include config/Makefile.h
include Makefile.config

CAMLC=boot/camlrun boot/camlc -I boot
CAMLOPT=boot/camlrun ./camlopt -I stdlib
COMPFLAGS=$(INCLUDES)
LINKFLAGS=
CAMLYACC=boot/camlyacc
YACCFLAGS=
CAMLLEX=boot/camlrun boot/camllex
CAMLDEP=boot/camlrun tools/camldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/camlrun

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver -I toplevel

UTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/digest.cmo

PARSING=parsing/location.cmo parsing/longident.cmo \
  parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo

TYPING=typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/typedtree.cmo \
  typing/subst.cmo typing/printtyp.cmo \
  typing/predef.cmo typing/datarepr.cmo typing/env.cmo \
  typing/ctype.cmo typing/mtype.cmo \
  typing/includecore.cmo typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typemod.cmo

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/dectree.cmo bytecomp/matching.cmo \
  bytecomp/translcore.cmo bytecomp/translmod.cmo \
  bytecomp/runtimedef.cmo

BYTECOMP=bytecomp/meta.cmo bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/symtable.cmo bytecomp/bytelibrarian.cmo bytecomp/bytelink.cmo

ASMCOMP=asmcomp/arch.cmo asmcomp/cmm.cmo asmcomp/printcmm.cmo \
  asmcomp/reg.cmo asmcomp/mach.cmo asmcomp/proc.cmo \
  asmcomp/clambda.cmo asmcomp/compilenv.cmo \
  asmcomp/closure.cmo asmcomp/cmmgen.cmo \
  asmcomp/printmach.cmo asmcomp/selection.cmo asmcomp/liveness.cmo \
  asmcomp/spill.cmo asmcomp/split.cmo \
  asmcomp/interf.cmo asmcomp/coloring.cmo asmcomp/reload.cmo \
  asmcomp/printlinear.cmo asmcomp/linearize.cmo asmcomp/scheduling.cmo \
  asmcomp/emitaux.cmo asmcomp/emit.cmo asmcomp/asmgen.cmo \
  asmcomp/asmlink.cmo asmcomp/asmlibrarian.cmo

DRIVER=driver/errors.cmo driver/compile.cmo driver/main.cmo

OPTDRIVER=driver/opterrors.cmo driver/optcompile.cmo driver/optmain.cmo

TOPLEVEL=driver/errors.cmo driver/compile.cmo \
  toplevel/printval.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo

TOPLEVELMAIN=toplevel/topmain.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)

TOPLIB=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(TOPLEVEL)

TOPOBJS=$(TOPLIB) $(TOPLEVELMAIN)

OPTOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) $(OPTDRIVER)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/predef.cmo \
  bytecomp/runtimedef.cmo bytecomp/symtable.cmo \
  toplevel/expunge.cmo

PERVASIVES=arg array char filename format hashtbl lexing list map \
  obj parsing pervasives printexc printf queue set sort stack string sys

# Recompile the system using the bootstrap compiler
all: runtime camlc camllex camlyacc camltools library camltop

# The compilation of camltop will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.

# Compile everything the first time
world: coldstart clean all

# Complete bootstrapping cycle
bootstrap:
# Save the original bootstrap compiler
	$(MAKE) backup
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/camlrun and produces bytecode for byterun/camlrun
	$(MAKE) promote-cross
# Rebuild camlc and camllex (run on byterun/camlrun)
	$(MAKE) clean
	$(MAKE) camlc camllex
# Rebuild the library (using byterun/camlrun ./camlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) promote
# Rebuild everything, including camltop and the tools
	$(MAKE) clean
	$(MAKE) all
# Check if fixpoint reached
	$(MAKE) compare

# Start up the system from the distribution compiler
coldstart:
	cd byterun; $(MAKE) all
	cp byterun/camlrun boot/camlrun
	cd yacc; $(MAKE) all
	cp yacc/camlyacc boot/camlyacc
	cd stdlib; $(MAKE) COMPILER=../boot/camlc all
	cp stdlib/stdlib.cma stdlib/*.cmi stdlib/cslheader boot

# Save the current bootstrap compiler
backup:
	if test -d boot/Saved; then : ; else mkdir boot/Saved; fi
	mv boot/Saved boot/Saved.prev
	mkdir boot/Saved
	mv boot/Saved.prev boot/Saved/Saved.prev
	cp boot/camlrun boot/Saved
	mv boot/camlc boot/camllex boot/camlyacc boot/Saved
	mv boot/*.cmi boot/stdlib.cma boot/cslheader boot/Saved

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
promote-cross:
	cp camlc boot/camlc
	cp lex/camllex boot/camllex
	cp yacc/camlyacc boot/camlyacc
	cp stdlib/stdlib.cma stdlib/*.cmi stdlib/cslheader boot

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
promote: promote-cross
	cp byterun/camlrun boot/camlrun

# Restore the saved bootstrap compiler if a problem arises
restore:
	mv boot/Saved/* boot
	rmdir boot/Saved
	mv boot/Saved.prev boot/Saved

# Check if fixpoint reached
compare:
	@if cmp boot/camlc camlc && cmp boot/camllex lex/camllex; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
        else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

# Remove old bootstrap compilers
cleanboot:
	rm -rf boot/Saved/Saved.prev/*

# Compile the native-code compiler
opt: runtimeopt camlopt libraryopt

# Installation
install:
	if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else mkdir $(LIBDIR); fi
	if test -d $(MANDIR); then : ; else mkdir $(MANDIR); fi
	cd byterun; $(MAKE) install
	cp camlc $(BINDIR)/cslc
	cp camltop $(BINDIR)/csltop
	cd stdlib; $(MAKE) install
	cp lex/camllex $(BINDIR)/csllex
	cp yacc/camlyacc $(BINDIR)/cslyacc
	$(CAMLC) -a -o $(LIBDIR)/toplevellib.cma $(TOPLIB)
	cp toplevel/topmain.cmo $(LIBDIR)
	cp toplevel/toploop.cmi toplevel/topdirs.cmi $(LIBDIR)
	cp tools/camldep $(BINDIR)/csldep
	cp tools/camlmktop $(BINDIR)/cslmktop

# Installation of the native-code compiler
installopt:
	cd asmrun; $(MAKE) install
	cp camlopt $(BINDIR)/cslopt
	cd stdlib; $(MAKE) installopt

realclean:: clean

# The compiler

camlc: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o camlc $(COMPOBJS)

clean::
	rm -f camlc

# The native-code compiler

camlopt: $(OPTOBJS)
	$(CAMLC) $(LINKFLAGS) -o camlopt $(OPTOBJS)

clean::
	rm -f camlopt

# The toplevel

camltop: $(TOPOBJS) expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o camltop.tmp $(TOPOBJS)
	- $(CAMLRUN) ./expunge camltop.tmp camltop $(PERVASIVES)
	rm -f camltop.tmp

clean::
	rm -f camltop

# The configuration file

utils/config.ml: utils/config.mlp Makefile.config
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCLINKFLAGS) $(LOWADDRESSES)|' \
            -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCLINKFLAGS)|' \
            -e 's|%%CCLIBS%%|$(CCLIBS)|' \
            -e 's|%%ARCH%%|$(ARCH)|' \
            -e 's|%%SYSTEM%%|$(SYSTEM)|' \
            utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

clean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

# The parser generator

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly

clean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer generator

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

clean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The compiler compiled with the native-code compiler
# Currently not working because it requires C primitives from byterun/meta.c
# which are not provided by asmrun/libasmrun.a

# camlc.opt: $(COMPOBJS:.cmo=.cmx)
#	$(CAMLOPT) $(LINKFLAGS) -o camlc.opt $(COMPOBJS:.cmo=.cmx)

clean::
	rm -f camlc.opt

# The native-code compiler compiled with itself

camlopt.opt: $(OPTOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o camlopt.opt $(OPTOBJS:.cmo=.cmx)

clean::
	rm -f camlopt.opt

# The numeric opcodes

bytecomp/opcodes.ml: byterun/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
        awk -f tools/make-opcodes > bytecomp/opcodes.ml

clean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# The predefined exceptions and primitives

runtime/primitives:
	cd runtime; make primitives

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

# Choose the right arch, emit and proc files

asmcomp/arch.ml: asmcomp/arch_$(ARCH).ml
	ln -s arch_$(ARCH).ml asmcomp/arch.ml

clean::
	rm -f asmcomp/arch.ml

beforedepend:: asmcomp/arch.ml

asmcomp/proc.ml: asmcomp/proc_$(ARCH).ml
	ln -s proc_$(ARCH).ml asmcomp/proc.ml

clean::
	rm -f asmcomp/proc.ml

beforedepend:: asmcomp/proc.ml

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/emit_$(ARCH).mlp tools/cvt_emit
	perl tools/cvt_emit asmcomp/emit_$(ARCH).mlp > asmcomp/emit.ml \
        || { rm -f asmcomp/emit.ml; exit 2; }

clean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

# The "expunge" utility

expunge: $(EXPUNGEOBJS)
	$(CAMLC) $(LINKFLAGS) -o expunge $(EXPUNGEOBJS)

clean::
	rm -f expunge

# The runtime system for the bytecode compiler

runtime:
	cd byterun; $(MAKE) all
realclean::
	cd byterun; $(MAKE) clean
alldepend::
	cd byterun; $(MAKE) depend

# The runtime system for the native-code compiler

runtimeopt:
	cd asmrun; $(MAKE) all
	if test -f stdlib/libasmrun.a; then :; else \
          ln -s ../asmrun/libasmrun.a stdlib/libasmrun.a; fi
realclean::
	cd asmrun; $(MAKE) clean
alldepend::
	cd asmrun; $(MAKE) depend

# The library

library:
	cd stdlib; $(MAKE) all
library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/camlrun all
libraryopt:
	cd stdlib; $(MAKE) allopt
clean::
	cd stdlib; $(MAKE) clean
alldepend::
	cd stdlib; $(MAKE) depend

# The lexer and parser generators

camllex:
	cd lex; $(MAKE) all
clean::
	cd lex; $(MAKE) clean
alldepend::
	cd lex; $(MAKE) depend

camlyacc:
	cd yacc; $(MAKE) all
realclean::
	cd yacc; $(MAKE) clean

# Tools

camltools:
	cd tools; $(MAKE) all
realclean::
	cd tools; $(MAKE) clean
alldepend::
	cd tools; $(MAKE) depend

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
	rm -f *~

depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp driver toplevel; \
         do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
         done) > .depend

alldepend:: depend

include .depend
