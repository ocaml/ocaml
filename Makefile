# The main Makefile

include config/Makefile.h
include Makefile.config

CAMLC=boot/camlrun boot/camlc -I boot
COMPFLAGS=$(INCLUDES)
LINKFLAGS=
CAMLYACC=boot/camlyacc
YACCFLAGS=
CAMLLEX=boot/camlrun boot/camllex
CAMLDEP=tools/camldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/camlrun

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I driver -I toplevel

UTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/meta.cmo utils/terminfo.cmo utils/crc.cmo

PARSING=parsing/location.cmo parsing/parser.cmo parsing/lexer.cmo parsing/parse.cmo

TYPING=typing/ident.cmo typing/path.cmo typing/typedtree.cmo \
  typing/subst.cmo typing/printtyp.cmo \
  typing/predef.cmo typing/env.cmo \
  typing/ctype.cmo typing/mtype.cmo \
  typing/includecore.cmo typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typemod.cmo

BYTECOMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/dectree.cmo bytecomp/matching.cmo \
  bytecomp/translcore.cmo bytecomp/translmod.cmo \
  bytecomp/instruct.cmo bytecomp/codegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/runtimedef.cmo bytecomp/symtable.cmo \
  bytecomp/librarian.cmo bytecomp/linker.cmo

DRIVER=driver/errors.cmo driver/compile.cmo driver/main.cmo

TOPLEVEL=driver/errors.cmo driver/compile.cmo \
  toplevel/printval.cmo toplevel/toploop.cmo toplevel/topdirs.cmo \
  toplevel/topmain.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(BYTECOMP) $(DRIVER)

TOPOBJS=$(UTILS) $(PARSING) $(TYPING) $(BYTECOMP) $(TOPLEVEL)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/predef.cmo \
  bytecomp/runtimedef.cmo bytecomp/symtable.cmo \
  toplevel/expunge.cmo

PERVASIVES=arg array char filename format hashtbl lexing list map \
  obj parsing pervasives printexc printf queue set sort stack string sys

# Recompile the system using the bootstrap compiler
all: runtime camlc camllex camlyacc library camltop

# The compilation of camltop will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.

# Compile everything the first time
world: coldstart all

# Complete bootstrapping cycle
bootstrap: backup promote-cross clean camlc camllex library-cross promote clean all compare
# backup        save the bootstrap compiler
# promote-cross promote the new compiler but keep the old runtime
#               (runs on boot/camlrun, produces code for byterun/camlrun)
# clean camlc camllex
#               rebuild the compiler (runs on byterun/camlrun)
# library-cross rebuild the library (using byterun/camlrun camlc)
# promote       promote the new compiler and the new runtime
# clean all     rebuild everything
# compare       check fixpoint

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
	test -d boot/Saved || mkdir boot/Saved
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

# Installation
install:
	test -d $(BINDIR) || mkdir $(BINDIR)
	test -d $(LIBDIR) || mkdir $(LIBDIR)
	test -d $(MANDIR) || mkdir $(MANDIR)
	cd byterun; $(MAKE) install
	cp camlc $(BINDIR)/cslc
	cp camltop $(BINDIR)/csltop
	cd stdlib; $(MAKE) install
	cp lex/camllex $(BINDIR)/csllex
	cp yacc/camlyacc $(BINDIR)/cslyacc
	$(CAMLC) -a -o $(LIBDIR)/toplevellib.cma $(TOPOBJS)
	cp tools/camldep $(BINDIR)/csldep
	cp tools/camlmktop $(BINDIR)/cslmktop

realclean:: clean

# The compiler

camlc: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o camlc $(COMPOBJS)

clean::
	rm -f camlc

# The toplevel

camltop: $(TOPOBJS) expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o camltop.tmp $(TOPOBJS)
	- $(CAMLRUN) ./expunge camltop.tmp camltop $(PERVASIVES)
	rm -f camltop.tmp

clean::
	rm -f camltop

# The configuration file

utils/config.ml: utils/config.mlp Makefile.config
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%CC%%|$(CC) $(CCLINKFLAGS) $(LOWADDRESSES)|' \
            -e 's|%%CCLIBS%%|$(CCLIBS)|' \
            utils/config.mlp > utils/config.ml

clean::
	rm -f utils/config.ml

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

# The "expunge" utility

expunge: $(EXPUNGEOBJS)
	$(CAMLC) $(LINKFLAGS) -o expunge $(EXPUNGEOBJS)

clean::
	rm -f expunge

# The runtime system

runtime:
	cd byterun; $(MAKE) all
realclean::
	cd byterun; $(MAKE) clean
alldepend::
	cd byterun; $(MAKE) depend

# The library

library:
	cd stdlib; $(MAKE) all
library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/camlrun all
clean::
	cd stdlib; $(MAKE) clean
alldepend::
	cd stdlib; $(MAKE) depend

# The lexer and parser generators

camllex:
	cd lex; $(MAKE)
clean::
	cd lex; $(MAKE) clean
alldepend::
	cd lex; $(MAKE) depend

camlyacc:
	cd yacc; $(MAKE)
realclean::
	cd yacc; $(MAKE) clean

# Utilities

realclean::
	cd tools; $(MAKE) clean
alldepend::
	cd tools; $(MAKE) depend

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

clean::
	rm -f utils/*.cm[io] utils/*~
	rm -f parsing/*.cm[io] parsing/*~
	rm -f typing/*.cm[io] typing/*~
	rm -f bytecomp/*.cm[io] bytecomp/*~
	rm -f driver/*.cm[io] driver/*~
	rm -f toplevel/*.cm[io] toplevel/*~
	rm -f *~

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) */*.mli */*.ml > .depend

alldepend:: depend

include .depend
