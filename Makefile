#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The main Makefile

MAKEREC=$(MAKE)
include Makefile.shared

SHELL=/bin/sh
MKDIR=mkdir -p

# For users who don't read the INSTALL file
defaultentry:
	@echo "Please refer to the installation instructions in file INSTALL."
	@echo "If you've just unpacked the distribution, something like"
	@echo "	./configure"
	@echo "	make world.opt"
	@echo "	make install"
	@echo "should work.  But see the file INSTALL for more details."

# Recompile the system using the bootstrap compiler
all:
	$(MAKE) runtime
	$(MAKE) coreall
	$(MAKE) ocaml
	$(MAKE) otherlibraries $(WITH_DEBUGGER) \
	  $(WITH_OCAMLDOC)

# Compile everything the first time
world:
	$(MAKE) coldstart
	$(MAKE) all

# Compile also native code compiler and libraries, fast
world.opt:
	$(MAKE) coldstart
	$(MAKE) opt.opt

reconfigure:
	./configure $(CONFIGURE_ARGS)

# Hard bootstrap how-to:
# (only necessary in some cases, for example if you remove some primitive)
#
# make coreboot     [old system -- you were in a stable state]
# <change the source>
# make clean runtime coreall
# <debug your changes>
# make clean runtime coreall
# make coreboot [new system -- now in a stable state]

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
	$(MAKE) CAMLRUN=byterun/ocamlrun promote
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
	cd stdlib; \
	  $(MAKE) COMPILER="../boot/ocamlc -use-prims ../byterun/primitives" all
	cd stdlib; cp $(LIBFILES) ../boot
	if test -f boot/libcamlrun.a; then :; else \
	  ln -s ../byterun/libcamlrun.a boot/libcamlrun.a; fi
	if test -d stdlib/caml; then :; else \
	  ln -s ../byterun/caml stdlib/caml; fi

# Build the core system: the minimum needed to make depend and bootstrap
core:
	$(MAKE) coldstart
	$(MAKE) coreall

# Recompile the core system using the bootstrap compiler
coreall:
	$(MAKE) ocamlc
	$(MAKE) ocamllex ocamlyacc ocamltools library

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
	$(CAMLRUN) tools/stripdebug ocamlc boot/ocamlc
	$(CAMLRUN) tools/stripdebug lex/ocamllex boot/ocamllex
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	$(CAMLRUN) tools/stripdebug tools/ocamldep boot/ocamldep
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
	@if $(CAMLRUN) tools/cmpbyt boot/ocamlc ocamlc \
         && $(CAMLRUN) tools/cmpbyt boot/ocamllex lex/ocamllex \
         && $(CAMLRUN) tools/cmpbyt boot/ocamldep tools/ocamldep; \
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
	$(MAKE) otherlibrariesopt ocamltoolsopt

# Native-code versions of the tools
opt.opt:
	$(MAKE) checkstack
	$(MAKE) runtime
	$(MAKE) core
	$(MAKE) ocaml
	$(MAKE) opt-core
	$(MAKE) ocamlc.opt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC)
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt $(OCAMLDOC_OPT)

base.opt:
	$(MAKE) checkstack
	$(MAKE) runtime
	$(MAKE) core
	$(MAKE) ocaml
	$(MAKE) opt-core
	$(MAKE) ocamlc.opt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC)
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt

# Installation

COMPLIBDIR=$(LIBDIR)/compiler-libs

INSTALL_BINDIR=$(DESTDIR)$(BINDIR)
INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
INSTALL_COMPLIBDIR=$(DESTDIR)$(COMPLIBDIR)
INSTALL_STUBLIBDIR=$(DESTDIR)$(STUBLIBDIR)
INSTALL_MANDIR=$(DESTDIR)$(MANDIR)

install:
	if test -d $(INSTALL_BINDIR); then : ; \
	  else $(MKDIR) $(INSTALL_BINDIR); fi
	if test -d $(INSTALL_LIBDIR); then : ; \
	  else $(MKDIR) $(INSTALL_LIBDIR); fi
	if test -d $(INSTALL_STUBLIBDIR); then : ; \
	  else $(MKDIR) $(INSTALL_STUBLIBDIR); fi
	if test -d $(INSTALL_COMPLIBDIR); then : ; \
	  else $(MKDIR) $(INSTALL_COMPLIBDIR); fi
	if test -d $(INSTALL_MANDIR)/man$(MANEXT); then : ; \
	  else $(MKDIR) $(INSTALL_MANDIR)/man$(MANEXT); fi
	cp VERSION $(INSTALL_LIBDIR)/
	cd $(INSTALL_LIBDIR); rm -f \
	  dllbigarray$(EXT_DLL) dllnums$(EXT_DLL) dllthreads$(EXT_DLL) \
	  dllunix$(EXT_DLL) dllgraphics$(EXT_DLL) dllstr$(EXT_DLL)
	cd byterun; $(MAKE) install
	cp ocamlc $(INSTALL_BINDIR)/ocamlc.byte$(EXE)
	cp ocaml $(INSTALL_BINDIR)/ocaml$(EXE)
	cd stdlib; $(MAKE) install
	cp lex/ocamllex $(INSTALL_BINDIR)/ocamllex.byte$(EXE)
	cp $(CAMLYACC)$(EXE) $(INSTALL_BINDIR)/ocamlyacc$(EXE)
	cp utils/*.cmi utils/*.cmt utils/*.cmti utils/*.mli \
	   parsing/*.cmi parsing/*.cmt parsing/*.cmti parsing/*.mli \
	   typing/*.cmi typing/*.cmt typing/*.cmti typing/*.mli \
	   bytecomp/*.cmi bytecomp/*.cmt bytecomp/*.cmti bytecomp/*.mli \
	   driver/*.cmi driver/*.cmt driver/*.cmti driver/*.mli \
	   toplevel/*.cmi toplevel/*.cmt toplevel/*.cmti toplevel/*.mli \
	   $(INSTALL_COMPLIBDIR)
	cp compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
	   compilerlibs/ocamltoplevel.cma $(BYTESTART) $(TOPLEVELSTART) \
	   $(INSTALL_COMPLIBDIR)
	cp expunge $(INSTALL_LIBDIR)/expunge$(EXE)
	cp toplevel/topdirs.cmi toplevel/topdirs.cmt toplevel/topdirs.cmti \
           toplevel/topdirs.mli $(INSTALL_LIBDIR)
	cd tools; $(MAKE) install
	-cd man; $(MAKE) install
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) install) || exit $$?; \
	done
	if test -n "$(WITH_OCAMLDOC)"; then (cd ocamldoc; $(MAKE) install); fi
	if test -n "$(WITH_DEBUGGER)"; then (cd debugger; $(MAKE) install); fi
	cp config/Makefile $(INSTALL_LIBDIR)/Makefile.config
	if test -f ocamlopt; then $(MAKE) installopt; else \
	   cd $(INSTALL_BINDIR); \
	   ln -sf ocamlc.byte$(EXE) ocamlc$(EXE); \
	   ln -sf ocamllex.byte$(EXE) ocamllex$(EXE); \
	   fi

# Installation of the native-code compiler
installopt:
	cd asmrun; $(MAKE) install
	cp ocamlopt $(INSTALL_BINDIR)/ocamlopt.byte$(EXE)
	cd stdlib; $(MAKE) installopt
	cp middle_end/*.cmi middle_end/*.cmt middle_end/*.cmti \
	    middle_end/*.mli \
		$(INSTALL_COMPLIBDIR)
	cp middle_end/base_types/*.cmi middle_end/base_types/*.cmt \
	    middle_end/base_types/*.cmti middle_end/base_types/*.mli \
		$(INSTALL_COMPLIBDIR)
	cp asmcomp/*.cmi asmcomp/*.cmt asmcomp/*.cmti asmcomp/*.mli \
		$(INSTALL_COMPLIBDIR)
	cp compilerlibs/ocamloptcomp.cma $(OPTSTART) $(INSTALL_COMPLIBDIR)
	if test -n "$(WITH_OCAMLDOC)"; then (cd ocamldoc; $(MAKE) installopt); \
		else :; fi
	for i in $(OTHERLIBRARIES); \
	  do (cd otherlibs/$$i; $(MAKE) installopt) || exit $$?; done
	if test -f ocamlopt.opt ; then $(MAKE) installoptopt; else \
	   cd $(INSTALL_BINDIR); ln -sf ocamlopt.byte$(EXE) ocamlopt$(EXE); fi
	cd tools; $(MAKE) installopt

installoptopt:
	cp ocamlc.opt $(INSTALL_BINDIR)/ocamlc.opt$(EXE)
	cp ocamlopt.opt $(INSTALL_BINDIR)/ocamlopt.opt$(EXE)
	cp lex/ocamllex.opt $(INSTALL_BINDIR)/ocamllex.opt$(EXE)
	cd $(INSTALL_BINDIR); \
	   ln -sf ocamlc.opt$(EXE) ocamlc$(EXE); \
	   ln -sf ocamlopt.opt$(EXE) ocamlopt$(EXE); \
	   ln -sf ocamllex.opt$(EXE) ocamllex$(EXE)
	cp utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
           driver/*.cmx asmcomp/*.cmx $(INSTALL_COMPLIBDIR)
	cp compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlcommon.a \
	   compilerlibs/ocamlbytecomp.cmxa compilerlibs/ocamlbytecomp.a \
	   compilerlibs/ocamloptcomp.cmxa compilerlibs/ocamloptcomp.a \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.o) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.o) \
	   $(INSTALL_COMPLIBDIR)
	if test -f ocamlnat$(EXE) ; then \
	  cp ocamlnat$(EXE) $(INSTALL_BINDIR)/ocamlnat$(EXE); \
	  cp toplevel/opttopdirs.cmi $(INSTALL_LIBDIR); \
	  cp compilerlibs/ocamlopttoplevel.cmxa \
	     compilerlibs/ocamlopttoplevel.a \
	     $(OPTTOPLEVELSTART:.cmo=.cmx) $(OPTTOPLEVELSTART:.cmo=.o) \
	     $(INSTALL_COMPLIBDIR); \
	fi
	cd $(INSTALL_COMPLIBDIR) && $(RANLIB) ocamlcommon.a ocamlbytecomp.a \
	   ocamloptcomp.a

# Installation of the *.ml sources of compiler-libs
install-compiler-sources:
	cp utils/*.ml parsing/*.ml typing/*.ml bytecomp/*.ml driver/*.ml \
	   toplevel/*.ml middle_end/*.ml middle_end/base_types/*.ml \
	   asmcomp/*.ml $(INSTALL_COMPLIBDIR)

# Run all tests

tests: opt.opt
	cd testsuite; $(MAKE) clean && $(MAKE) all

# Build the manual latex files from the etex source files
# (see manual/README.md)
manual-pregen: opt.opt
	cd manual; $(MAKE) clean && $(MAKE) pregen-etex

# The clean target

clean:: partialclean

# Shared parts of the system

compilerlibs/ocamlcommon.cma: $(COMMON)
	$(CAMLC) -a -linkall -o $@ $(COMMON)
partialclean::
	rm -f compilerlibs/ocamlcommon.cma

# The bytecode compiler

compilerlibs/ocamlbytecomp.cma: $(BYTECOMP)
	$(CAMLC) -a -o $@ $(BYTECOMP)
partialclean::
	rm -f compilerlibs/ocamlbytecomp.cma

ocamlc: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma $(BYTESTART)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o ocamlc \
	   compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
	   $(BYTESTART)

# The native-code compiler

compilerlibs/ocamloptcomp.cma: $(MIDDLE_END) $(ASMCOMP)
	$(CAMLC) -a -o $@ $(MIDDLE_END) $(ASMCOMP)

partialclean::
	rm -f compilerlibs/ocamloptcomp.cma

ocamlopt: compilerlibs/ocamlcommon.cma compilerlibs/ocamloptcomp.cma \
          compilerlibs/ocamlbytecomp.cma $(OPTSTART)
	$(CAMLC) $(LINKFLAGS) -o ocamlopt \
	  compilerlibs/ocamlcommon.cma compilerlibs/ocamloptcomp.cma \
	  compilerlibs/ocamlbytecomp.cma $(OPTSTART)

partialclean::
	rm -f ocamlopt

# The toplevel

compilerlibs/ocamltoplevel.cma: $(TOPLEVEL)
	$(CAMLC) -a -o $@ $(TOPLEVEL)
partialclean::
	rm -f compilerlibs/ocamltoplevel.cma

ocaml: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
       compilerlibs/ocamltoplevel.cma $(TOPLEVELSTART) expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o ocaml.tmp \
	  compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
	  compilerlibs/ocamltoplevel.cma $(TOPLEVELSTART)
	- $(CAMLRUN) ./expunge ocaml.tmp ocaml $(PERVASIVES)
	rm -f ocaml.tmp

partialclean::
	rm -f ocaml

RUNTOP=./byterun/ocamlrun ./ocaml -nostdlib -I stdlib -noinit $(TOPFLAGS)
NATRUNTOP=./ocamlnat$(EXE) -nostdlib -I stdlib -noinit $(TOPFLAGS)

runtop:
	$(MAKE) runtime
	$(MAKE) coreall
	$(MAKE) ocaml
	@rlwrap --help 2>/dev/null && rlwrap $(RUNTOP) || $(RUNTOP)

natruntop:
	$(MAKE) runtime
	$(MAKE) coreall
	$(MAKE) opt.opt
	$(MAKE) ocamlnat
	@rlwrap --help 2>/dev/null && rlwrap $(NATRUNTOP) || $(NATRUNTOP)

# Native dynlink

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/natdynlink.ml
	cd otherlibs/dynlink && $(MAKE) allopt

# The configuration file

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
	    -e 's|%%BYTERUN%%|$(BINDIR)/ocamlrun|' \
	    -e 's|%%CCOMPTYPE%%|cc|' \
	    -e 's|%%BYTECC%%|$(BYTECC) $(SHAREDCCCOMPOPTS)|' \
	    -e 's|%%NATIVECC%%|$(NATIVECC)|' \
	    -e '/c_compiler =/s| -Werror||' \
	    -e 's|%%PACKLD%%|$(PACKLD)|' \
	    -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
	    -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
	    -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
	    -e 's|%%ARCMD%%|$(ARCMD)|' \
	    -e 's|%%CC_PROFILE%%|$(CC_PROFILE)|' \
	    -e 's|%%ARCH%%|$(ARCH)|' \
	    -e 's|%%MODEL%%|$(MODEL)|' \
	    -e 's|%%SYSTEM%%|$(SYSTEM)|' \
	    -e 's|%%EXT_OBJ%%|.o|' \
	    -e 's|%%EXT_ASM%%|.s|' \
	    -e 's|%%EXT_LIB%%|.a|' \
	    -e 's|%%EXT_DLL%%|$(EXT_DLL)|' \
	    -e 's|%%SYSTHREAD_SUPPORT%%|$(SYSTHREAD_SUPPORT)|' \
	    -e 's|%%ASM%%|$(ASM)|' \
	    -e 's|%%ASM_CFI_SUPPORTED%%|$(ASM_CFI_SUPPORTED)|' \
	    -e 's|%%WITH_FRAME_POINTERS%%|$(WITH_FRAME_POINTERS)|' \
	    -e 's|%%WITH_PROFINFO%%|$(WITH_PROFINFO)|' \
	    -e 's|%%WITH_SPACETIME%%|$(WITH_SPACETIME)|' \
	    -e 's|%%PROFINFO_WIDTH%%|$(PROFINFO_WIDTH)|' \
	    -e 's|%%LIBUNWIND_AVAILABLE%%|$(LIBUNWIND_AVAILABLE)|' \
	    -e 's|%%LIBUNWIND_LINK_FLAGS%%|$(LIBUNWIND_LINK_FLAGS)|' \
	    -e 's|%%MKDLL%%|$(MKDLL)|' \
	    -e 's|%%MKEXE%%|$(MKEXE)|' \
	    -e 's|%%MKMAINDLL%%|$(MKMAINDLL)|' \
	    -e 's|%%HOST%%|$(HOST)|' \
	    -e 's|%%TARGET%%|$(TARGET)|' \
	    -e 's|%%FLAMBDA%%|$(FLAMBDA)|' \
	    -e 's|%%SAFE_STRING%%|$(SAFE_STRING)|' \
	    utils/config.mlp > utils/config.ml

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

# Shared parts of the system compiled with the native-code compiler

compilerlibs/ocamlcommon.cmxa: $(COMMON:.cmo=.cmx)
	$(CAMLOPT) -a -linkall -o $@ $(COMMON:.cmo=.cmx)
partialclean::
	rm -f compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlcommon.a

# The bytecode compiler compiled with the native-code compiler

compilerlibs/ocamlbytecomp.cmxa: $(BYTECOMP:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(BYTECOMP:.cmo=.cmx)
partialclean::
	rm -f compilerlibs/ocamlbytecomp.cmxa compilerlibs/ocamlbytecomp.a

ocamlc.opt: compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlbytecomp.cmxa \
            $(BYTESTART:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -ccopt "$(BYTECCLINKOPTS)" -o ocamlc.opt \
	  compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlbytecomp.cmxa \
	  $(BYTESTART:.cmo=.cmx) -cclib "$(BYTECCLIBS)"

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

compilerlibs/ocamloptcomp.cmxa: $(MIDDLE_END:.cmo=.cmx) $(ASMCOMP:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(MIDDLE_END:.cmo=.cmx) $(ASMCOMP:.cmo=.cmx)
partialclean::
	rm -f compilerlibs/ocamloptcomp.cmxa compilerlibs/ocamloptcomp.a

ocamlopt.opt: compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
              compilerlibs/ocamlbytecomp.cmxa  \
              $(OPTSTART:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o ocamlopt.opt \
	   compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
	   compilerlibs/ocamlbytecomp.cmxa  \
	   $(OPTSTART:.cmo=.cmx)

partialclean::
	rm -f ocamlopt.opt

$(COMMON:.cmo=.cmx) $(BYTECOMP:.cmo=.cmx) $(MIDDLE_END:.cmo=.cmx) \
$(ASMCOMP:.cmo=.cmx): ocamlopt

# The numeric opcodes

bytecomp/opcodes.ml: byterun/caml/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/caml/instruct.h | \
	awk -f tools/make-opcodes > bytecomp/opcodes.ml

partialclean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# The predefined exceptions and primitives

byterun/primitives:
	cd byterun; $(MAKE) primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/caml/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' \
	     byterun/caml/fail.h; \
	 echo '|]'; \
	 echo 'let builtin_primitives = [|'; \
	 sed -e 's/.*/  "&";/' byterun/primitives; \
	 echo '|]') > bytecomp/runtimedef.ml

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH_OCAMLOPT)/arch.ml
	ln -s $(ARCH_OCAMLOPT)/arch.ml asmcomp/arch.ml

asmcomp/proc.ml: asmcomp/$(ARCH_OCAMLOPT)/proc.ml
	ln -s $(ARCH_OCAMLOPT)/proc.ml asmcomp/proc.ml

asmcomp/selection.ml: asmcomp/$(ARCH_OCAMLOPT)/selection.ml
	ln -s $(ARCH_OCAMLOPT)/selection.ml asmcomp/selection.ml

asmcomp/CSE.ml: asmcomp/$(ARCH_OCAMLOPT)/CSE.ml
	ln -s $(ARCH_OCAMLOPT)/CSE.ml asmcomp/CSE.ml

asmcomp/reload.ml: asmcomp/$(ARCH_OCAMLOPT)/reload.ml
	ln -s $(ARCH_OCAMLOPT)/reload.ml asmcomp/reload.ml

asmcomp/scheduling.ml: asmcomp/$(ARCH_OCAMLOPT)/scheduling.ml
	ln -s $(ARCH_OCAMLOPT)/scheduling.ml asmcomp/scheduling.ml

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH_OCAMLOPT)/emit.mlp tools/cvt_emit
	echo \# 1 \"$(ARCH_OCAMLOPT)/emit.mlp\" > asmcomp/emit.ml
	$(CAMLRUN) tools/cvt_emit <asmcomp/$(ARCH_OCAMLOPT)/emit.mlp \
	                          >>asmcomp/emit.ml \
	|| { rm -f asmcomp/emit.ml; exit 2; }

tools/cvt_emit: tools/cvt_emit.mll
	cd tools && $(MAKE) cvt_emit

# The "expunge" utility

expunge: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
         toplevel/expunge.cmo
	$(CAMLC) $(LINKFLAGS) -o expunge compilerlibs/ocamlcommon.cma \
	         compilerlibs/ocamlbytecomp.cma toplevel/expunge.cmo

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
	cd stdlib; $(MAKE) CAMLRUN=../byterun/ocamlrun all

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

# OCamldoc

ocamldoc: ocamlc ocamlyacc ocamllex otherlibraries
	cd ocamldoc && $(MAKE) all

ocamldoc.opt: ocamlc.opt ocamlyacc ocamllex
	cd ocamldoc && $(MAKE) opt.opt

# Documentation

html_doc: ocamldoc
	make -C ocamldoc html_doc
	@echo "documentation is in ./ocamldoc/stdlib_html/"

partialclean::
	cd ocamldoc && $(MAKE) clean

alldepend::
	cd ocamldoc && $(MAKE) depend

# The extra libraries

otherlibraries: ocamltools
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) all) || exit $$?; \
	done

otherlibrariesopt:
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) allopt) || exit $$?; \
	done

partialclean::
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i && $(MAKE) partialclean); \
	done

clean::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i && $(MAKE) clean); done

alldepend::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) depend); done

# The replay debugger

ocamldebugger: ocamlc ocamlyacc ocamllex otherlibraries
	cd debugger; $(MAKE) all

partialclean::
	cd debugger; $(MAKE) clean

alldepend::
	cd debugger; $(MAKE) depend

# Check that the stack limit is reasonable.

checkstack:
	@if $(MKEXE) -o tools/checkstack$(EXE) tools/checkstack.c; \
	  then tools/checkstack$(EXE); \
	  else :; \
	fi
	@rm -f tools/checkstack

# Make clean in the test suite

clean::
	cd testsuite; $(MAKE) clean

# Make MacOS X package

package-macosx:
	sudo rm -rf package-macosx/root
	$(MAKE) PREFIX="`pwd`"/package-macosx/root install
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
	for d in utils parsing typing bytecomp asmcomp middle_end \
	         middle_end/base_types driver toplevel tools; \
	  do rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.[so] $$d/*~; done
	rm -f *~

depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp middle_end \
	 middle_end/base_types driver toplevel; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend
	$(CAMLDEP) $(DEPFLAGS) -native \
		-impl driver/compdynlink.mlopt >> .depend
	$(CAMLDEP) $(DEPFLAGS) -bytecode \
		-impl driver/compdynlink.mlbyte >> .depend

alldepend:: depend

distclean:
	$(MAKE) clean
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/camlheader boot/ocamlyacc \
	      boot/*.cm* boot/libcamlrun.a
	rm -f config/Makefile config/m.h config/s.h
	rm -f tools/*.bak
	rm -f ocaml ocamlc
	rm -f testsuite/_log

.PHONY: all backup bootstrap checkstack clean
.PHONY: partialclean beforedepend alldepend cleanboot coldstart
.PHONY: compare core coreall
.PHONY: coreboot defaultentry depend distclean install installopt
.PHONY: library library-cross libraryopt
.PHONY: ocamldebugger ocamldoc
.PHONY: ocamldoc.opt ocamllex ocamllex.opt ocamltools ocamltoolsopt
.PHONY: ocamltoolsopt.opt ocamlyacc opt-core opt opt.opt otherlibraries
.PHONY: otherlibrariesopt package-macosx promote promote-cross
.PHONY: restore runtime runtimeopt makeruntimeopt world world.opt

include .depend
