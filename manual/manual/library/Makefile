CORE_INTF=Pervasives.tex

CSLDIR=$(RELEASEDIR)
P :=
include $(CSLDIR)/stdlib/StdlibModules
MLIDIR=$(CSLDIR)/ocamldoc/stdlib_non_prefixed/

STDLIB_MODULES := pervasives $(filter-out stdlib,$(STDLIB_MODULES))
PARSING_MLIS := $(wildcard $(CSLDIR)/parsing/*.mli)

STDLIB_INTF= Arg.tex Array.tex ArrayLabels.tex Char.tex Complex.tex \
  Digest.tex Filename.tex Format.tex \
  Gc.tex Genlex.tex Hashtbl.tex Int32.tex Int64.tex \
  Lazy.tex Lexing.tex List.tex ListLabels.tex Map.tex Marshal.tex \
  MoreLabels.tex Nativeint.tex Obj.tex Oo.tex \
  Parsing.tex Printexc.tex Printf.tex Queue.tex Random.tex Scanf.tex \
  Set.tex Sort.tex Stack.tex Stream.tex String.tex StringLabels.tex Sys.tex \
  Weak.tex Callback.tex Buffer.tex StdLabels.tex \
  Bytes.tex BytesLabels.tex Spacetime.tex

COMPILER_LIBS_PLUGIN_HOOKS=Pparse.tex Typemod.tex

COMPILER_LIBS_INTF=Asthelper.tex Astmapper.tex Asttypes.tex \
	Lexer.tex Location.tex Longident.tex Parse.tex Pprintast.tex Printast.tex \
	$(COMPILER_LIBS_PLUGIN_HOOKS)

OTHERLIB_INTF=Unix.tex UnixLabels.tex Str.tex \
  Graphics.tex GraphicsX11.tex \
  Thread.tex Mutex.tex Condition.tex Event.tex ThreadUnix.tex \
  Dynlink.tex Bigarray.tex

INTF=$(CORE_INTF) $(STDLIB_INTF) $(COMPILER_LIBS_INTF) $(OTHERLIB_INTF)

MLIS= $(STDLIB_MODULES:%=%.mli) \
  $(PARSING_MLIS:$(CSLDIR)/parsing/%.mli=%.mli) \
  pparse.mli \
  typemod.mli \
  simplif.mli \
  unix.mli \
  unixLabels.mli \
  str.mli \
  bigarray.mli \
  graphics.mli \
  graphicsX11.mli \
  dynlink.mli \
  thread.mli \
  mutex.mli \
  condition.mli \
  event.mli \
  threadUnix.mli

MLIS:=$(addprefix $(MLIDIR), $(MLIS))

BLURB=core.tex builtin.tex stdlib.tex compilerlibs.tex \
  libunix.tex libstr.tex libnum.tex libgraph.tex \
  libthreads.tex libdynlink.tex libbigarray.tex

FILES=$(BLURB) $(INTF)

FORMAT=../../tools/format-intf
TEXQUOTE=../../tools/texquote2



VPATH=.:$(STDLIB_DIR):$(CSLDIR)/parsing:$(CSLDIR)/otherlibs/unix:$(CSLDIR)/otherlibs/str:$(CSLDIR)/otherlibs/graph:$(CSLDIR)/otherlibs/threads:$(CSLDIR)/otherlibs/dynlink:$(CSLDIR)/otherlibs/bigarray

etex-files: $(BLURB)
all: libs

libs: $(FILES)

OCAMLDOC=$(if $(wildcard $(CSLDIR)/ocamldoc/ocamldoc.opt),\
  $(CSLDIR)/ocamldoc/ocamldoc.opt,\
  $(CSLDIR)/byterun/ocamlrun $(CSLDIR)/ocamldoc/ocamldoc) \
  -nostdlib -initially-opened-module Pervasives

$(INTF): interfaces
interfaces: $(MLIS)
	 $(OCAMLDOC) -latex \
	-I $(MLIDIR) \
	$(MLIS) \
	-sepfiles \
	-latextitle "1,subsection*" \
	-latextitle "2,subsubsection*" \
	-latex-type-prefix "TYP" \
	-latex-module-prefix "" \
	-latex-module-type-prefix "" \
	-latex-value-prefix ""
	mv -f Ast_helper.tex Asthelper.tex
	mv -f Ast_mapper.tex Astmapper.tex

clean:
	rm -f $(FILES)


.SUFFIXES:
.SUFFIXES: .tex .etex .mli

.etex.tex: $(TEXQUOTE)
	@$(TEXQUOTE) < $*.etex > $*.texquote_error.tex\
	&& mv $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex


.mli.tex: $(FORMAT)
	$(FORMAT) $< > $*.tex < $<
