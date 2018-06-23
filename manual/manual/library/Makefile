SRC = ../../..

CSLDIR = $(SRC)

LD_PATH := "$(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/"
SET_LD_PATH = CAML_LD_LIBRARY_PATH=$(LD_PATH)


OCAMLDOC = $(if $(wildcard $(CSLDIR)/ocamldoc/ocamldoc.opt),\
  $(CSLDIR)/ocamldoc/ocamldoc.opt,\
  $(SET_LD_PATH) $(CSLDIR)/runtime/ocamlrun $(CSLDIR)/ocamldoc/ocamldoc) \
  -nostdlib -initially-opened-module Pervasives

# Copy and unprefix the standard library when needed
include $(SRC)/ocamldoc/Makefile.unprefix


TEXQUOTE = $(SRC)/runtime/ocamlrun ../../tools/texquote2


CORE_INTF = Pervasives.tex

STDLIB_INTF = Arg.tex Array.tex ArrayLabels.tex Char.tex Complex.tex \
  Digest.tex Filename.tex Format.tex \
  Gc.tex Genlex.tex Hashtbl.tex Int32.tex Int64.tex \
  Lazy.tex Lexing.tex List.tex ListLabels.tex Map.tex Marshal.tex \
  MoreLabels.tex Nativeint.tex Obj.tex Oo.tex \
  Parsing.tex Printexc.tex Printf.tex Queue.tex Random.tex Scanf.tex \
  Set.tex Stack.tex Stream.tex String.tex StringLabels.tex Sys.tex \
  Weak.tex Callback.tex Buffer.tex StdLabels.tex \
  Bytes.tex BytesLabels.tex Spacetime.tex

COMPILER_LIBS_PLUGIN_HOOKS = Pparse.tex Typemod.tex

COMPILER_LIBS_INTF = Asthelper.tex Astmapper.tex Asttypes.tex \
  Lexer.tex Location.tex Longident.tex Parse.tex Pprintast.tex Printast.tex \
  $(COMPILER_LIBS_PLUGIN_HOOKS)

OTHERLIB_INTF = Unix.tex UnixLabels.tex Str.tex \
  Graphics.tex GraphicsX11.tex \
  Thread.tex Mutex.tex Condition.tex Event.tex ThreadUnix.tex \
  Dynlink.tex Bigarray.tex

INTF = $(CORE_INTF) $(STDLIB_INTF) $(COMPILER_LIBS_INTF) $(OTHERLIB_INTF)

BLURB = core.tex builtin.tex stdlib.tex compilerlibs.tex \
  libunix.tex libstr.tex libnum.tex libgraph.tex \
  libthreads.tex libdynlink.tex libbigarray.tex

FILES = $(BLURB) $(INTF)


etex-files: $(BLURB)
all: libs

libs: $(FILES)


# ocamldoc.out is used as witness for the generation of the stdlib tex files to
# avoid issues with parallel make invocations.
$(INTF): ocamldoc.out
ocamldoc.out: $(STDLIB_MLIS) | unprefix_stdlib_for_ocamldoc
	$(OCAMLDOC) -latex \
	  -I $(STDLIB_UNPREFIXED) \
	  $(STDLIB_MLIS) \
	  -sepfiles \
	  -latextitle "1,subsection*" \
	  -latextitle "2,subsubsection*" \
	  -latex-type-prefix "TYP" \
	  -latex-module-prefix "" \
	  -latex-module-type-prefix "" \
	  -latex-value-prefix ""
	mv Ast_helper.tex Asthelper.tex
	mv Ast_mapper.tex Astmapper.tex

%.tex: %.etex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@


.PHONY: clean
clean:
	rm -f *.tex ocamldoc.out ocamldoc.sty
