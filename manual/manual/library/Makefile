CORE_INTF=Pervasives.tex

STDLIB_INTF=Arg.tex Array.tex ArrayLabels.tex Char.tex Digest.tex Filename.tex Format.tex \
  Gc.tex Genlex.tex Hashtbl.tex Int32.tex Int64.tex \
  Lazy.tex Lexing.tex List.tex ListLabels.tex Map.tex Marshal.tex MoreLabels.tex \
  Nativeint.tex Obj.tex Oo.tex \
  Parsing.tex Printexc.tex Printf.tex Queue.tex Random.tex Set.tex Sort.tex \
  Stack.tex Stream.tex String.tex StringLabels.tex Sys.tex Weak.tex Callback.tex \
  Buffer.tex StdLabels.tex

OTHERLIB_INTF=Unix.tex UnixLabels.tex Str.tex \
  Num.tex Arithstatus.tex Bigint.tex \
  Graphics.tex GraphicsX11.tex Thread.tex Mutex.tex Condition.tex Event.tex ThreadUnix.tex \
  Dbm.tex Dynlink.tex Bigarray.tex Tk.tex


INTF=$(CORE_INTF) $(STDLIB_INTF) $(OTHERLIB_INTF)

MLIS=$(CSLDIR)/stdlib/*.mli \
	$(CSLDIR)/otherlibs/bigarray/bigarray.mli \
	$(CSLDIR)/otherlibs/dbm/dbm.mli \
	$(CSLDIR)/otherlibs/dynlink/dynlink.mli \
	$(CSLDIR)/otherlibs/graph/graphics.mli \
	$(CSLDIR)/otherlibs/graph/graphicsX11.mli \
	$(CSLDIR)/otherlibs/num/num.mli \
	$(CSLDIR)/otherlibs/num/arith_status.mli \
	$(CSLDIR)/otherlibs/num/big_int.mli \
	$(CSLDIR)/otherlibs/str/*.mli \
	$(CSLDIR)/otherlibs/systhreads/*.mli \
	$(CSLDIR)/otherlibs/unix/*.mli\
	tk.mli

BLURB=core.tex stdlib.tex libunix.tex libstr.tex libnum.tex libgraph.tex \
  libthreads.tex libdbm.tex libdynlink.tex liblabltk.tex libbigarray.tex

FILES=$(BLURB) $(INTF)

FORMAT=../../tools/format-intf
TEXQUOTE=../../tools/texquote2

CSLDIR=../../../csl

VPATH=.:$(CSLDIR)/stdlib:$(CSLDIR)/otherlibs/unix:$(CSLDIR)/otherlibs/str:$(CSLDIR)/otherlibs/num:$(CSLDIR)/otherlibs/graph:$(CSLDIR)/otherlibs/threads:$(CSLDIR)/otherlibs/dbm:$(CSLDIR)/otherlibs/dynlink:$(CSLDIR)/otherlibs/bigarray

all: libs 

libs: $(FILES)

$(INTF): $(MLIS)
	ocamldoc -latex -I $(CSLDIR)/stdlib  \
	-I $(CSLDIR)/otherlibs/bigarray \
	-I $(CSLDIR)/otherlibs/dbm \
	-I $(CSLDIR)/otherlibs/dynlink \
	-I $(CSLDIR)/otherlibs/graph \
	-I $(CSLDIR)/otherlibs/num \
	-I $(CSLDIR)/otherlibs/str \
	-I $(CSLDIR)/otherlibs/systhreads \
	-I $(CSLDIR)/otherlibs/unix \
	-I +labltk \
	$(MLIS) \
	-sepfiles \
	-latextitle "2,subsection*"
	mv -f Arith_status.tex Arithstatus.tex
	mv -f Big_int.tex Bigint.tex

clean:
	rm -f $(FILES) 


.SUFFIXES:
.SUFFIXES: .tex .etex .mli

.etex.tex: $(TEXQUOTE)
	$(TEXQUOTE) < $*.etex > $*.tex

.mli.tex: $(FORMAT)
	$(FORMAT) $< > $*.tex < $<

