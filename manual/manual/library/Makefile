CORE_INTF=pervasives.tex

STDLIB_INTF=arg.tex array.tex char.tex digest.tex filename.tex format.tex \
  gc.tex genlex.tex hashtbl.tex int32.tex int64.tex \
  lazy.tex lexing.tex list.tex map.tex marshal.tex nativeint.tex oo.tex \
  parsing.tex printexc.tex printf.tex queue.tex random.tex set.tex sort.tex \
  stack.tex stream.tex string.tex sys.tex weak.tex callback.tex lazy.tex \
  buffer.tex

OTHERLIB_INTF=unix.tex str.tex num.tex arithstatus.tex graphics.tex \
  thread.tex mutex.tex condition.tex event.tex threadUnix.tex \
  dbm.tex dynlink.tex tk.tex bigarray.tex

INTF=$(CORE_INTF) $(STDLIB_INTF) $(OTHERLIB_INTF)

BLURB=core.tex stdlib.tex libunix.tex libstr.tex libnum.tex libgraph.tex \
  libthreads.tex libdbm.tex libdynlink.tex liblabltk.tex libbigarray.tex

FILES=$(BLURB) $(INTF)

FORMAT=../../tools/format-intf
TEXQUOTE=../../tools/texquote2

CSLDIR=../../../csl

VPATH=.:$(CSLDIR)/stdlib:$(CSLDIR)/otherlibs/unix:$(CSLDIR)/otherlibs/str:$(CSLDIR)/otherlibs/num:$(CSLDIR)/otherlibs/graph:$(CSLDIR)/otherlibs/threads:$(CSLDIR)/otherlibs/dbm:$(CSLDIR)/otherlibs/dynlink:$(CSLDIR)/otherlibs/bigarray

all: $(FILES)

clean:
	rm -f $(FILES) arithstatus.mli

$(INTF): $(FORMAT)

.SUFFIXES:
.SUFFIXES: .tex .etex .mli

.etex.tex: $(TEXQUOTE)
	$(TEXQUOTE) < $*.etex > $*.tex

.mli.tex: $(FORMAT)
	$(FORMAT) $< > $*.tex

# TeX hates underscores in file names
arithstatus.mli: $(CSLDIR)/otherlibs/num/arith_status.mli
	cp $(CSLDIR)/otherlibs/num/arith_status.mli arithstatus.mli
