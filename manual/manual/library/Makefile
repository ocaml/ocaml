CORE_INTF=pervasives.tex

STDLIB_INTF=arg.tex array.tex char.tex digest.tex filename.tex format.tex \
  gc.tex genlex.tex hashtbl.tex lazy.tex lexing.tex list.tex map.tex oo.tex \
  parsing.tex printexc.tex printf.tex queue.tex random.tex set.tex sort.tex \
  stack.tex stream.tex string.tex sys.tex weak.tex callback.tex

OTHERLIB_INTF=unix.tex str.tex num.tex arithstatus.tex graphics.tex \
  thread.tex mutex.tex condition.tex event.tex threadUnix.tex \
  dbm.tex

INTF=$(CORE_INTF) $(STDLIB_INTF) $(OTHERLIB_INTF)

BLURB=core.tex stdlib.tex libunix.tex libstr.tex libnum.tex libgraph.tex \
  libthreads.tex libdbm.tex

FILES=$(BLURB) $(INTF) $(BLURB:.tex=.htex) $(INTF:.tex=.htex)

FORMAT=../../tools/format-intf

CSLDIR=../../../csl

VPATH=.:$(CSLDIR)/stdlib:$(CSLDIR)/otherlibs/unix:$(CSLDIR)/otherlibs/str:$(CSLDIR)/otherlibs/num:$(CSLDIR)/otherlibs/graph:$(CSLDIR)/otherlibs/threads:$(CSLDIR)/otherlibs/dbm

all: $(FILES)

clean:
	rm -f $(FILES) arithstatus.mli

$(INTF) $(INTF:.tex=.htex): $(FORMAT)

.SUFFIXES:
.SUFFIXES: .tex .etex .mli .htex

.etex.tex:
	../../tools/texquote2 < $*.etex > $*.tex

.mli.tex:
	$(FORMAT) $< > $*.tex

.etex.htex:
	cp $*.etex $*.htex

.mli.htex:
	$(FORMAT) -html $< > $*.htex

# TeX hates underscores in file names
arithstatus.mli: $(CSLDIR)/otherlibs/num/arith_status.mli
	cp $(CSLDIR)/otherlibs/num/arith_status.mli arithstatus.mli
