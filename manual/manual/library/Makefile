CORE_INTF=pervasives.tex

STDLIB_INTF=arg.tex array.tex char.tex digest.tex filename.tex format.tex \
  gc.tex hashtbl.tex lexing.tex list.tex map.tex parsing.tex \
  printexc.tex printf.tex queue.tex set.tex sort.tex \
  stack.tex string.tex sys.tex

FILES=core.tex $(CORE_INTF) \
      stdlib.tex stdlib-all.tex $(STDLIB_INTF)

FORMAT=../../tools/format-intf

CSLDIR=../../../csl

VPATH=$(CSLDIR)/stdlib

all: $(FILES)

clean:
	rm -f $(FILES)

$(CORE_INTF) $(STDLIB_INTF): $(FORMAT)

stdlib-all.tex: $(STDLIB_INTF)
	@ (for i in $(STDLIB_INTF); do echo "\\input $$i"; done) | sort > stdlib-all.tex

.SUFFIXES:
.SUFFIXES: .tex .etex .mli

.etex.tex:
	../../tools/texquote2 < $*.etex > $*.tex

.mli.tex:
	$(FORMAT) $< > $*.tex

