CORE_INTF=pervasives.tex

STDLIB_INTF=arg.tex array.tex char.tex digest.tex filename.tex format.tex \
  gc.tex hashtbl.tex lexing.tex list.tex map.tex parsing.tex \
  printexc.tex printf.tex queue.tex set.tex sort.tex \
  stack.tex string.tex sys.tex

INTF=$(CORE_INTF) $(STDLIB_INTF)
FILES=core.tex core.htex stdlib.tex stdlib.htex stdlib-all.tex $(INTF) $(INTF:.tex=.htex)

FORMAT=../../tools/format-intf

CSLDIR=../../../csl

VPATH=$(CSLDIR)/stdlib

all: $(FILES)

clean:
	rm -f $(FILES)

$(INTF) $(INTF:.tex=.htex): $(FORMAT)

stdlib-all.tex: $(STDLIB_INTF)
	@ (for i in $(STDLIB_INTF); do echo "\\input $$i"; done) | sort > stdlib-all.tex

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

