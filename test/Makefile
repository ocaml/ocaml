CAMLC=../boot/cslrun ../cslc -I ../stdlib -I KB -I Lex
CAMLOPT=../boot/cslrun ../cslopt -I ../stdlib -I KB -I Lex
OPTFLAGS=-S
CAMLYACC=../yacc/cslyacc
CAMLLEX=../boot/cslrun ../lex/csllex
CAMLDEP=../boot/cslrun ../tools/csldep
CAMLRUN=../byterun/cslrun
CODERUNPARAMS=CAMLRUNPARAM='o=100'

BYTE_EXE=fib.byt takc.byt taku.byt sieve.byt quicksort.byt quicksort.fast.byt \
  fft.byt fft.fast.byt soli.byt soli.fast.byt boyer.byt kb.byt \
  nucleic.byt genlex.byt

CODE_EXE=$(BYTE_EXE:.byt=.out)

all: $(BYTE_EXE) $(CODE_EXE)

# KB

BYTE_KB=KB/terms.cmo KB/equations.cmo KB/kb.cmo KB/orderings.cmo KB/kbmain.cmo
CODE_KB=$(BYTE_KB:.cmo=.cmx)

kb.byt: $(BYTE_KB)
	$(CAMLC) -I KB $(BYTE_KB) -o kb.byt
kb.out: $(CODE_KB)
	$(CAMLOPT) $(OPTFLAGS) -I KB $(CODE_KB) -o kb.out

clean::
	rm -f KB/*.cm[iox] KB/*.[os]
	rm -f KB/*~

# Genlex

BYTE_GENLEX=Lex/syntax.cmo Lex/scan_aux.cmo Lex/scanner.cmo Lex/gram_aux.cmo \
  Lex/grammar.cmo Lex/lexgen.cmo Lex/output.cmo Lex/main.cmo
CODE_GENLEX=$(BYTE_GENLEX:.cmo=.cmx)

genlex.byt: $(BYTE_GENLEX)
	$(CAMLC) -I Lex $(BYTE_GENLEX) -o genlex.byt
genlex.out: $(CODE_GENLEX)
	$(CAMLOPT) $(OPTFLAGS) -I Lex $(CODE_GENLEX) -o genlex.out

clean::
	rm -f Lex/*.cm[iox] Lex/*.[os]
	rm -f Lex/*~

Lex/grammar.ml Lex/grammar.mli: Lex/grammar.mly
	$(CAMLYACC) $(YACCFLAGS) Lex/grammar.mly

clean::
	rm -f Lex/grammar.ml Lex/grammar.mli
beforedepend:: Lex/grammar.ml Lex/grammar.mli

Lex/scanner.ml: Lex/scanner.mll
	$(CAMLLEX) Lex/scanner.mll

clean::
	rm -f Lex/scanner.ml
beforedepend:: Lex/scanner.ml

# Common rules

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .byt .fast.byt .out .fast.out

.ml.byt:
	$(CAMLC) -o $*.byt $<

.ml.fast.byt:
	cp $*.ml $*_fast.ml
	$(CAMLC) -unsafe -o $*.fast.byt $*_fast.ml
	rm -f $*_fast.ml

.ml.out:
	$(CAMLOPT) $(OPTFLAGS) -o $*.out $<

.ml.fast.out:
	cp $*.ml $*_fast.ml
	$(CAMLOPT) $(OPTFLAGS) -unsafe -o $*.fast.out $*_fast.ml
	rm -f $*_fast.ml

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmo:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTFLAGS) -c $<

$(BYTE_EXE) $(BYTE_KB) $(BYTE_GENLEX): ../cslc
$(BYTE_EXE): ../stdlib/stdlib.cma
$(CODE_EXE) $(CODE_KB) $(CODE_GENLEX): ../cslopt
$(CODE_EXE): ../stdlib/stdlib.cmxa ../stdlib/libasmrun.a

clean::
	rm -f *.byt *.out
	rm -f *.cm[iox] *.[os]
	rm -f *~

# Regression test

test: codetest

bytetest:
	set -e; \
        for prog in $(BYTE_EXE:.byt=); do \
          echo $$prog; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest test $(CAMLRUN) $$prog.byt; \
          else \
            $(CAMLRUN) $$prog.byt | cmp - Results/$$prog.out; \
          fi; \
        done

codetest:
	set -e; \
        for prog in $(CODE_EXE:.out=); do \
          echo $$prog; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest test $$prog.out; \
          else \
            $$prog.out | cmp - Results/$$prog.out; \
          fi; \
        done

clean::
	rm -f Lex/testscanner.ml

# Benchmark

bench: codebench

bytebench:
	set -e; \
        for prog in $(BYTE_EXE:.byt=); do \
          echo -n "$$prog		"; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest bench $(CAMLRUN) $$prog.byt; \
          else \
            xtime -o /dev/null -e /dev/null $(CAMLRUN) $$prog.byt; \
          fi; \
        done

codebench:
	set -e; \
        for prog in $(CODE_EXE:.out=); do \
          echo -n "$$prog		"; \
          if test -f Results/$$prog.runtest; then \
            $(CODERUNPARAMS) sh Results/$$prog.runtest bench $$prog.out; \
          else \
            $(CODERUNPARAMS) xtime -repeat 3 -o /dev/null -e /dev/null $$prog.out; \
          fi; \
        done

# Dependencies

depend: beforedepend
	$(CAMLDEP) -I KB -I Lex *.mli *.ml KB/*.mli KB/*.ml Lex/*.mli Lex/*.ml > .depend

include .depend

