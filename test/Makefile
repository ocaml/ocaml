CAMLC=../boot/camlrun ../camlc -I ../stdlib
CAMLYACC=../yacc/camlyacc
CAMLLEX=../boot/camlrun ../lex/camllex
CAMLDEP=../tools/camldep

EXE=fib takc taku sieve quicksort quicksort.fast fft fft.fast \
  soli soli.fast boyer kb nucleic genlex

all: $(EXE)

# KB

KBFILES=KB/terms.mli KB/terms.ml KB/equations.mli KB/equations.ml \
  KB/kb.mli KB/kb.ml KB/orderings.mli KB/orderings.ml KB/kbmain.ml

kb: $(KBFILES)
	$(CAMLC) -I KB $(KBFILES) -o kb

clean::
	rm -f KB/*.cm[io]
	rm -f KB/*~

# Genlex

GENLEXFILES=Lex/syntax.ml Lex/scan_aux.ml Lex/grammar.mli Lex/scanner.ml \
  Lex/gram_aux.ml Lex/grammar.ml Lex/lexgen.ml Lex/output.ml Lex/main.ml

genlex: $(GENLEXFILES)
	$(CAMLC) -I Lex $(GENLEXFILES) -o genlex

clean::
	rm -f Lex/*.cm[io]
	rm -f Lex/*~

Lex/grammar.ml Lex/grammar.mli: Lex/grammar.mly $(CAMLYACC)
	$(CAMLYACC) $(YACCFLAGS) Lex/grammar.mly

clean::
	rm -f Lex/grammar.ml Lex/grammar.mli

Lex/scanner.ml: Lex/scanner.mll $(CAMLLEX)
	$(CAMLLEX) Lex/scanner.mll

clean::
	rm -f Lex/scanner.ml

# Common rules

.SUFFIXES: .mli .ml .cmi .cmo .fast

.ml:
	$(CAMLC) -o $* $<

.ml.fast:
	$(CAMLC) -fast -o $*.fast $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmo:
	$(CAMLC) -c $<

$(EXE): ../camlc

clean::
	rm -f $(EXE)
	rm -f *.cm[io]
	rm -f *~

# Regression test

test:
	set -e; \
        for prog in $(EXE); do \
          echo $$prog; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest test; \
          elif test -f Results/$$prog.out; then \
            sh Results/runtest $$prog; \
          fi; \
        done

clean::
	rm -f Lex/testscanner.ml

# Benchmark

bench:
	set -e; \
        for prog in $(EXE); do \
          echo $$prog; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest bench; \
          else \
            xtime -o /dev/null ../byterun/camlrun $$prog; \
          fi; \
        done
