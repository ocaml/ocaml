ARCH=alpha

include ../Makefile.config

CAMLC=cslc
COMPFLAGS=$(INCLUDES)
LINKFLAGS=
CAMLYACC=cslyacc
YACCFLAGS=
CAMLLEX=csllex
CAMLDEP=../tools/camldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=cslrun

INCLUDES=-I ../utils -I ../typing

UTILS=../utils/misc.cmo ../utils/tbl.cmo ../typing/ident.cmo

OBJS=arch.cmo cmm.cmo printcmm.cmo \
  reg.cmo mach.cmo proc.cmo printmach.cmo \
  selection.cmo sequence.cmo liveness.cmo spill.cmo split.cmo \
  interf.cmo coloring.cmo reload.cmo linearize.cmo printlinear.cmo \
  emitaux.cmo emit.cmo \
  parsecmmaux.cmo parsecmm.cmo lexcmm.cmo \
  codegen.cmo main.cmo

codegen: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o codegen $(UTILS) $(OBJS)
clean::
	rm -f codegen

# Choose the right arch, emit and proc files

arch.ml: arch_$(ARCH).ml
	ln -s arch_$(ARCH).ml arch.ml
clean::
	rm -f arch.ml
beforedepend:: arch.ml

proc.ml: proc_$(ARCH).ml
	ln -s proc_$(ARCH).ml proc.ml
clean::
	rm -f proc.ml
beforedepend:: proc.ml

# Preprocess the code emitters

emit.ml: emit_$(ARCH).mlp ../tools/cvt_emit
	../tools/cvt_emit emit_$(ARCH).mlp > emit.ml || rm -f emit.ml
clean::
	rm -f emit.ml

beforedepend:: emit.ml

# The parser

parsecmm.mli parsecmm.ml: parsecmm.mly
	$(CAMLYACC) $(YACCFLAGS) parsecmm.mly

clean::
	rm -f parsecmm.mli parsecmm.ml parsecmm.output

beforedepend:: parsecmm.mli parsecmm.ml

# The lexer

lexcmm.ml: lexcmm.mll
	$(CAMLLEX) lexcmm.mll

clean::
	rm -f lexcmm.ml

beforedepend:: lexcmm.ml

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

clean::
	rm -f *.cm[io] *~

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

include .depend
