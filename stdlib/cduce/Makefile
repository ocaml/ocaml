CDUCE_DIR=../../../cduce

SRC = \
 misc/serialize.ml \
 misc/custom.ml \
 misc/encodings.ml \
 misc/imap.ml \
 misc/state.ml \
 misc/pool.ml \
 misc/ns.ml \
 types/sortedList.ml \
 types/atoms.ml \
 misc/bool.ml \
 types/chars.ml \
 types/ident.ml \
 types/intervals.ml \
 misc/inttbl.ml \
 types/normal.ml \
 misc/pretty.ml \
 misc/stats.ml \
 types/types.ml \
 types/sequence.ml \
 types/sample.ml \
 types/patterns.ml \
 compile/lambda.ml \
 runtime/value.ml \
 runtime/run_dispatch.ml \
 runtime/explain.ml \
 runtime/serial.ml \
 typing/typepat.ml

QSRC=$(SRC:%=$(CDUCE_DIR)/%)

link:
	for i in $(QSRC) $(QSRC:.ml=.mli); do \
	  if test -f $$i; then ln -f -s $$i .; else :; fi \
	done

copy:
	for i in $(QSRC) $(QSRC:.ml=.mli); do \
	  if test -f $$i; then cp --remove-destination $$i .; else :; fi \
	done

link_clean:
	rm -f *.ml *.mli
