CDUCE_DIR=../../../cduce

SRC = \
 misc/custom.ml \
 misc/encodings.ml \
 misc/imap.ml \
 misc/state.ml \
 misc/upool.ml \
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
 types/compunit.ml \
 types/types.ml \
 types/sequence.ml \
 types/sample.ml \
 compile/auto_pat.ml \
 types/patterns.ml \
 runtime/value.ml \
 runtime/run_dispatch.ml \
 runtime/explain.ml \
 typing/typepat.ml \
 runtime/serial.ml

link:
	(cd src; \
	for i in $(SRC) $(SRC:.ml=.mli); do \
	  if test -f $(CDUCE_DIR)/$$i; then ln -f -s $(CDUCE_DIR)/$$i .; else :; fi \
	done)

copy:
	(cd src; \
	for i in $(SRC) $(SRC:.ml=.mli); do \
	  if test -f $(CDUCE_DIR)/$$i; then cp --remove-destination $(CDUCE_DIR)/$$i .; else :; fi \
	done)

clean:
	cd src; rm -f *.ml *.mli *.cm* *.o