include ../config/Makefile

install:
	for i in *.m; do cp $$i $(MANDIR)/`basename $$i .m`.$(MANEXT); done
	echo '.so man$(MANEXT)/ocamlc.$(MANEXT)' > $(MANDIR)/ocamlc.opt.$(MANEXT)
	echo '.so man$(MANEXT)/ocamlopt.$(MANEXT)' > $(MANDIR)/ocamlopt.opt.$(MANEXT)
