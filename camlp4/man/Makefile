

include ../config/Makefile.cnf

TARGET=camlp4.1
ALIASES=camlp4o.1 camlp4r.1 mkcamlp4.1 ocpp.1 camlp4o.opt.1 camlp4r.opt.1

include ../config/Makefile.base

install-local:
	if test -n '$(MANDIR)'; then \
	  $(MKDIR) $(MANDIR)/man1 ; \
	  cp $(TARGET) $(MANDIR)/man1/. ; \
	  for i in $(ALIASES); do \
            rm -f $(MANDIR)/man1/$$i; \
            echo '.so man1/$(TARGET)' > $(MANDIR)/man1/$$i; \
          done; \
	fi

camlp4.1: camlp4.1.tpl
	sed -e "s'LIBDIR'$(LIBDIR)'g" camlp4.1.tpl > camlp4.1
