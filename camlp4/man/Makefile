# $Id$

include ../config/Makefile

TARGET=camlp4.1

all: $(TARGET)

clean::
	rm -f $(TARGET)

depend:

get_promote:

install:
	-$(MKDIR) $(MANDIR)
	-cp $(TARGET) $(MANDIR)/.
	-cd $(MANDIR); rm -f camlp4o.1; ln -s $(TARGET) camlp4o.1
	-cd $(MANDIR); rm -f camlp4r.1; ln -s $(TARGET) camlp4r.1
	-cd $(MANDIR); rm -f mkcamlp4.1; ln -s $(TARGET) mkcamlp4.1
	-cd $(MANDIR); rm -f ocpp.1; ln -s $(TARGET) ocpp.1
	-cd $(MANDIR); rm -f camlp4o.opt.1; ln -s $(TARGET) camlp4o.opt.1
	-cd $(MANDIR); rm -f camlp4r.opt.1; ln -s $(TARGET) camlp4r.opt.1

camlp4.1: camlp4.1.tpl
	sed -e "s'LIBDIR'$(LIBDIR)'g" camlp4.1.tpl > camlp4.1
