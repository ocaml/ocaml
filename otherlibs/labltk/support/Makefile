include Makefile.common

all: support.cmo rawwidget.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo camltkwrap.cmo \
     lib$(LIBNAME).a

opt: support.cmx rawwidget.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx camltkwrap.cmx \
     lib$(LIBNAME).a

COBJS=cltkCaml.o cltkUtf.o cltkEval.o cltkEvent.o cltkFile.o cltkMain.o \
      cltkMisc.o cltkTimer.o cltkVar.o cltkWait.o cltkImg.o

CCFLAGS=-I../../../byterun $(TK_DEFS) $(SHAREDCCCOMPOPTS)

COMPFLAGS=-I $(OTHERS)/unix

lib$(LIBNAME).a : $(COBJS)
	$(MKLIB) -o $(LIBNAME) $(COBJS) $(TK_LINK)

PUB=fileevent.cmi fileevent.mli \
    protocol.cmi protocol.mli \
    textvariable.cmi textvariable.mli \
    timer.cmi timer.mli \
    rawwidget.cmi rawwidget.mli \
    widget.cmi widget.mli

install: lib$(LIBNAME).a $(PUB)
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUB) lib$(LIBNAME).a $(INSTALLDIR)
	cd $(INSTALLDIR); $(RANLIB) lib$(LIBNAME).a
	cd $(INSTALLDIR); chmod 644 $(PUB) lib$(LIBNAME).a
	if test -f dll$(LIBNAME).so; then \
	   cp dll$(LIBNAME).so $(STUBLIBDIR)/dll$(LIBNAME).so; fi

clean : 
	rm -f *.cm* *.o *.a *.so

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx .mlp .c .o

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.o:
	$(BYTECC) $(BYTECCCOMPOPTS) $(CCFLAGS) -c $<

depend:
	$(CAMLDEP) *.mli *.ml > .depend

$(COBJS): $(TOPDIR)/config/Makefile camltk.h

include .depend
