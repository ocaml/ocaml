include Makefile.common

all: support.cmo rawwidget.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo camltkwrap.cmo \
     liblabltk41.a

opt: support.cmx rawwidget.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx camltkwrap.cmx \
     liblabltk41.a

COBJS=cltkCaml.o cltkEval.o cltkEvent.o cltkFile.o cltkMain.o \
      cltkMisc.o cltkTimer.o cltkVar.o cltkWait.o cltkImg.o

CCFLAGS=-I../../../byterun $(TK_DEFS) $(X11_INCLUDES) $(SHAREDCCCOMPOPTS)

COMPFLAGS=-I $(OTHERS)/unix

liblabltk41.a : $(COBJS)
	$(MKLIB) -o labltk41 $(COBJS) $(TK_LINK) $(X11_LINK)

PUB=fileevent.cmi fileevent.mli \
    protocol.cmi protocol.mli \
    textvariable.cmi textvariable.mli \
    timer.cmi timer.mli \
    rawwidget.cmi rawwidget.mli \
    widget.cmi widget.mli

install: liblabltk41.a $(PUB)
	if test -d $(MLTKDIR); then : ; else mkdir $(MLTKDIR); fi
	cp $(PUB) liblabltk41.a $(MLTKDIR)
	cd $(MLTKDIR); $(RANLIB) liblabltk41.a
	cd $(MLTKDIR); chmod 644 $(PUB) liblabltk41.a
	if test -f dlllabltk41.so; then \
	   cp dlllabltk41.so $(MLTKDIR)/dlllabltk41.so; \
	   chmod 644 $(MLTKDIR)/dlllabltk41.so; fi

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
