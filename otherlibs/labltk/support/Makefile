include Makefile.common

all: support.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo \
     liblabltk41.a

opt: support.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx \
     liblabltk41.a

COBJS=cltkCaml.o cltkEval.o cltkEvent.o cltkFile.o cltkMain.o \
      cltkMisc.o cltkTimer.o cltkVar.o cltkWait.o

CCFLAGS=-I../../../byterun $(TK_DEFS) $(X11_INCLUDES) $(SHAREDCCCOMPOPTS)

COMPFLAGS=-I $(OTHERS)/unix

liblabltk41.a : $(COBJS)
	$(MKLIB) -o labltk41 $(COBJS) $(TK_LINK) $(X11_LINK)

PUB=fileevent.cmi fileevent.mli \
    protocol.cmi protocol.mli \
    textvariable.cmi textvariable.mli \
    timer.cmi timer.mli \
    widget.cmi widget.mli

install: liblabltk41.a $(PUB)
	if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(PUB) liblabltk41.a $(LABLTKDIR)
	cd $(LABLTKDIR); $(RANLIB) liblabltk41.a
	cd $(LABLTKDIR); chmod 644 $(PUB) liblabltk41.a
	if test -f dlllabltk41.so; then \
	   cp dlllabltk41.so $(LABLTKDIR)/dlllabltk41.so; \
	   chmod 644 $(LABLTKDIR)/dlllabltk41.so; fi

clean : 
	rm -f *.cm* *.o *.a

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx .mlp .c .o

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.o:
	$(BYTECC) $(BYTECCCOMPOPTS) $(CCFLAGS) -c $<

depend:
	$(LABLDEP) *.mli *.ml > .depend

$(COBJS): $(TOPDIR)/config/Makefile camltk.h

include .depend
