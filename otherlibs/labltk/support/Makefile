include Makefile.common

all: support.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo \
     liblabltk41.a

opt: support.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx \
     liblabltk41.a

COBJS=cltkCaml.o cltkEval.o cltkEvent.o cltkFile.o cltkMain.o \
      cltkMisc.o cltkTimer.o cltkVar.o cltkWait.o

CCFLAGS=-ccopt "$(TK_DEFS) $(X11_INCLUDES)"

COMPFLAGS=-I $(OTHERS)/unix

liblabltk41.a : $(COBJS)
	rm -f liblabltk41.a
	ar rc liblabltk41.a $(COBJS)
	$(RANLIB) liblabltk41.a

PUB=fileevent.cmi fileevent.mli \
    protocol.cmi protocol.mli \
    textvariable.cmi textvariable.mli \
    timer.cmi timer.mli \
    widget.cmi widget.mli

install: liblabltk41.a $(PUB)
	if test -d $(LABLTKDIR); then : ; else mkdir $(LABLTKDIR); fi
	cp $(PUB) $(LABLTKDIR)
	cp liblabltk41.a $(LABLTKDIR)
	cd $(LABLTKDIR); chmod 644 $(PUB) liblabltk41.a
	$(RANLIB) $(LABLTKDIR)/liblabltk41.a

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
	$(LABLCOMP) $(CCFLAGS) $<

depend:
	$(LABLDEP) *.mli *.ml > .depend

$(COBJS): $(TOPDIR)/config/Makefile

include .depend
