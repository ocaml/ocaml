include Makefile.common

all: support.cmo rawwidget.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo camltkwrap.cmo \
     tkthread.cmo lib$(LIBNAME).a

opt: support.cmx rawwidget.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx camltkwrap.cmx \
     tkthread.cmx lib$(LIBNAME).a

COBJS=cltkCaml.o cltkUtf.o cltkEval.o cltkEvent.o cltkFile.o cltkMain.o \
      cltkMisc.o cltkTimer.o cltkVar.o cltkWait.o cltkImg.o

CCFLAGS=-I../../../byterun $(TK_DEFS) $(SHAREDCCCOMPOPTS)

COMPFLAGS=-I $(OTHERS)/unix
THFLAGS=-I $(OTHERS)/systhreads -I $(OTHERS)/threads

lib$(LIBNAME).a : $(COBJS)
	$(MKLIB) -o $(LIBNAME) $(COBJS) $(TK_LINK)

PUBMLI=fileevent.mli protocol.mli textvariable.mli timer.mli \
    rawwidget.mli widget.mli
PUB= $(PUBMLI) $(PUBMLI:.mli=.cmi) tkthread.mli tkthread.cmi tkthread.cmo

install: lib$(LIBNAME).a $(PUB)
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUB) lib$(LIBNAME).a $(INSTALLDIR)
	cd $(INSTALLDIR); $(RANLIB) lib$(LIBNAME).a
	cd $(INSTALLDIR); chmod 644 $(PUB) lib$(LIBNAME).a
	if test -f dll$(LIBNAME).so; then \
	   cp dll$(LIBNAME).so $(STUBLIBDIR)/dll$(LIBNAME).so; fi

installopt: opt
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUBMLI:.mli=.cmx) $(INSTALLDIR)
	if test -f tkthread.cmx; then \
	  cp tkthread.cmx tkthread.o $(INSTALLDIR); \
	  chmod 644 $(INSTALLDIR)/tkthread.cmx $(INSTALLDIR)/tkthread.o; \
	fi

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

tkthread.cmi: tkthread.mli
	$(CAMLCOMP) $(COMPFLAGS) $(THFLAGS) $<
tkthread.cmo: tkthread.ml
	$(CAMLCOMP) $(COMPFLAGS) $(THFLAGS) $<
tkthread.cmx: tkthread.ml
	if test -f $(OTHERS)/systhreads/threads.cmxa; then \
	  $(CAMLOPT) -c $(COMPFLAGS) $(THFLAGS) $< ; \
	fi

depend:
	$(CAMLDEP) *.mli *.ml > .depend

$(COBJS): $(TOPDIR)/config/Makefile camltk.h

include .depend
