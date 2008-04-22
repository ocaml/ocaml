include Makefile.common

all: support.cmo rawwidget.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo camltkwrap.cmo \
     tkthread.cmo lib$(LIBNAME).$(A)

opt: support.cmx rawwidget.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx camltkwrap.cmx \
     tkthread.cmx lib$(LIBNAME).$(A)

COBJS=cltkCaml.$(O) cltkUtf.$(O) cltkEval.$(O) cltkEvent.$(O) \
      cltkFile.$(O) cltkMain.$(O) cltkMisc.$(O) cltkTimer.$(O) \
      cltkVar.$(O) cltkWait.$(O) cltkImg.$(O)

CCFLAGS=-I../../../byterun $(TK_DEFS) $(SHAREDCCCOMPOPTS)

COMPFLAGS=-I $(OTHERS)/win32unix -I $(OTHERS)/unix
THFLAGS=-I $(OTHERS)/systhreads -I $(OTHERS)/threads

lib$(LIBNAME).$(A): $(COBJS)
	$(MKLIB) -o $(LIBNAME) $(COBJS) -ldopt "$(TK_LINK)"

PUBMLI=fileevent.mli protocol.mli textvariable.mli timer.mli \
    rawwidget.mli widget.mli
PUB= $(PUBMLI) $(PUBMLI:.mli=.cmi) tkthread.mli tkthread.cmi tkthread.cmo

install:
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUB) lib$(LIBNAME).$(A) $(INSTALLDIR)
	cd $(INSTALLDIR); $(RANLIB) lib$(LIBNAME).$(A)
	cd $(INSTALLDIR); chmod 644 $(PUB) lib$(LIBNAME).$(A)
	if test -f dll$(LIBNAME)$(EXT_DLL); then \
	   cp dll$(LIBNAME)$(EXT_DLL) $(STUBLIBDIR)/; fi

installopt:
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUBMLI:.mli=.cmx) $(INSTALLDIR)
	if test -f tkthread.$(O); then \
	  cp tkthread.cmx tkthread.$(O) $(INSTALLDIR); \
	  chmod 644 $(INSTALLDIR)/tkthread.cmx $(INSTALLDIR)/tkthread.$(O); \
	fi

clean:
	rm -f *.cm* *.o *.a *.so *.obj *.lib *.dll *.exp

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .mlp .c .$(O)

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.$(O):
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
