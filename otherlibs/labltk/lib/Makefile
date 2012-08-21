#######################################################################
#                                                                     #
#                 MLTk, Tcl/Tk interface of OCaml                     #
#                                                                     #
#    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    #
#               projet Cristal, INRIA Rocquencourt                    #
#            Jacques Garrigue, Kyoto University RIMS                  #
#                                                                     #
#  Copyright 1999 Institut National de Recherche en Informatique et   #
#  en Automatique and Kyoto University.  All rights reserved.         #
#  This file is distributed under the terms of the GNU Library        #
#  General Public License, with the special exception on linking      #
#  described in file LICENSE found in the OCaml source tree.          #
#                                                                     #
#######################################################################

include ../support/Makefile.common

all: $(LIBNAME).cma $(LIBNAME)top$(EXE) $(LIBNAME)

opt: $(LIBNAME).cmxa

clean:
	rm -f $(LIBNAME)top$(EXE) $(LIBNAME) *.cm* *.$(A)

superclean:
	- if test -f tk.cmo; then \
	  echo We have changes... Now lib directory has no .cmo files; \
	  rm -f *.cm* *.$(O); \
	fi

include ../labltk/modules
LABLTKOBJS=tk.cmo $(WIDGETOBJS)

include ../camltk/modules
CAMLTKOBJS=cTk.cmo $(CWIDGETOBJS) labltk.cmo camltk.cmo

SUPPORT=../support/support.cmo ../support/rawwidget.cmo \
	../support/widget.cmo ../support/protocol.cmo \
	../support/textvariable.cmo ../support/timer.cmo \
	../support/fileevent.cmo ../support/camltkwrap.cmo

TKOBJS=$(SUPPORT) $(LABLTKOBJS) $(CAMLTKOBJS)

TOPLEVELLIBS=$(TOPDIR)/compilerlibs/ocamlcommon.cma \
             $(TOPDIR)/compilerlibs/ocamlbytecomp.cma \
             $(TOPDIR)/compilerlibs/ocamltoplevel.cma

TOPLEVELSTART=$(TOPDIR)/toplevel/topstart.cmo

TOPDEPS = $(TOPLEVELLIBS) $(TOPLEVELSTART)

$(LIBNAME).cma: $(SUPPORT) ../Widgets.src
	$(MAKE) superclean
	cd ../labltk; $(MAKE)
	cd ../camltk; $(MAKE)
	$(MKLIB) -ocamlc '$(CAMLCB)' -o $(LIBNAME) \
	  -I ../labltk -I ../camltk $(TKOBJS) \
	  $(TK_LINK)

$(LIBNAME).cmxa: $(SUPPORT:.cmo=.cmx) ../Widgets.src
	$(MAKE) superclean
	cd ../labltk; $(MAKE) opt
	cd ../camltk; $(MAKE) opt
	$(MKLIB) -ocamlopt '$(CAMLOPTB)' -o $(LIBNAME) -oc $(LIBNAME) \
	  -I ../labltk -I ../camltk $(TKOBJS:.cmo=.cmx) \
	  $(TK_LINK)

$(LIBNAME)top$(EXE) : $(TOPDEPS) $(LIBNAME).cma ../support/lib$(LIBNAME).$(A)
	$(CAMLC) -verbose -linkall -o $(LIBNAME)top$(EXE) -I ../support \
	       $(TOPLEVELLIBS) \
	       -I $(OTHERS)/unix -I $(OTHERS)/win32unix unix.cma \
	       -I ../labltk -I ../camltk $(LIBNAME).cma \
	       -I $(OTHERS)/str str.cma \
	       $(TOPLEVELSTART)

$(LIBNAME): Makefile $(TOPDIR)/config/Makefile
	@echo Generate $@
	@echo "#!/bin/sh" > $@
	@echo 'exec $(INSTALLDIR)/$(LIBNAME)top$(EXE) -I $(INSTALLDIR) "$$@"' >> $@

install-script: $(LIBNAME)
	cp $(LIBNAME) $(BINDIR)
	chmod 755 $(BINDIR)/$(LIBNAME)

install-batch:
	cp labltk.bat $(BINDIR)

install:
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(LIBNAME).cma $(LIBNAME)top$(EXE) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/$(LIBNAME).cma
	chmod 755 $(INSTALLDIR)/$(LIBNAME)top$(EXE)
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	@case x$(TOOLCHAIN) in \
	  xmingw|xmsvc) $(MAKE) install-batch ;; \
	  *) 		$(MAKE) install-script ;; \
	esac

installopt:
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(LIBNAME).cmxa $(LIBNAME).$(A) $(INSTALLDIR)
	cd $(INSTALLDIR); $(RANLIB) $(LIBNAME).$(A)
	chmod 644 $(INSTALLDIR)/$(LIBNAME).cmxa
	chmod 644 $(INSTALLDIR)/$(LIBNAME).$(A)
