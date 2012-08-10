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
	rm -f $(LIBNAME)top$(EXE) $(LIBNAME) *.cm* *.a

superclean:
	- if test -f tk.cmo; then \
	  echo We have changes... Now lib directory has no .cmo files; \
	  rm -f *.cm* *.o; \
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

TOPDEPS = $(TOPDIR)/toplevel/toplevellib.cma $(TOPDIR)/toplevel/topmain.cmo

$(LIBNAME).cma: $(SUPPORT) ../Widgets.src
	$(MAKE) superclean
	cd ../labltk; $(MAKE)
	cd ../camltk; $(MAKE)
	$(MKLIB) -ocamlc '$(CAMLC)' -o $(LIBNAME) -oc $(LIBNAME) \
<<<<<<< .courant
          -I ../labltk -I ../camltk $(TKOBJS) \
          $(TK_LINK)
=======
	  -I ../labltk -I ../camltk $(TKOBJS) \
	  -ccopt "\"$(TK_LINK)\""
>>>>>>> .fusion-droit.r10497

$(LIBNAME).cmxa: $(SUPPORT:.cmo=.cmx) ../Widgets.src
	$(MAKE) superclean
	cd ../labltk; $(MAKE) opt
	cd ../camltk; $(MAKE) opt
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o $(LIBNAME) -oc $(LIBNAME) \
<<<<<<< .courant
          -I ../labltk -I ../camltk $(TKOBJS:.cmo=.cmx) \
          $(TK_LINK)
=======
	  -I ../labltk -I ../camltk $(TKOBJS:.cmo=.cmx) \
	  -ccopt "\"$(TK_LINK)\""
>>>>>>> .fusion-droit.r10497

$(LIBNAME)top$(EXE) : $(TOPDEPS) $(LIBNAME).cma ../support/lib$(LIBNAME).a
	$(CAMLC) -verbose -linkall -o $(LIBNAME)top$(EXE) -I ../support \
	       -I $(TOPDIR)/toplevel toplevellib.cma \
	       -I ../labltk -I ../camltk $(LIBNAME).cma \
	       -I $(OTHERS)/unix unix.cma \
	       -I $(OTHERS)/str str.cma \
	       topstart.cmo

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
	cp $(LIBNAME).cmxa $(LIBNAME).a $(INSTALLDIR)
	cd $(INSTALLDIR); $(RANLIB) $(LIBNAME).a
	chmod 644 $(INSTALLDIR)/$(LIBNAME).cmxa
	chmod 644 $(INSTALLDIR)/$(LIBNAME).a
