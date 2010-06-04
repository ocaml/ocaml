<<<<<<< .courant
include ../support/Makefile.common

LABLTKLIB=-I ../labltk -I ../lib -I ../support
#OTHERSLIB=-I $(OTHERS)/win32unix -I $(OTHERS)/systhreads -I $(OTHERS)/str
=======
# $Id$

>>>>>>> .fusion-droit.r10497
OTHERSLIB=-I $(OTHERS)/unix -I $(OTHERS)/str
OCAMLTOPLIB=-I $(TOPDIR)/parsing -I $(TOPDIR)/utils -I $(TOPDIR)/typing
INCLUDES=$(OTHERSLIB) $(LABLTKLIB) $(OCAMLTOPLIB)

OBJ =	list2.cmo	useunix.cmo	setpath.cmo	lexical.cmo	\
	fileselect.cmo	searchid.cmo	searchpos.cmo	shell.cmo	\
	help.cmo	\
	viewer.cmo	typecheck.cmo	editor.cmo	main.cmo

JG =	jg_tk.cmo	jg_config.cmo	jg_bind.cmo	 jg_completion.cmo \
	jg_box.cmo \
	jg_button.cmo	jg_toplevel.cmo	jg_text.cmo	jg_message.cmo \
	jg_menu.cmo	jg_entry.cmo	jg_multibox.cmo	jg_memo.cmo

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLCOMP) $(INCLUDES) $<

.mli.cmi:
	$(CAMLCOMP) $(INCLUDES) $<

all: ocamlbrowser$(EXE)

ocamlbrowser$(EXE): $(TOPDIR)/toplevel/toplevellib.cma jglib.cma $(OBJ) \
			../support/lib$(LIBNAME).a
	$(CAMLC) -nojoin -o ocamlbrowser$(EXE) $(INCLUDES) \
		$(TOPDIR)/toplevel/toplevellib.cma \
		unix.cma str.cma $(LIBNAME).cma jglib.cma $(OBJ)

ocamlbrowser.cma: jglib.cma $(OBJ)
	$(CAMLC) -nojoin -a -o $@ -linkall jglib.cma $(OBJ)

jglib.cma: $(JG)
	$(CAMLCOMP) -a -o jglib.cma $(JG)

help.ml:
	echo 'let text = "\\' > $@
	sed -e 's/^ /\\032/' -e 's/$$/\\n\\/' help.txt >> $@
	echo '";;' >> $@

install:
	if test -f ocamlbrowser$(EXE); then : ; \
	  cp ocamlbrowser$(EXE) $(BINDIR)/jocamlbrowser$(EXE); fi

clean:
	rm -f *.cm? ocamlbrowser$(EXE) dummy.mli *~ *.orig

depend:
	$(CAMLDEP) *.ml *.mli > .depend

dummy.mli:
	rm -f $@
	ln -s dummyUnix.mli $@
shell.cmo: dummy.cmi
setpath.cmo fileselect.cmo lexical.cmi searchid.cmi typecheck.cmi: $(TOPDIR)/toplevel/toplevellib.cma

include	.depend
