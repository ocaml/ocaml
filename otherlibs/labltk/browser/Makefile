include ../support/Makefile.common

LABLTKLIB=-I ../lib -I ../support
OTHERSLIB=-I $(OTHERS)/unix -I $(OTHERS)/str
OCAMLTOPLIB=-I $(TOPDIR)/parsing -I $(TOPDIR)/utils -I $(TOPDIR)/typing
INCLUDES=$(OTHERSLIB) $(LABLTKLIB) $(OCAMLTOPLIB)

OBJ =	list2.cmo	useunix.cmo	setpath.cmo	lexical.cmo	\
	fileselect.cmo	searchid.cmo	searchpos.cmo	shell.cmo	\
	viewer.cmo	typecheck.cmo	editor.cmo	main.cmo

JG =	jg_tk.cmo	jg_config.cmo	jg_bind.cmo	 jg_completion.cmo \
	jg_box.cmo \
	jg_button.cmo	jg_toplevel.cmo	jg_text.cmo	jg_message.cmo \
	jg_menu.cmo	jg_entry.cmo	jg_multibox.cmo	jg_memo.cmo

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(LABLCOMP) $(INCLUDES) $<

.mli.cmi:
	$(LABLCOMP) $(INCLUDES) $<

all: ocamlbrowser

ocamlbrowser: $(TOPDIR)/toplevel/toplevellib.cma jglib.cma $(OBJ)
	$(LABLC) -o ocamlbrowser $(INCLUDES) \
		$(TOPDIR)/toplevel/toplevellib.cma \
		unix.cma str.cma labltk.cma jglib.cma $(OBJ)

jglib.cma: $(JG)
	$(LABLCOMP) -a -o jglib.cma $(JG) 

install:
	if test -f ocamlbrowser; then : ; cp ocamlbrowser $(BINDIR); fi

clean:
	rm -f *.cm? ocamlbrowser dummy.mli *~ *.orig

depend:
	$(LABLDEP) *.ml *.mli > .depend

dummy.mli:
	ln -sf dummyUnix.mli dummy.mli
shell.cmo: dummy.cmi

include	.depend
