include ../Makefile.config

LINKER=labltklink
LABLTKLIB=-I $(INSTALLDIR)
INCLUDES=$(LABLTKLIB) $(OLABLINCLUDES)
OLABLINCLUDES=-I $(OCAMLDIR)/parsing -I $(OCAMLDIR)/utils -I $(OCAMLDIR)/typing

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

all: lablbrowser

lablbrowser: jglib.cma $(OBJ)
	$(LINKER) -o lablbrowser $(LABLTKLIB) toplevellib.cma \
		unix.cma str.cma tk41.cma jglib.cma $(OBJ) \
		-cclib -lstr -cclib -lunix $(SYSLIBS)

jglib.cma: $(JG)
	$(LABLCOMP) -a -o jglib.cma $(JG) 

install:
	if test -f lablbrowser; then : ; cp lablbrowser $(INSTALLBINDIR); fi

clean:
	rm -f *.cm? lablbrowser *~ *.orig

depend:
	$(LABLDEP) *.ml *.mli > .depend

include	.depend
