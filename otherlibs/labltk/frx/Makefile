include ../support/Makefile.common

COMPFLAGS=-I ../camltk -I ../support

OBJS= frx_misc.cmo frx_widget.cmo frx_font.cmo frx_entry.cmo frx_text.cmo \
      frx_listbox.cmo frx_req.cmo frx_fillbox.cmo frx_focus.cmo \
      frx_dialog.cmo frx_mem.cmo frx_rpc.cmo frx_synth.cmo frx_selection.cmo \
      frx_after.cmo frx_fit.cmo frx_ctext.cmo frx_color.cmo

OBJSX = $(OBJS:.cmo=.cmx)

all: frxlib.cma

opt: frxlib.cmxa

frxlib.cma: $(OBJS)
	$(CAMLLIBR) -o frxlib.cma $(OBJS)

frxlib.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o frxlib.cmxa $(OBJSX)

install:
	cp *.cmi *.mli frxlib.cma $(INSTALLDIR)

installopt:
	cp frxlib.cmxa frxlib.$(A) $(INSTALLDIR)

clean:
	rm -f *.cm* *.$(O) *.$(A)

$(OBJS) $(OBJS:.cmo=.cmi): ../lib/$(LIBNAME).cma

$(OBJSX): ../lib/$(LIBNAME).cmxa

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<


depend: 
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
