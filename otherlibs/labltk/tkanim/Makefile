include ../support/Makefile.common

COMPFLAGS=-I ../support -I ../camltk

all: tkanim.cma libtkanim.a
opt: tkanim.cmxa libtkanim.a
example: gifanimtest

OBJS=tkanim.cmo
COBJS= cltkaniminit.o tkAnimGIF.o

tkanim.cma: $(OBJS)
	$(MKLIB) -ocamlc '$(LABLC)' -o tkanim -oc tkanim \
          $(OBJS) $(TK_LINK) $(X11_LINK)

tkanim.cmxa: $(OBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o tkanim -oc tkanim \
          $(OBJS:.cmo=.cmx) $(TK_LINK) $(X11_LINK)

libtkanim.a: $(COBJS)
	$(MKLIB) -o tkanim $(COBJS) $(TK_LINK) $(X11_LINK)

gifanimtest: all gifanimtest.cmo
	$(LABLC) -o $@ -I ../lib -I ../support -dllpath ../support -dllpath . unix.cma labltk.cma tkanim.cma gifanimtest.cmo

animwish: $(TKANIM_LIB) tkAppInit.o
	$(CC) -o $@  tkAppInit.o $(TK_LINK) $(X11_LINK) \
		-L. -ltkanim $(LIBS)

clean:
	rm -f *.cm* *.o libtkanim.a dlltkanim.so animwish gifanimtest

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .cmx .c .o

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.o:
	$(LABLCOMP) -c $(X_CFLAGS) $(COMPFLAGS) $(TCLTKINCLUDES) $<


install: tkanim.cma 
	cp tkanim.cma *.cmi *.mli libtkanim.a $(LABLTKDIR)
	if [ -f dlltkanim.so ]; then \
		cp dlltkanim.so $(LABLTKDIR); \
	fi

installopt: tkanim.cmxa
	cp tkanim.cmxa $(LABLTKDIR)

depend: tkanim.ml
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
