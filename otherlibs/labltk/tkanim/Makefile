include ../support/Makefile.common

COMPFLAGS=-I ../../../byterun -I ../support -I ../camltk -I ../../unix
CCFLAGS=-I../../../byterun -I../support $(TK_DEFS) $(SHAREDCCCOMPOPTS)

all: tkanim.cma libtkanim.a
opt: tkanim.cmxa libtkanim.a
example: gifanimtest

OBJS=tkanim.cmo
COBJS= cltkaniminit.o tkAnimGIF.o

tkanim.cma: $(OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o tkanim -oc tkanim \
          $(OBJS) $(TK_LINK)

tkanim.cmxa: $(OBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o tkanim -oc tkanim \
          $(OBJS:.cmo=.cmx) $(TK_LINK)

libtkanim.a: $(COBJS)
	$(MKLIB) -o tkanim $(COBJS) $(TK_LINK)

gifanimtest-static: all gifanimtest.cmo
	$(CAMLC) -custom -o $@ -I ../lib -I ../support -I ../../unix -dllpath ../support -dllpath . unix.cma -ccopt -L. $(LIBNAME).cma tkanim.cma gifanimtest.cmo

# dynamic loading
gifanimtest: all gifanimtest.cmo
	$(CAMLC) -o $@ -I ../lib -I ../support -I ../../unix -dllpath ../support -dllpath . unix.cma $(LIBNAME).cma tkanim.cma gifanimtest.cmo

#animwish: $(TKANIM_LIB) tkAppInit.o
#	$(CC) -o $@  tkAppInit.o $(TK_LINK) $(X11_LINK) \
#		-L. -ltkanim $(LIBS)

$(OBJS) $(OBJS:.cmo=.cmi): ../lib/$(LIBNAME).cma

$(OBJS:.cmo=.cmx): ../lib/$(LIBNAME).cmxa

clean:
	rm -f *.cm* *.o *.a dlltkanim.so gifanimtest gifanimtest-static

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .cmx .c .o

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.o:
	$(BYTECC) $(BYTECCCOMPOPTS) $(CCFLAGS) -c $<


install: tkanim.cma 
	cp tkanim.cma *.cmi *.mli libtkanim.a $(INSTALLDIR)
	if [ -f dlltkanim.so ]; then \
		cp dlltkanim.so $(STUBLIBDIR)/dlltkanim.so; \
	fi

installopt: tkanim.cmxa
	cp tkanim.cmxa tkanim.a $(INSTALLDIR)

depend: tkanim.ml
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
