# tkAnimGIF.c used the function Tk_ImageObjCmd, which is not available
# in a plain Tk installation. Should we disable this subdirectory ?

include ../support/Makefile.common

COMPFLAGS=-I ../support -I ../camltk -I ../../unix -I ../../win32unix
CCFLAGS=-I../../../byterun -I../support $(TK_DEFS) $(SHAREDCCCOMPOPTS)

all: tkanim.cma libtkanim.$(A)
opt: tkanim.cmxa libtkanim.$(A)
example: gifanimtest$(EXE)

OBJS=tkanim.cmo
COBJS= cltkaniminit.$(O) tkAnimGIF.$(O)

tkanim.cma: $(OBJS)
	$(MKLIB) -ocamlc '$(CAMLCB)' -o tkanim $(OBJS)

tkanim.cmxa: $(OBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPTB)' -o tkanim $(OBJS:.cmo=.cmx)

libtkanim.$(A): $(COBJS)
	$(MKLIB) -o tkanim $(COBJS)

gifanimtest-static$(EXE): all gifanimtest.cmo
	$(CAMLC) -custom -o $@ -I ../lib -I ../support -I ../../win32unix -I ../../unix -dllpath ../support -dllpath . unix.cma -ccopt -L. $(LIBNAME).cma tkanim.cma gifanimtest.cmo

# dynamic loading
gifanimtest$(EXE): all gifanimtest.cmo
	$(CAMLC) -o $@ -I ../lib -I ../support  -I ../../win32unix -I ../../unix -dllpath ../support -dllpath . unix.cma $(LIBNAME).cma tkanim.cma gifanimtest.cmo

#animwish: $(TKANIM_LIB) tkAppInit.o
#	$(CC) -o $@  tkAppInit.o $(TK_LINK) $(X11_LINK) \
#		-L. -ltkanim $(LIBS)

$(OBJS) $(OBJS:.cmo=.cmi): ../lib/$(LIBNAME).cma

$(OBJS:.cmo=.cmx): ../lib/$(LIBNAME).cmxa

clean:
	rm -f *.cm* *.$(O) *.$(A) dlltkanim$(EXT_DLL) gifanimtest$(EXE) gifanimtest-static$(EXE)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .cmx .c .$(O)

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.$(O):
	$(BYTECC) $(BYTECCCOMPOPTS) $(CCFLAGS) -c $<


install:
	cp tkanim.cma *.cmi *.mli libtkanim.$(A) $(INSTALLDIR)
	if [ -f dlltkanim$(EXT_DLL) ]; then \
		cp dlltkanim$(EXT_DLL) $(STUBLIBDIR)/; \
	fi

installopt:
	cp tkanim.cmxa tkanim.$(A) $(INSTALLDIR)

depend: tkanim.ml
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
