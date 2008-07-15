include ../../config/Makefile

CAMLOPT=../../boot/ocamlrun ../../ocamlopt -thread
SUBCAMLOPT=../../../boot/ocamlrun ../../../ocamlopt
COMPFLAGS=
#SHARED=-shared -closed -S -dstartup
#SHARED=-shared

PLUGINS=plugin.so plugin2.so sub/plugin.so sub/plugin3.so plugin4.so mypack.so packed1.so packed1_client.so pack_client.so plugin_ref.so plugin_high_arity.so plugin_ext.so plugin_simple.so bug.so plugin_thread.so plugin4_unix.so a.so b.so c.so

all: $(PLUGINS) main mylib.so

clean:
	rm -f *.so *.o *.cm* main main_ext *.exe *.s *.asm *.obj
	rm -f *.a *.lib
	rm -f sub/*.so sub/*.o sub/*.cm* sub/*.s sub/*.asm sub/*.obj

main: api.cmx main.cmx
	$(CAMLOPT) -o main -linkall unix.cmxa threads.cmxa dynlink.cmxa api.cmx main.cmx $(PTHREAD_LINK)

main_ext: api.cmx main.cmx factorial.$(O)
	$(CAMLOPT) -o main_ext dynlink.cmxa api.cmx main.cmx factorial.$(O)

sub/plugin3.cmx: sub/api.cmi sub/api.cmx sub/plugin3.ml
	(cd sub; mv api.cmx api.cmx.bak; $(SUBCAMLOPT) -c $(COMPFLAGS) plugin3.ml; mv api.cmx.bak api.cmx)

plugin2.cmx: api.cmx plugin.cmi plugin.cmx
	(mv plugin.cmx plugin.cmx.bak; $(CAMLOPT) -c $(COMPFLAGS) plugin2.ml; mv plugin.cmx.bak plugin.cmx)

sub/api.so: sub/api.cmi sub/api.ml
	(cd sub; $(SUBCAMLOPT) -c $(COMPFLAGS) $(SHARED) api.ml)

sub/api.cmi: sub/api.mli
	(cd sub; $(SUBCAMLOPT) -c $(COMPFLAGS) api.mli)

sub/api.cmx: sub/api.cmi sub/api.ml
	(cd sub; $(SUBCAMLOPT) -c $(COMPFLAGS) api.ml)

plugin.cmx: api.cmx plugin.cmi
sub/plugin.cmx: api.cmx
plugin4.cmx: api.cmx
main.cmx: api.cmx
plugin_ext.cmx: api.cmx plugin_ext.ml
	$(CAMLOPT) -c $(COMPFLAGS) plugin_ext.ml

plugin_ext.so: factorial.$(O) plugin_ext.cmx
	$(CAMLOPT) $(COMPFLAGS) -shared -o plugin_ext.so factorial.$(O) plugin_ext.cmx

plugin4_unix.so: plugin4.cmx
	$(CAMLOPT) -shared -o plugin4_unix.so unix.cmxa plugin4.cmx

packed1_client.cmx: packed1.cmx

pack_client.cmx: mypack.cmx

packed1.cmx: api.cmx packed1.ml
	$(CAMLOPT) -c $(COMPFLAGS) -for-pack Mypack packed1.ml

mypack.cmx: packed1.cmx
	$(CAMLOPT) $(COMPFLAGS) -S -pack -o mypack.cmx packed1.cmx

mylib.cmxa: plugin.cmx plugin2.cmx
	$(CAMLOPT) $(COMPFLAGS) -a -o mylib.cmxa plugin.cmx plugin2.cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx .so .cmxa

.mli.cmi:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.cmx.so:
	$(CAMLOPT) -o $@ -shared $(COMPFLAGS) $<

.cmxa.so:
	$(CAMLOPT) -o $@ -shared -linkall $(COMPFLAGS) $<

factorial.$(O): factorial.c
	$(CAMLOPT) -c -ccopt "$(SHAREDCCCOMPOPTS)" factorial.c

demo: all
	./main plugin_thread.so

test_private:
	$(CAMLOPT) -shared -o b.so a.ml b.ml	
	$(CAMLOPT) -shared -o c.so a.ml c.ml
	./main -b.so c.so
