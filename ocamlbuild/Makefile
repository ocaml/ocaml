.PHONY: all byte native profile debug ppcache doc

ifndef INSTALL_PREFIX
INSTALL_PREFIX := $(PWD)/_install
endif

ifndef INSTALL_LIB
INSTALL_LIB := $(INSTALL_PREFIX)/lib/ocamlbuild
endif

ifndef INSTALL_BIN
INSTALL_BIN := $(INSTALL_PREFIX)/bin
endif

ifndef BUILDDIR
BUILDDIR := "_build"
endif

ifndef OCAMLBUILDCMD
OCAMLBUILDCMD := ./boot/ocamlbuild
endif

ifdef O
OCAMLBUILD_OPTIONS := $(OCAMLBUILD_OPTIONS) $(O)
endif

ifeq ($(wildcard ./ocamlbuild_Myocamlbuil*_config.ml),./ocamlbuild_Myocamlbuild_config.ml)
ifeq ($(wildcard ./boot/oc*build),./boot/ocamlbuild)
OCAMLBUILD=INSTALL_LIB=$(INSTALL_LIB) INSTALL_BIN=$(INSTALL_BIN) $(OCAMLBUILDCMD) -build-dir $(BUILDDIR) -no-links $(OCAMLBUILD_OPTIONS)
LIBS=ocamlbuildlib ocamlbuildlightlib
PROGRAMS=ocamlbuild ocamlbuildlight
BYTE=$(LIBS:=.cma) $(PROGRAMS:=.byte)
NATIVE=$(LIBS:=.cmxa) $(PROGRAMS:=.native)

all:
	$(OCAMLBUILD) $(BYTE) $(NATIVE)
byte:
	$(OCAMLBUILD) $(BYTE)
native:
	$(OCAMLBUILD) $(NATIVE)
profile:
	$(OCAMLBUILD) $(LIBS:=.p.cmxa) $(PROGRAMS:=.p.native)
debug:
	$(OCAMLBUILD) $(LIBS:=.d.cma) $(PROGRAMS:=.d.byte)
ppcache:
	$(OCAMLBUILD) ppcache.byte ppcache.native
doc:
	$(OCAMLBUILD) ocamlbuild.docdir/index.html
	ln -s -f $(BUILDDIR)/ocamlbuild.docdir doc
else
all byte native: ocamlbuild.byte.start
	mkdir -p boot
	cp ocamlbuild.byte.start boot/ocamlbuild
	$(MAKE) $(MFLAGS) $(MAKECMDGOALS)
	cp $(BUILDDIR)/ocamlbuild.native boot/ocamlbuild
	$(MAKE) $(MFLAGS) $(MAKECMDGOALS) OCAMLBUILD_OPTIONS="-nothing-should-be-rebuilt -verbose -1"
endif
else
all byte native:
	@echo "Please copy the myocamlbuild_config.ml of the OCaml source distribution"
	@echo "  as ocamlbuild_Myocamlbuild_config.ml"
	@echo
	@echo "$$ cp ../myocamlbuild_config.ml ocamlbuild_Myocamlbuild_config.ml"
endif

ocamlbuild.byte.start:
	./start.sh

promote:
	cp $(BUILDDIR)/ocamlbuild.native boot/ocamlbuild

clean:
	rm -rf $(BUILDDIR)

distclean: clean
	rm -rf _log _start ocamlbuild.byte.start boot/ocamlbuild

install: all
	mkdir -p $(INSTALL_BIN)
	mkdir -p $(INSTALL_LIB)
	install $(BUILDDIR)/ocamlbuild.byte \
		$(BUILDDIR)/ocamlbuild.native \
		$(BUILDDIR)/ocamlbuildlight.byte \
		$(BUILDDIR)/ocamlbuildlight.native \
		$(INSTALL_BIN)
	install $(BUILDDIR)/ocamlbuild.native $(INSTALL_BIN)/ocamlbuild
	install $(BUILDDIR)/ocamlbuildlight.byte $(INSTALL_BIN)/ocamlbuildlight
	install -m 644 \
	        $(BUILDDIR)/ocamlbuildlib.cmxa \
	        $(BUILDDIR)/ocamlbuildlib.a \
	        $(BUILDDIR)/ocamlbuildlib.cma \
	        $(BUILDDIR)/ocamlbuildlightlib.cmxa \
	        $(BUILDDIR)/ocamlbuildlightlib.a \
	        $(BUILDDIR)/ocamlbuildlightlib.cma \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.cmx \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.o \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.cmo \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.cmi \
	        $(BUILDDIR)/ocamlbuild_executor.cmi \
	        $(BUILDDIR)/ocamlbuild_executor.cmo \
	        $(BUILDDIR)/ocamlbuild_executor.cmx \
	        $(BUILDDIR)/ocamlbuild_executor.o \
	        $(BUILDDIR)/ocamlbuild_pack.cmi \
	        $(BUILDDIR)/ocamlbuild_pack.cmo \
	        $(BUILDDIR)/ocamlbuild_pack.cmx \
	        $(BUILDDIR)/ocamlbuild_pack.o \
	        $(BUILDDIR)/ocamlbuild.cmi \
	        $(BUILDDIR)/ocamlbuild_plugin.cmi \
	        $(BUILDDIR)/ocamlbuild.cmx \
	        $(BUILDDIR)/ocamlbuild.o \
	        $(BUILDDIR)/ocamlbuild.cmo \
	        $(BUILDDIR)/ocamlbuildlight.cmx \
	        $(BUILDDIR)/ocamlbuildlight.o \
	        $(BUILDDIR)/ocamlbuildlight.cmo $(INSTALL_LIB)
	ranlib $(INSTALL_LIB)/ocamlbuildlib.a
	ranlib $(INSTALL_LIB)/ocamlbuildlightlib.a
