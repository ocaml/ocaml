override EMPTY :=
include $(dir $(lastword $(MAKEFILE_LIST)))config/Makefile

COMPLIBDIR=$(LIBDIR)/compiler-libs

override shellquote = ""'''$(subst ','\'',$1)'#)'
override DESTDIR:=$(call shellquote,$(DESTDIR))
override INSTALL_BINDIR:=$(DESTDIR)$(BINDIR)
override INSTALL_LIBDIR:=$(DESTDIR)$(LIBDIR)
override INSTALL_COMPLIBDIR:=$(DESTDIR)$(COMPLIBDIR)
override STUBLIBDIR:=$(LIBDIR)/stublibs
override INSTALL_STUBLIBDIR:=$(DESTDIR)$(STUBLIBDIR)
override INSTALL_MANDIR:=$(DESTDIR)$(MANDIR)
