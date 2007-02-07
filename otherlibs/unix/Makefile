#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# $Id$

# Makefile for the Unix interface library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../ocamlcomp.sh
CAMLOPT=../../ocamlcompopt.sh
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A -g

OBJS=accept.o access.o addrofstr.o alarm.o bind.o chdir.o chmod.o \
  chown.o chroot.o close.o closedir.o connect.o cst2constr.o cstringv.o \
  dup.o dup2.o envir.o errmsg.o execv.o execve.o execvp.o exit.o \
  fchmod.o fchown.o fcntl.o fork.o ftruncate.o \
  getaddrinfo.o getcwd.o getegid.o geteuid.o getgid.o \
  getgr.o getgroups.o gethost.o gethostname.o getlogin.o \
  getnameinfo.o getpeername.o getpid.o getppid.o getproto.o getpw.o \
  gettimeofday.o getserv.o getsockname.o getuid.o \
  gmtime.o isatty.o itimer.o kill.o link.o listen.o lockf.o lseek.o mkdir.o \
  mkfifo.o nice.o open.o opendir.o pipe.o putenv.o read.o \
  readdir.o readlink.o rename.o rewinddir.o rmdir.o select.o sendrecv.o \
  setgid.o setsid.o setuid.o shutdown.o signals.o \
  sleep.o socket.o socketaddr.o \
  socketpair.o sockopt.o stat.o strofaddr.o symlink.o termios.o \
  time.o times.o truncate.o umask.o unixsupport.o unlink.o \
  utimes.o wait.o write.o

MLOBJS=unix.cmo unixLabels.cmo

all: libunix.a unix.cma

allopt: libunix.a unix.cmxa

libunix.a: $(OBJS)
	$(MKLIB) -o unix $(OBJS)

unix.cma: $(MLOBJS)
	$(MKLIB) -o unix -ocamlc '$(CAMLC)' -linkall $(MLOBJS)

unix.cmxa: $(MLOBJS:.cmo=.cmx)
	$(MKLIB) -o unix -ocamlopt '$(CAMLOPT)' -linkall $(MLOBJS:.cmo=.cmx)

unix.cmx: ../../ocamlopt

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o *.so

install:
	if test -f dllunix.so; then cp dllunix.so $(STUBLIBDIR)/dllunix.so; fi
	cp libunix.a $(LIBDIR)/libunix.a
	cd $(LIBDIR); $(RANLIB) libunix.a
	cp unix.cma $(MLOBJS:.cmo=.cmi) $(MLOBJS:.cmo=.mli) $(LIBDIR)
	cp unixsupport.h $(LIBDIR)/caml

installopt:
	cp $(MLOBJS:.cmo=.cmx) unix.cmxa unix.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) unix.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) -nolabels $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) -nolabels $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
