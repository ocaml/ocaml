# Makefile for the Unix interface library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib

OBJS=accept.o access.o addrofstr.o alarm.o bind.o chdir.o chmod.o \
  chown.o chroot.o close.o closedir.o connect.o cst2constr.o cstringv.o \
  dup.o dup2.o envir.o errmsg.o execv.o execve.o execvp.o exit.o \
  fchmod.o fchown.o fcntl.o fork.o ftruncate.o getcwd.o getegid.o \
  geteuid.o getgid.o getgr.o getgroups.o gethost.o gethostname.o \
  getlogin.o getpeername.o getpid.o getppid.o getproto.o getpw.o \
  gettimeofday.o getserv.o getsockname.o getuid.o \
  gmtime.o itimer.o kill.o link.o listen.o lockf.o lseek.o mkdir.o \
  mkfifo.o nice.o open.o opendir.o pause.o pipe.o putenv.o read.o \
  readdir.o readlink.o rename.o rewinddir.o rmdir.o select.o sendrecv.o \
  setgid.o setsid.o setuid.o shutdown.o sleep.o socket.o socketaddr.o \
  socketpair.o sockopt.o stat.o strofaddr.o symlink.o termios.o \
  time.o times.o truncate.o umask.o unixsupport.o unlink.o \
  utimes.o wait.o write.o

all: libunix.a unix.cmi unix.cma

allopt: libunix.a unix.cmi unix.cmxa

libunix.a: $(OBJS)
	rm -f libunix.a
	ar rc libunix.a $(OBJS)
	$(RANLIB) libunix.a

unix.cma: unix.cmo
	$(CAMLC) -a -linkall -o unix.cma unix.cmo

unix.cmxa: unix.cmx
	$(CAMLOPT) -a -linkall -o unix.cmxa unix.cmx

unix.cmx: ../../ocamlopt

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o

install:
	cp libunix.a $(LIBDIR)/libunix.a
	cd $(LIBDIR); $(RANLIB) libunix.a
	cp unix.cmi unix.cma unix.mli $(LIBDIR)

installopt:
	cp unix.cmx unix.cmxa unix.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) unix.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
