# Makefile for the Unix interface library

include ../../Makefile.config

# Compilation options
CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
CAMLC=../../boot/camlrun ../../boot/camlc -I ../../boot

OBJS=accept.o access.o addrofstr.o alarm.o bind.o chdir.o chmod.o \
  chown.o chroot.o close.o closedir.o connect.o cst2constr.o cstringv.o \
  dup.o dup2.o envir.o errmsg.o execv.o execve.o execvp.o exit.o \
  fchmod.o fchown.o fcntl.o fork.o ftruncate.o getcwd.o getegid.o \
  geteuid.o getgid.o getgr.o getgroups.o gethost.o gethostname.o \
  getlogin.o getpeername.o getpid.o getppid.o getproto.o getpw.o \
  getserv.o getsockname.o getuid.o \
  gmtime.o ioctl.o kill.o link.o listen.o lockf.o lseek.o mkdir.o \
  mkfifo.o nice.o open.o opendir.o pause.o pipe.o read.o \
  readdir.o readlink.o rename.o rewinddir.o rmdir.o select.o sendrecv.o \
  setgid.o setuid.o shutdown.o sleep.o socket.o socketaddr.o \
  socketpair.o stat.o strofaddr.o symlink.o termios.o time.o times.o \
  truncate.o umask.o unix.o unlink.o utimes.o wait.o waitopt.o waitpid.o \
  write.o

INTF= unix.cmi
IMPL= unix.cmo
LIB= unix.cma

all: libunix.a $(INTF) $(LIB)

libunix.a: $(OBJS)
	rm -f libunix.a
	ar rc libunix.a $(OBJS)
	$(RANLIB) libunix.a

unix.cma: $(IMPL)
	$(CAMLC) -a -o unix.cma $(IMPL)

clean:
	rm -f libunix.a *.o *.cm[ioa]

install:
	cp libunix.a $(LIBDIR)/libunix.a
	cd $(LIBDIR); $(RANLIB) libunix.a
	cp $(INTF) $(LIB) $(LIBDIR)

.SUFFIXES: .ml .mli .cmo .cmi

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/camldep *.mli *.ml >> .depend

include .depend
