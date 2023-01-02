#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for the Unix interface library

ROOTDIR = ../..
# Note: at the moment, we need to include the configuration makefile directly
# even before we can include Makefile.otherlibs.common
# because the UNIX_OR_WIN32 variable must be defined even before
# we can include Makefile.otherlibs.common
# This will be fixed later
include $(ROOTDIR)/Makefile.config_if_required

LIBNAME=unix

EXTRACAMLFLAGS=-nolabels

# This flag is used to disable compatibility definitions in header files to
# ensure that they're not accidentally used in the library itself.
OC_CPPFLAGS += -DCAML_BUILDING_UNIX

unixLabels.cmi: \
  EXTRACAMLFLAGS += -pp "$(AWK) -f $(ROOTDIR)/stdlib/expand_module_aliases.awk"

ifeq "$(UNIX_OR_WIN32)" "win32"
WIN32_LIBS=$(call SYSLIB,ws2_32) $(call SYSLIB,advapi32)
LINKOPTS=$(addprefix -cclib ,$(WIN32_LIBS))
LDOPTS=$(addprefix -ldopt ,$(WIN32_LIBS))
else # Unix
# dllunix.so particularly requires libm for modf symbols
LDOPTS=$(NATIVECCLIBS)
endif

# C source files common to both Unix and Windows
COMMON_C_SOURCES = $(addsuffix .c, \
  access addrofstr chdir chmod cst2constr cstringv execv execve execvp fsync \
  mkdir exit getaddrinfo getcwd gethost gethostname getnameinfo getproto \
  getserv gmtime mmap_ba putenv rmdir socketaddr strofaddr time unlink)

# OS-specific C source files
OS_C_SOURCES = $(addsuffix _$(UNIX_OR_WIN32).c, \
  accept bind channels close connect dup envir errmsg getpeername getpid \
  getsockname gettimeofday isatty link listen lockf lseek mmap open pipe \
  read readlink realpath rename select sendrecv shutdown sleep socket \
  socketpair sockopt stat symlink times truncate unixsupport utimes write)

# OS-specific modules that have no counterpart on the other OS
ifeq "$(UNIX_OR_WIN32)" "win32"
OS_C_SOURCES += $(addsuffix .c, \
  close_on createprocess nonblock startup system windbug windir winlist \
  winwait winworker)
else
OS_C_SOURCES += $(addsuffix .c, \
  alarm chown chroot closedir dup2 fchmod fchown fcntl fork ftruncate \
  getegid geteuid getgid getgr getgroups getlogin getppid getpw getuid \
  initgroups itimer kill mkfifo nice opendir readdir rewinddir setgid \
  setgroups setsid setuid signals spawn termios umask wait)
endif

C_SOURCES = $(COMMON_C_SOURCES) $(OS_C_SOURCES)

CAMLOBJS=unix.cmo unixLabels.cmo

HEADERS=unixsupport.h socketaddr.h

include ../Makefile.otherlibs.common

unix.ml: unix_$(UNIX_OR_WIN32).ml
	cp $< $@

distclean::
	rm -f unix.ml

.PHONY: depend
depend: unix.ml
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

include .depend
