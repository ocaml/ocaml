#include <mlvalues.h>
#include "unix.h"

#ifdef HAS_LOCKF
#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define F_ULOCK 0
#define F_LOCK 1
#define F_TLOCK 2
#define F_TEST 3
#endif

static int lock_command_table[] = {
  F_ULOCK, F_LOCK, F_TLOCK, F_TEST
};

value unix_lockf(fd, cmd, span)  /* ML */
     value fd, cmd, span;
{
  if (lockf(Int_val(fd), lock_command_table[Tag_val(cmd)], Long_val(span))
      == -1) uerror("lockf", Nothing);
  return Atom(0);
}

#else

#include <errno.h>
#include <fcntl.h>

#ifdef F_SETLK

value unix_lockf(fd, cmd, span)  /* ML */
     value fd, cmd, span;
{
  struct flock l;
  int ret;
  int fildes;
  long size;

  fildes = Int_val(fd);
  size = Long_val(span);
  l.l_whence = 1;
  if (size < 0) {
    l.l_start = size;
    l.l_len = -size;
  } else {
    l.l_start = 0L;
    l.l_len = size;
  }
  switch (Tag_val(cmd)) {
  case 0: /* F_ULOCK */
    l.l_type = F_UNLCK;
    ret = fcntl(fildes, F_SETLK, &l);
    break;
  case 1: /* F_LOCK */
    l.l_type = F_WRLCK;
    ret = fcntl(fildes, F_SETLKW, &l);
    break;
  case 2: /* F_TLOCK */
    l.l_type = F_WRLCK;
    ret = fcntl(fildes, F_SETLK, &l);
    break;
  case 3: /* F_TEST */
    l.l_type = F_WRLCK;
    ret = fcntl(fildes, F_GETLK, &l);
    if (ret != -1) {
      if (l.l_type == F_UNLCK)
        ret = 0;
      else {
        errno = EACCES;
        ret = -1;
      }
    }
    break;
  default:
    errno = EINVAL;
    ret = -1;
  }
  if (ret == -1) uerror("lockf", Nothing);
  return Val_unit;
}

#else

value unix_lockf() { invalid_argument("lockf not implemented"); }

#endif
#endif
