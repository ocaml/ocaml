/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <Events.h>
#include <Processes.h>
#include <Resources.h>
#include <TextUtils.h>

#include <errno.h>
#include <string.h>
#include <time.h>

#include <alloc.h>
#include <mlvalues.h>
#include <ui.h>

#include "unixsupport.h"


static unsigned long start_ticks;

value macosunix_startup (value unit)  /* ML */
{
  start_ticks = TickCount ();

  return Val_unit;
}

#if 0
void _exit (int status);
value unix_exit (value n)             /* ML */
{
  ui_exit (Int_val (n));
}
#endif

value unix_getlogin (void)            /* ML */
{
  char **hs = (char **) GetString (-16096);
  if (hs == NULL || *hs == NULL || strlen (*hs) == 0){
    unix_error (ENOENT, "getlogin", Nothing);
  }
  return copy_string (*hs);
}

value unix_getpid (void)              /* ML */
{
  ProcessSerialNumber psn;
  
  GetCurrentProcess (&psn);
  return Val_long (psn.lowLongOfPSN);
}

value unix_time (void)                /* ML */
{
  return copy_double (time (NULL) - 2082844800.);
}

value unix_times (void)               /* ML */
{
  value res;

  res = alloc_small(4 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, (double) (TickCount () - start_ticks) / 60);
  Store_double_field(res, 1, (double) 0.0);
  Store_double_field(res, 2, (double) 0.0);
  Store_double_field(res, 3, (double) 0.0);
  return res;
}

#define Unimplemented(f, args) \
  value unix_##f args { invalid_argument (#f " not implemented"); }

Unimplemented (chown, (value path, value uid, value gid))
Unimplemented (chroot, (value path))
Unimplemented (environment, (void))
Unimplemented (execv, (value path, value args))
Unimplemented (execve, (value path, value args, value env))
Unimplemented (execvp, (value path, value args))
Unimplemented (execvpe, (value path, value args, value env))
Unimplemented (fork, (value unit))
Unimplemented (getegid, (void))
Unimplemented (geteuid, (void))
Unimplemented (getgid, (void))
Unimplemented (getgrnam, (value name))
Unimplemented (getgrgid, (value gid))
Unimplemented (getppid, (void))
Unimplemented (getpwnam, (value name))
Unimplemented (getpwuid, (value uid))
Unimplemented (getuid, (void))
Unimplemented (kill, (value pid, value signal))
Unimplemented (link, (value path1, value path2))
Unimplemented (mkfifo, (value path, value mode))
Unimplemented (nice, (value incr))
Unimplemented (setgid, (value gid))
Unimplemented (setuid, (value uid))
Unimplemented (umask, (value perm))
Unimplemented (wait, (void))
Unimplemented (waitpid, (value flags, value pid_req))
