/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <wtypes.h>
#include <winbase.h>
#include <process.h>

char * runtime_name = "ocamlrun.exe";
char * errmsg = "Cannot find ocamlrun.exe\n";

int main(int argc, char ** argv)
{
  int retcode;
  char * cmdline = GetCommandLine();
  retcode = spawnlp(P_WAIT, runtime_name, cmdline, NULL);
  /* We use P_WAIT instead of P_OVERLAY here because under NT,
     P_OVERLAY returns to the command interpreter, displaying the prompt
     before executing the command. */
  if (retcode == -1) {
    write(2, errmsg, strlen(errmsg));
    return 2;
  }
  return retcode;
}

/* Prevent VC++ from linking its own _setargv function, which
   performs command-line processing (we don't need it) */

static void _setargv() { }
