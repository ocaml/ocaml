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

#include <process.h>

char * runtime_name = "cslrun.exe";
char * errmsg = "Cannot find cslrun.exe\n";

int main(argc, argv)
     int argc;
     char ** argv;
{
  int retcode;
  retcode = spawnvp(P_WAIT, runtime_name, argv);
  /* We use P_WAIT instead of P_OVERLAY here because under NT,
     P_OVERLAY returns to the command interpreter, displaying the prompt
     before executing the command. */
  if (retcode == -1) {
    write(2, errmsg, strlen(errmsg));
    return 2;
  }
  return retcode;
}
