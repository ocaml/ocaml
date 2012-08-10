/*************************************************************************/
/*                                                                       */
/*                         OCaml LablTk library                          */
/*                                                                       */
/*            Jacques Garrigue, Kyoto University RIMS                    */
/*                                                                       */
/*   Copyright 2001 Institut National de Recherche en Informatique et    */
/*   en Automatique and Kyoto University.  All rights reserved.          */
/*   This file is distributed under the terms of the GNU Library         */
/*   General Public License, with the special exception on linking       */
/*   described in file ../../../LICENSE.                                 */
/*                                                                       */
/*************************************************************************/

/* $Id$ */

#include <windows.h>
#include <mlvalues.h>
#include <callback.h>
#include <sys.h>

extern int __argc;
extern char **__argv;
extern void caml_expand_command_line(int * argcp, char *** argvp);
extern void caml_main (char **);

int WINAPI WinMain(HINSTANCE h, HINSTANCE HPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow)
{
  caml_expand_command_line(&__argc, &__argv);
  caml_main(__argv);
  sys_exit(Val_int(0));
  return 0;
}
