/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Main entry point (can be overriden by a user-provided main()
   function that calls caml_main() later). */

#include "misc.h"
#include "mlvalues.h"
#include "sys.h"

extern void caml_main P((int, char **));


#ifdef _WIN32
extern void expand_command_line P((int *, char ***));
#endif


#if macintosh
#include "rotatecursor.h"
#include "signals.h"
int volatile have_to_interact = 0;
#endif


int main(argc, argv)
     int argc;
     char ** argv;
{
#ifdef _WIN32
  expand_command_line(&argc, &argv);
#endif
#if macintosh
  rotatecursor_init (&something_to_do, &have_to_interact);
#endif
  caml_main(argc, argv);
  sys_exit(Val_int(0));
  return 0; /* not reached */
}
