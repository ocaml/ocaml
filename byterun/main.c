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

extern int caml_main P((int, char **));

int main(argc, argv)
     int argc;
     char ** argv;
{
  return caml_main(argc, argv);
}
