/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* glue code for MPW tools */

#include <stdio.h>
#include <stdlib.h>

int ui_read (int fd, char *p, unsigned int n)
{
  return read (fd, p, n);
}

int ui_write (int fd, char *p, unsigned int n)
{
  return write (fd, p, n);
}

void ui_print_stderr (char *msg, void *arg)
{
  fprintf (stderr, msg, arg);
}

void ui_exit (int retcode)
{
  exit (retcode);
}
