/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdio.h>
#include "config.h"
#include "misc.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifdef DEBUG

void caml_failed_assert (char * expr, char * file, int line)
{
  fprintf (stderr, "file %s; line %d ### Assertion failed: %s\n",
           file, line, expr);
  fflush (stderr);
  exit (100);
}

#endif

int verb_gc;

void gc_message (int level, char *msg, unsigned long arg)
{
  if (level < 0 || (verb_gc & level) != 0){
#ifdef HAS_UI
    ui_print_stderr(msg, (void *) arg);
#else
    fprintf (stderr, msg, arg);
    fflush (stderr);
#endif
  }
}

void fatal_error (char *msg)
{
#ifdef HAS_UI
  ui_print_stderr("%s", msg);
  ui_exit (2);
#else
  fprintf (stderr, "%s", msg);
  exit(2);
#endif
}

void fatal_error_arg (char *fmt, char *arg)
{
#ifdef HAS_UI
  ui_print_stderr(fmt, arg);
  ui_exit (2);
#else
  fprintf (stderr, fmt, arg);
  exit(2);
#endif
}

char *aligned_malloc (asize_t size, int modulo, void **block)
{
  char *raw_mem;
  unsigned long aligned_mem;
                                                  Assert (modulo < Page_size);
  raw_mem = (char *) malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  *block = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
#ifdef DEBUG
  {
    unsigned long *p;
    unsigned long *p0 = (void *) *block,
                  *p1 = (void *) (aligned_mem - modulo),
                  *p2 = (void *) (aligned_mem - modulo + size),
                  *p3 = (void *) ((char *) *block + size + Page_size);

    for (p = p0; p < p1; p++) *p = Debug_filler_align;
    for (p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (p = p2; p < p3; p++) *p = Debug_filler_align;
  }
#endif
  return (char *) (aligned_mem - modulo);
}
