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

/* Print an uncaught exception and abort */

#include <stdio.h>
#include <stdlib.h>
#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifdef HAS_UI
#define errprintf1(fmt) ui_print_stderr(fmt, NULL)
#define errprintf2(fmt,arg) ui_print_stderr(fmt, (char *)(arg))
#else
#define errprintf1(fmt) fprintf(stderr, fmt)
#define errprintf2(fmt,arg) fprintf(stderr, fmt, arg)
#endif

void fatal_uncaught_exception(exn)
     value exn;
{
  mlsize_t i;
  value v;

  errprintf2("Fatal error: uncaught exception %s",
             String_val(Field(Field(exn, 0), 0)));
  if (Wosize_val(exn) >= 2) {
    errprintf1("(");
    for (i = 1; i < Wosize_val(exn); i++) {
      if (i > 1) errprintf1(", ");
      v = Field(exn, i);
      if (Is_long(v))
        errprintf2("%ld", Long_val(v));
      else if (Tag_val(v) == String_tag)
        errprintf2("\"%s\"", String_val(v));
      else
        errprintf1("_");
    }
    errprintf1(")");
  }
  errprintf1("\n");
  exit(2);
}

