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

/* Read and output terminal commands */

#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "mlvalues.h"

#ifdef HAS_TERMCAP

#define _BSD /* For DEC OSF1 */
#undef getch
#include <curses.h>

value terminfo_setup(unit)      /* ML */
     value unit;
{
  static buffer[1024];
  if (tgetent(buffer, getenv("TERM")) != 1) failwith("Terminfo.setupterm");
  return Val_unit;
}

value terminfo_getstr(capa)     /* ML */
     value capa;
{
  char buff[1024];
  char * p = buff;
  if (tgetstr(String_val(capa), &p) == 0) raise_not_found();
  return copy_string(buff);
}

value terminfo_getnum(capa)     /* ML */
     value capa;
{
  int res = tgetnum(String_val(capa));
  if (res == -1) raise_not_found();
  return Val_int(res);
}

static struct channel * terminfo_putc_channel;

static int terminfo_putc(c)
     int c;
{
  putch(terminfo_putc_channel, c);
  return c;
}

value terminfo_puts(chan, str, count) /* ML */
     struct channel * chan;
     value str, count;
{
  terminfo_putc_channel = chan;
  tputs(String_val(str), Int_val(count), terminfo_putc);
  return Val_unit;
}

#else

value terminfo_setup(unit)
     value unit;
{
  failwith("Terminfo.setupterm");
  return Val_unit;
}

value terminfo_getstr(capa)
     value capa;
{
  raise_not_found();
  return Val_unit;
}

value terminfo_getnum(capa)
     value capa;
{
  raise_not_found();
  return Val_unit;
}

value terminfo_puts(chan, str, count)
     struct channel * chan;
     value str, count;
{
  invalid_argument("Terminfo.puts");
}

#endif
