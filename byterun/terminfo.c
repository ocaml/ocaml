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

extern int tgetent (char * buffer, char * name);
extern int tgetstr (char * id, char ** area);
extern int tgetnum (char * id);
extern int tputs (char * str, int count, int (*outchar)(int c));

value terminfo_setup(value unit)      /* ML */
{
  static char buffer[1024];
  if (tgetent(buffer, getenv("TERM")) != 1) failwith("Terminfo.setupterm");
  return Val_unit;
}

value terminfo_getstr(value capa)     /* ML */
{
  char buff[1024];
  char * p = buff;
  if (tgetstr(String_val(capa), &p) == 0) raise_not_found();
  return copy_string(buff);
}

value terminfo_getnum(value capa)     /* ML */
{
  int res = tgetnum(String_val(capa));
  if (res == -1) raise_not_found();
  return Val_int(res);
}

static struct channel * terminfo_putc_channel;

static int terminfo_putc(int c)
{
  putch(terminfo_putc_channel, c);
  return c;
}

value terminfo_puts(value vchan, value str, value count) /* ML */
{
  terminfo_putc_channel = Channel(vchan);
  tputs(String_val(str), Int_val(count), terminfo_putc);
  return Val_unit;
}

#else

value terminfo_setup(value unit)
{
  failwith("Terminfo.setupterm");
  return Val_unit;
}

value terminfo_getstr(value capa)
{
  raise_not_found();
  return Val_unit;
}

value terminfo_getnum(value capa)
{
  raise_not_found();
  return Val_unit;
}

value terminfo_puts(value vchan, value str, value count)
{
  invalid_argument("Terminfo.puts");
  return Val_unit;
}

#endif
