/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Read and output terminal commands */

#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "mlvalues.h"

#define Uninitialised Val_int(0)
#define Bad_term Val_int(1)
#define Good_term_tag 0

#ifdef HAS_TERMCAP

extern int tgetent (char * buffer, char * name);
extern char * tgetstr (char * id, char ** area);
extern int tgetnum (char * id);
extern int tputs (char * str, int count, int (*outchar)(int c));

static struct channel *chan;
static char area [1024];
static char *area_p = area;
static int num_lines;
static char *up = NULL;
static char *down = NULL;
static char *standout = NULL;
static char *standend = NULL;

value terminfo_setup (value vchan)      /* ML */
{
  value result;
  static char buffer[1024];

  chan = Channel (vchan);

  if (tgetent(buffer, getenv("TERM")) != 1) return Bad_term;

  num_lines = tgetnum ("li");
  up = tgetstr ("up", &area_p);
  down = tgetstr ("do", &area_p);
  standout = tgetstr ("us", &area_p);
  standend = tgetstr ("ue", &area_p);
  if (standout == NULL || standend == NULL){
    standout = tgetstr ("so", &area_p);
    standend = tgetstr ("se", &area_p);
  }
  Assert (area_p <= area + 1024);
  if (num_lines == -1 || up == NULL || down == NULL
      || standout == NULL || standend == NULL){
    return Bad_term;
  }
  result = alloc (1, Good_term_tag);
  Field (result, 0) = Val_int (num_lines);
  return result;
}

static int terminfo_putc (int c)
{
  putch (chan, c);
  return c;
}

value terminfo_backup (value lines)    /* ML */
{
  int i;

  for (i = 0; i < Int_val (lines); i++){
    tputs (up, 1, terminfo_putc);
  }
  return Val_unit;
}

value terminfo_standout (value start)  /* ML */
{
  tputs (Bool_val (start) ? standout : standend, 1, terminfo_putc);
  return Val_unit;
}

value terminfo_resume (value lines)    /* ML */
{
  int i;

  for (i = 0; i < Int_val (lines); i++){
    tputs (down, 1, terminfo_putc);
  }
  return Val_unit;
}

#else

value terminfo_setup (value unit)
{
  return Bad_term;
}

value terminfo_backup (value lines)
{
  invalid_argument("Terminfo.backup");
  return Val_unit;
}

value terminfo_standout (value start)
{
  invalid_argument("Terminfo.standout");
  return Val_unit;
}

value terminfo_resume (value lines)
{
  invalid_argument("Terminfo.resume");
  return Val_unit;
}

#endif
