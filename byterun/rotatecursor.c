/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Cursor rotation for MPW tools (ocamlrun and ocamlyacc) */

#include <CursorCtl.h>
#include <stdlib.h>
#include <Timer.h>
#include <Types.h>

#include "rotatecursor.h"

typedef struct {
  TMTask t;
  int volatile *p1;
  int volatile *p2;
} Xtmtask;

static Xtmtask mytmtask;


#if GENERATINGCFM

static void mytimerproc (Xtmtask *p)
{
  if (p->p1 != NULL && *(p->p1) == 0) *(p->p1) = 1;
  if (p->p2 != NULL && *(p->p2) == 0) *(p->p2) = 1;
}

#else

extern Xtmtask *getparam() ONEWORDINLINE(0x2009);  /* MOVE.L A1, D0 */

static void mytimerproc (void)
{
  register Xtmtask *p = getparam ();
  
  if (p->p1 != NULL && *(p->p1) == 0) *(p->p1) = 1;
  if (p->p2 != NULL && *(p->p2) == 0) *(p->p2) = 1;
}

#endif /* GENERATINGCFM */


static void remove_task (void)
{
  RmvTime ((QElemPtr) &mytmtask);
}

void rotatecursor_init (int volatile *p1, int volatile *p2)
{
  InitCursorCtl (NULL);
  mytmtask.t.tmAddr = NewTimerProc (mytimerproc);
  mytmtask.t.tmWakeUp = 0;
  mytmtask.t.tmReserved = 0;
  mytmtask.p1 = p1;
  mytmtask.p2 = p2;
  InsTime ((QElemPtr) &mytmtask);
  PrimeTime ((QElemPtr) &mytmtask, 1);
  atexit (remove_task);
}

int rotatecursor_action (int reverse)
{
  PrimeTime ((QElemPtr) &mytmtask, 50);     /* 20 Hz */
  RotateCursor (reverse ? -32 : 32);
  return 0;
}
