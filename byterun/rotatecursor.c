/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <CursorCtl.h>
#include <MacTypes.h>
#include <stdlib.h>
#include <Timer.h>

#include "rotatecursor.h"

typedef struct {
  TMTask t;
  int volatile *p1;
  int volatile *p2;
} Xtmtask;

int volatile rotatecursor_flag = 1;
static int rotatecursor_inited = 0;
static int rotatecursor_period = 50;
static Xtmtask rotatecursor_tmtask;


#if GENERATINGCFM

static void rotatecursor_timerproc (Xtmtask *p)
{
  if (p->p1 != NULL && *(p->p1) == 0) *(p->p1) = 1;
  if (p->p2 != NULL && *(p->p2) == 0) *(p->p2) = 1;
}

#else

extern Xtmtask *getparam() ONEWORDINLINE(0x2009);  /* MOVE.L A1, D0 */

static void rotatecursor_timerproc (void)
{
  register Xtmtask *p = getparam ();

  if (p->p1 != NULL && *(p->p1) == 0) *(p->p1) = 1;
  if (p->p2 != NULL && *(p->p2) == 0) *(p->p2) = 1;
}

#endif /* GENERATINGCFM */


static void rotatecursor_remove_task (void)
{
  RmvTime ((QElemPtr) &rotatecursor_tmtask);
}

static void rotatecursor_init (void)
{
  if (rotatecursor_inited) return;

  InitCursorCtl (NULL);

  rotatecursor_tmtask.t.tmAddr = NewTimerProc (rotatecursor_timerproc);
  rotatecursor_tmtask.t.tmWakeUp = 0;
  rotatecursor_tmtask.t.tmReserved = 0;
  rotatecursor_tmtask.p1 = NULL;
  rotatecursor_tmtask.p2 = &rotatecursor_flag;

  InsTime ((QElemPtr) &rotatecursor_tmtask);
  PrimeTime ((QElemPtr) &rotatecursor_tmtask, 1);

  atexit (rotatecursor_remove_task);

  rotatecursor_inited = 1;
}

void rotatecursor_options (int volatile *p1, int period)
{
  if (!rotatecursor_inited) rotatecursor_init ();
  
  rotatecursor_tmtask.p1 = p1;
  rotatecursor_period = period;
}

int rotatecursor_action (int reverse)
{
  if (!rotatecursor_inited) rotatecursor_init ();

  rotatecursor_flag = 0;

  PrimeTime ((QElemPtr) &rotatecursor_tmtask, rotatecursor_period);

  RotateCursor (reverse ? -32 : 32);

  return 0;
}
