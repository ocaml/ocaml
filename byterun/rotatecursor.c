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

/* rotatecursor library, written by <Damien.Doligez@inria.fr>
   This file is in the public domain.

   version 1.13
   
   See rotatecursor.h for documentation.
*/

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
static pascal void (*rotatecursor_action) (long) = &RotateCursor;


#if GENERATINGCFM

static void rotatecursor_timerproc (Xtmtask *p)
{
  if (p->p1 != NULL && *(p->p1) == 0) *(p->p1) = 1;
  if (p->p2 != NULL && *(p->p2) == 0) *(p->p2) = 1;
}

#else /* GENERATINGCFM */

extern Xtmtask *getparam() ONEWORDINLINE(0x2009);  /* MOVE.L A1, D0 */

static void rotatecursor_timerproc (void)
{
  register Xtmtask *p = getparam ();

  if (p->p1 != NULL && *(p->p1) == 0) *(p->p1) = 1;
  if (p->p2 != NULL && *(p->p2) == 0) *(p->p2) = 1;
}

#endif /* else GENERATINGCFM */


void rotatecursor_final (void)
{
  if (rotatecursor_inited){
    RmvTime ((QElemPtr) &rotatecursor_tmtask);
    rotatecursor_flag = 1;
    rotatecursor_inited = 0;
  }
}

static void rotatecursor_init (void)
{
  if (rotatecursor_inited) return;

  rotatecursor_tmtask.t.tmAddr = NewTimerProc (rotatecursor_timerproc);
  rotatecursor_tmtask.t.tmCount = 0;
  rotatecursor_tmtask.t.tmWakeUp = 0;
  rotatecursor_tmtask.t.tmReserved = 0;
  rotatecursor_tmtask.p1 = NULL;
  rotatecursor_tmtask.p2 = &rotatecursor_flag;

  InsTime ((QElemPtr) &rotatecursor_tmtask);
  atexit (rotatecursor_final);
  rotatecursor_flag = 1;

  rotatecursor_inited = 1;
}

void rotatecursor_options (int volatile *p1, int period, pascal void (*f) (long))
{
  if (!rotatecursor_inited) rotatecursor_init ();

  rotatecursor_tmtask.p1 = p1;
  if (p1 != NULL && *p1 == 0) *p1 = rotatecursor_flag;
  rotatecursor_period = (period == 0) ? 50 : period;
  rotatecursor_action = (f == NULL) ? &RotateCursor : f;
}

int rotatecursor_rearm (void)
{
  if (!rotatecursor_inited) rotatecursor_init ();
  
  rotatecursor_flag = 0;
  PrimeTime ((QElemPtr) &rotatecursor_tmtask, rotatecursor_period);
  return 0;
}

int rotatecursor_ticker (void)
{
  if (!rotatecursor_inited) rotatecursor_init ();

  rotatecursor_rearm ();
  (*rotatecursor_action) (32);
  return 0;
}
