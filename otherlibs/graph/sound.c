/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"

value gr_sound(vfreq, vdur)
     value vfreq, vdur;
{
  XKeyboardControl kbdcontrol;

  gr_check_open();
  kbdcontrol.bell_pitch = Int_val(vfreq);
  kbdcontrol.bell_duration = Int_val(vdur);
  XChangeKeyboardControl(grdisplay, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XBell(grdisplay, 0);
  kbdcontrol.bell_pitch = -1;   /* restore default value */
  kbdcontrol.bell_duration = -1; /* restore default value */
  XChangeKeyboardControl(grdisplay, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XFlush(grdisplay);
  return Val_unit;
}


