/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*              Jun Furuse, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"

value gr_open_subwindow(value vx, value vy, value width, value height)
{
  Window win;

  int h = Int_val(height);
  int w = Int_val(width);
  int x = Int_val(vx);
  int y = Int_val(vy);

  gr_check_open();
  win = XCreateSimpleWindow(grdisplay, grwindow.win,
                            x, Wcvt(y + h), w, h,
                            0, grblack, grbackground);
  XMapWindow(grdisplay, win);
  XFlush(grdisplay);
  return (id_of_window (win));
}

value gr_close_subwindow(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XDestroyWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}
