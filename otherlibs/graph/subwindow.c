/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*              Jun Furuse, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"

value gr_open_subwindow(value x, value y, value width, value height)
{
  Window win;

  gr_check_open();
  win = XCreateSimpleWindow(grdisplay, grwindow.win,
                            Int_val(x), Int_val(y), 
                            Int_val(width), Int_val(height),
                            0, grblack, grbackground);
  XMapWindow(grdisplay, win);
  XFlush(grdisplay);
  return (id_of_window ( win ));
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
