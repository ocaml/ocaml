/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"
#include <alloc.h>

value gr_plot(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  gr_check_open();
  XDrawPoint(grdisplay, grwindow.win, grwindow.gc, x, Wcvt(y));
  XDrawPoint(grdisplay, grbstore.win, grbstore.gc, x, Bcvt(y));
  XFlush(grdisplay);
  return Val_unit;
}

value gr_moveto(value vx, value vy)
{
  grx = Int_val(vx);
  gry = Int_val(vy);
  return Val_unit;
}

value gr_current_point(void)
{
  value res;
  res = alloc_small(2, 0);
  Field(res, 0) = Val_int(grx);
  Field(res, 1) = Val_int(gry);
  return res;
}

value gr_lineto(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  gr_check_open();
  XDrawLine(grdisplay, grwindow.win, grwindow.gc,
            grx, Wcvt(gry), x, Wcvt(y));
  XDrawLine(grdisplay, grbstore.win, grbstore.gc,
            grx, Bcvt(gry), x, Bcvt(y));
  grx = x;
  gry = y;
  XFlush(grdisplay);
  return Val_unit;
}

value gr_draw_arc_nat(value vx, value vy, value vrx, value vry, value va1, value va2)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int rx = Int_val(vrx);
  int ry = Int_val(vry);
  int a1 = Int_val(va1);
  int a2 = Int_val(va2);

  gr_check_open();
  XDrawArc(grdisplay, grwindow.win, grwindow.gc,
           x - rx, Wcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  XDrawArc(grdisplay, grbstore.win, grbstore.gc,
           x - rx, Bcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_draw_arc(value *argv, int argc)
{
  return gr_draw_arc_nat(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value gr_set_line_width(value vwidth)
{
  int width = Int_val(vwidth);

  gr_check_open();
  XSetLineAttributes(grdisplay, grwindow.gc,
                     width, LineSolid, CapRound, JoinRound);
  XSetLineAttributes(grdisplay, grbstore.gc,
                     width, LineSolid, CapRound, JoinRound);
  return Val_unit;
}

