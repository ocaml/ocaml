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
#include <alloc.h>

value gr_plot(vx, vy)
     value vx, vy;
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XDrawPoint(grdisplay, grwindow.win, grwindow.gc, x, Wcvt(y));
  XDrawPoint(grdisplay, grbstore.win, grbstore.gc, x, Bcvt(y));
  XFlush(grdisplay);
  return Val_unit;
}

value gr_moveto(vx, vy)
     value vx, vy;
{
  grx = Int_val(vx);
  gry = Int_val(vy);
  return Val_unit;
}

value gr_current_point()
{
  value res;
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(grx);
  Field(res, 1) = Val_int(gry);
  return res;
}

value gr_lineto(vx, vy)
     value vx, vy;
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XDrawLine(grdisplay, grwindow.win, grwindow.gc,
            grx, Wcvt(gry), x, Wcvt(y));
  XDrawLine(grdisplay, grbstore.win, grbstore.gc,
            grx, Bcvt(gry), x, Bcvt(y));
  grx = x;
  gry = y;
  XFlush(grdisplay);
  return Val_unit;
}

value gr_draw_arc(argv, argc)
     int argc;
     value * argv;
{
  int x = Int_val(argv[0]);
  int y = Int_val(argv[1]);
  int rx = Int_val(argv[2]);
  int ry = Int_val(argv[3]);
  int a1 = Int_val(argv[4]);
  int a2 = Int_val(argv[5]);
  XDrawArc(grdisplay, grwindow.win, grwindow.gc,
           x - rx, Wcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  XDrawArc(grdisplay, grbstore.win, grbstore.gc,
           x - rx, Bcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_set_line_width(vwidth)
     value vwidth;
{
  int width = Int_val(vwidth);
  XSetLineAttributes(grdisplay, grwindow.gc,
                     width, LineSolid, CapRound, JoinRound);
  XSetLineAttributes(grdisplay, grbstore.gc,
                     width, LineSolid, CapRound, JoinRound);
  return Val_unit;
}

