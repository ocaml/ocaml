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
#include <memory.h>

value gr_fill_rect(vx, vy, vw, vh)
     value vx, vy, vw, vh;
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int w = Int_val(vw);
  int h = Int_val(vh);

  XFillRectangle(grdisplay, grwindow.win, grwindow.gc,
                 x, Wcvt(y) - h + 1, w, h);
  XFillRectangle(grdisplay, grbstore.win, grbstore.gc,
                 x, Bcvt(y) - h + 1, w, h);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_fill_poly(array)
     value array;
{
  XPoint * points;
  int npoints, i;

  npoints = Wosize_val(array);
  points = (XPoint *) stat_alloc(npoints * sizeof(XPoint));
  for (i = 0; i < npoints; i++) {
    points[i].x = Int_val(Field(Field(array, i), 0));
    points[i].y = Wcvt(Int_val(Field(Field(array, i), 1)));
  }
  XFillPolygon(grdisplay, grwindow.win, grwindow.gc, points,
               npoints, Complex, CoordModeOrigin);
  for (i = 0; i < npoints; i++) {
    points[i].y = WtoB(points[i].y);
  }
  XFillPolygon(grdisplay, grbstore.win, grbstore.gc, points,
               npoints, Complex, CoordModeOrigin);
  XFlush(grdisplay);
  stat_free((char *) points);
  return Val_unit;
}

value gr_fill_arc(argv, argc)
     int argc;
     value * argv;
{
  int x = Int_val(argv[0]);
  int y = Int_val(argv[1]);
  int rx = Int_val(argv[2]);
  int ry = Int_val(argv[3]);
  int a1 = Int_val(argv[4]);
  int a2 = Int_val(argv[5]);
  XFillArc(grdisplay, grwindow.win, grwindow.gc,
           x - rx, Wcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  XFillArc(grdisplay, grbstore.win, grbstore.gc,
           x - rx, Bcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  XFlush(grdisplay);
  return Val_unit;
}

