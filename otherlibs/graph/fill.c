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
#include <memory.h>

value gr_fill_rect(value vx, value vy, value vw, value vh)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int w = Int_val(vw);
  int h = Int_val(vh);

  gr_check_open();
  if(grremember_mode)
    XFillRectangle(grdisplay, grbstore.win, grbstore.gc,
                   x, Bcvt(y) - h + 1, w, h);
  if(grdisplay_mode) {
    XFillRectangle(grdisplay, grwindow.win, grwindow.gc,
		   x, Wcvt(y) - h + 1, w, h);
    XFlush(grdisplay);
  }
  return Val_unit;
}

value gr_fill_poly(value array)
{
  XPoint * points;
  int npoints, i;

  gr_check_open();
  npoints = Wosize_val(array);
  points = (XPoint *) stat_alloc(npoints * sizeof(XPoint));
  for (i = 0; i < npoints; i++) {
    points[i].x = Int_val(Field(Field(array, i), 0));
    points[i].y = Bcvt(Int_val(Field(Field(array, i), 1)));
  }
  if(grremember_mode)
    XFillPolygon(grdisplay, grbstore.win, grbstore.gc, points,
                 npoints, Complex, CoordModeOrigin);
  if(grdisplay_mode) {
    for (i = 0; i < npoints; i++)
      points[i].y = BtoW(points[i].y);
    XFillPolygon(grdisplay, grwindow.win, grwindow.gc, points,
		 npoints, Complex, CoordModeOrigin);
    XFlush(grdisplay);
  }
  stat_free((char *) points);
  return Val_unit;
}

value gr_fill_arc_nat(value vx, value vy, value vrx, value vry, value va1, value va2)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int rx = Int_val(vrx);
  int ry = Int_val(vry);
  int a1 = Int_val(va1);
  int a2 = Int_val(va2);

  gr_check_open();
  if(grremember_mode)
    XFillArc(grdisplay, grbstore.win, grbstore.gc,
             x - rx, Bcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  if(grdisplay_mode) {
    XFillArc(grdisplay, grwindow.win, grwindow.gc,
	     x - rx, Wcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
    XFlush(grdisplay);
  }
  return Val_unit;
}

value gr_fill_arc(value *argv, int argc)
{
  return gr_fill_arc_nat(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
