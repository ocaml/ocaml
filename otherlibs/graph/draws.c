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
#include <memory.h>

/* Convert an array of points (represented as pairs of integers) into
   a C array of points for X. */
XPoint * make_XPoints (value pts, int npoints)
{
  XPoint *points;
  int i;
  points = (XPoint *) stat_alloc(npoints * sizeof(XPoint));
  for (i = 0; i < npoints; i++) {
    points[i].x = Int_val(Field(Field(pts, i), 0));
    points[i].y = Bcvt(Int_val(Field(Field(pts, i), 1)));
  };
  return (points);
}

/* Convert an array of points (represented as pairs of integers) into
   a path for X (an array of points with identical first and
   last points). The path could be closed or not depending on the
   value of to_be_closed. */
XPoint *make_XPathGen (value pts, int npoints, int *pathlen, int to_be_closed)
{
  XPoint *points;
  int i;

  if(npoints > 1) {
    int x1, y1, x2, y2;
    x1 = Int_val(Field(Field(pts, 0), 0));
    y1 = Bcvt(Int_val(Field(Field(pts, 0), 1)));
    x2 = Int_val(Field(Field(pts, (npoints - 1)), 0));
    y2 = Bcvt(Int_val(Field(Field(pts, (npoints - 1)), 1)));
    if((x1 == x2) && (y1 == y2)) {
      to_be_closed = 0;
    }
  }
  *pathlen = npoints + to_be_closed;
  points = (XPoint *) stat_alloc(*pathlen * sizeof(XPoint));

  for (i = 0; i < npoints; i++) {
    points[i].x = Int_val(Field(Field(pts, i), 0));
    points[i].y = Bcvt(Int_val(Field(Field(pts, i), 1)));
  };
  if(to_be_closed == 1) {
    points[npoints].x = Int_val(Field(Field(pts, 0), 0));
    points[npoints].y = Bcvt(Int_val(Field(Field(pts, 0), 1)));
  }
  return (points);
}

/* Convert an array of points (represented as pairs of integers) into
   a path for X (an array of points with identical first and
   last points). */
XPoint *make_XPath (value pts, int npoints, int *pathlen)
{
  return (make_XPathGen (pts, npoints, pathlen, 0));
}

/* Convert an array of points (represented as pairs of integers) into
   a closed path for X (an array of points with identical first and
   last points). */
XPoint *make_XClosedPath (value pts, int npoints, int *pathlen)
{
  return (make_XPathGen (pts, npoints, pathlen, 1));
}

value gr_plots(value pts)
{
  XPoint *points;
  int npoints;

  gr_check_open();
  npoints = Wosize_val(pts);
  points = make_XPoints (pts, npoints);

  if(grremember_mode)
    XDrawPoints(grdisplay, grbstore.win, grbstore.gc,
     points, npoints, CoordModeOrigin);
  if(grdisplay_mode) {
    XDrawPoints(grdisplay, grwindow.win, grwindow.gc,
     points, npoints, CoordModeOrigin);
    XFlush(grdisplay);
  }
  return Val_unit;
}

value gr_draw_poly(value pts)

{
  XPoint *points;
  int npoints;

  gr_check_open();
  npoints = Wosize_val(pts);
  points = make_XClosedPath(pts, npoints, &npoints);

  if(grremember_mode)
    XDrawLines(grdisplay, grbstore.win, grbstore.gc,
     points, npoints, CoordModeOrigin);
  if(grdisplay_mode) {
    XDrawLines(grdisplay, grwindow.win, grwindow.gc,
     points, npoints, CoordModeOrigin);
    XFlush(grdisplay);
  }
  return Val_unit;
}

value gr_draw_poly_line(value pts)

{
  XPoint *points;
  int npoints;

  gr_check_open();
  npoints = Wosize_val(pts);
  points = make_XPath(pts, npoints, &npoints);

  if(grremember_mode)
    XDrawLines(grdisplay, grbstore.win, grbstore.gc,
     points, npoints, CoordModeOrigin);
  if(grdisplay_mode) {
    XDrawLines(grdisplay, grwindow.win, grwindow.gc,
     points, npoints, CoordModeOrigin);
    XFlush(grdisplay);
  }
  return Val_unit;
}

XSegment * make_XSegments (value segs, int nsegments)
{
  XSegment *segments;
  int i;
  segments = (XSegment *) stat_alloc(nsegments * sizeof(XSegment));
  for (i = 0; i < nsegments; i++) {
    segments[i].x1 = Int_val(Field(Field(segs, i), 0));
    segments[i].y1 = Bcvt(Int_val(Field(Field(segs, i), 1)));
    segments[i].x2 = Int_val(Field(Field(segs, i), 2));
    segments[i].y2 = Bcvt(Int_val(Field(Field(segs, i), 3)));
  };
  return (segments);
}

value gr_draw_segments(value segs)
{
  XSegment *segments;
  int nsegments;

  gr_check_open();
  nsegments = Wosize_val(segs);

  segments = make_XSegments(segs, nsegments);

  if(grremember_mode)
    XDrawSegments(grdisplay, grbstore.win, grbstore.gc,
     segments, nsegments);
  if(grdisplay_mode) {
    XDrawSegments(grdisplay, grwindow.win, grwindow.gc,
     segments, nsegments);
    XFlush(grdisplay);
  }
  return Val_unit;
}


