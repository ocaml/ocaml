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
#include "image.h"
#include <alloc.h>

static void gr_free_image(value im)
{
  XFreePixmap(grdisplay, Data_im(im));
  if (Mask_im(im) != None) XFreePixmap(grdisplay, Mask_im(im));
}

#define Max_image_mem 1000000

value gr_new_image(int w, int h)
{
  value res = alloc_final(Grimage_wosize, gr_free_image, w*h, Max_image_mem);
  Width_im(res) = w;
  Height_im(res) = h;
  Data_im(res) = XCreatePixmap(grdisplay, grwindow.win, w, h, 
                               XDefaultDepth(grdisplay, grscreen));
  Mask_im(res) = None;
  return res;
}

value gr_create_image(value vw, value vh)
{
  gr_check_open();
  return gr_new_image(Int_val(vw), Int_val(vh));
}

value gr_blit_image(value im, value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  gr_check_open();
  XCopyArea(grdisplay, grbstore.win, Data_im(im), grbstore.gc,
            x, Bcvt(y) + 1 - Height_im(im),
            Width_im(im), Height_im(im),
            0, 0);
  return Val_unit;
}

value gr_draw_image(value im, value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int wy = Wcvt(y) + 1 - Height_im(im);
  int by = Bcvt(y) + 1 - Height_im(im);

  gr_check_open();
  if (Mask_im(im) != None) {
    XSetClipOrigin(grdisplay, grbstore.gc, x, by);
    XSetClipMask(grdisplay, grbstore.gc, Mask_im(im));
    if(grautoflush) {
      XSetClipOrigin(grdisplay, grwindow.gc, x, wy);
      XSetClipMask(grdisplay, grwindow.gc, Mask_im(im));
    }
  }
  XCopyArea(grdisplay, Data_im(im), grbstore.win, grbstore.gc,
            0, 0,
            Width_im(im), Height_im(im),
            x, by);
  if(grautoflush)
    XCopyArea(grdisplay, Data_im(im), grwindow.win, grwindow.gc,
	      0, 0,
	      Width_im(im), Height_im(im),
	      x, wy);
  if (Mask_im(im) != None) {
    XSetClipMask(grdisplay, grbstore.gc, None);
    if(grautoflush)
      XSetClipMask(grdisplay, grwindow.gc, None);
  }
  if(grautoflush)
    XFlush(grdisplay);
  return Val_unit;
}
