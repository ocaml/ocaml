/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"
#include "image.h"
#include <alloc.h>

static void gr_free_image(im)
     value im;
{
  XFreePixmap(grdisplay, Data_im(im));
  if (Mask_im(im) != None) XFreePixmap(grdisplay, Mask_im(im));
}

#define Max_image_mem 1000000

value gr_new_image(w, h)
     int w, h;
{
  value res = alloc_final(Grimage_wosize, gr_free_image, w*h, Max_image_mem);
  Width_im(res) = w;
  Height_im(res) = h;
  Data_im(res) = XCreatePixmap(grdisplay, grwindow.win, w, h, 
                               XDefaultDepth(grdisplay, grscreen));
  Mask_im(res) = None;
  return res;
}

value gr_create_image(vw, vh)
     value vw, vh;
{
  gr_check_open();
  return gr_new_image(Int_val(vw), Int_val(vh));
}

value gr_blit_image(im, vx, vy)
     value im, vx, vy;
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

value gr_draw_image(im, vx, vy)
     value im, vx, vy;
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int wy = Wcvt(y) + 1 - Height_im(im);
  int by = Bcvt(y) + 1 - Height_im(im);

  gr_check_open();
  if (Mask_im(im) != None) {
    XSetClipOrigin(grdisplay, grwindow.gc, x, wy);
    XSetClipMask(grdisplay, grwindow.gc, Mask_im(im));
    XSetClipOrigin(grdisplay, grbstore.gc, x, by);
    XSetClipMask(grdisplay, grbstore.gc, Mask_im(im));
  }
  XCopyArea(grdisplay, Data_im(im), grwindow.win, grwindow.gc,
            0, 0,
            Width_im(im), Height_im(im),
            x, wy);
  XCopyArea(grdisplay, Data_im(im), grbstore.win, grbstore.gc,
            0, 0,
            Width_im(im), Height_im(im),
            x, by);
  if (Mask_im(im) != None) {
    XSetClipMask(grdisplay, grwindow.gc, None);
    XSetClipMask(grdisplay, grbstore.gc, None);
  }
  XFlush(grdisplay);
  return Val_unit;
}


            
