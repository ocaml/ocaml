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
#include <memory.h>

value gr_make_image(m)
     value m;
{
  int width, height;
  value im;
  Bool has_transp;
  XImage * idata, * imask;
  char * bdata, * bmask;
  int i, j, rgb;
  value line;
  GC gc;

  gr_check_open();
  height = Wosize_val(m);
  if (height == 0) return gr_new_image(0, 0);
  width = Wosize_val(Field(m, 0));
  for (i = 1; i < height; i++)
    if (Wosize_val(Field(m, i)) != width)
      gr_fail("make_image: lines of different lengths", NULL);

  /* Build an XImage for the data part of the image */
  idata =
    XCreateImage(grdisplay, DefaultVisual(grdisplay, grscreen),
                 XDefaultDepth(grdisplay, grscreen),
                 ZPixmap, 0, NULL, width, height,
                 BitmapPad(grdisplay), 0);
  bdata = (char *) stat_alloc(height * idata->bytes_per_line);
  idata->data = bdata;
  has_transp = False;

  for (i = 0; i < height; i++) {
    line = Field(m, i);
    for (j = 0; j < width; j++) {
      rgb = Int_val(Field(line, j));
      if (rgb == Transparent) { has_transp = True; rgb = 0; }
      XPutPixel(idata, j, i, gr_pixel_rgb(rgb));
    }
  }

  /* If the matrix contains transparent points,
     build an XImage for the mask part of the image */
  if (has_transp) {
    imask =
      XCreateImage(grdisplay, DefaultVisual(grdisplay, grscreen),
                   1, ZPixmap, 0, NULL, width, height,
                   BitmapPad(grdisplay), 0);
    bmask = (char *) stat_alloc(height * imask->bytes_per_line);
    imask->data = bmask;

    for (i = 0; i < height; i++) {
      line = Field(m, i);
      for (j = 0; j < width; j++) {
        rgb = Int_val(Field(line, j));
        XPutPixel(imask, j, i, rgb != Transparent);
      }
    }
  } else {
    imask = NULL;
  }

  /* Allocate the image and store the XImages into the Pixmaps */
  im = gr_new_image(width, height);
  gc = XCreateGC(grdisplay, Data_im(im), 0, NULL);
  XPutImage(grdisplay, Data_im(im), gc, idata, 0, 0, 0, 0, width, height);
  XDestroyImage(idata);
  XFreeGC(grdisplay, gc);
  if (has_transp) {
    Mask_im(im) = XCreatePixmap(grdisplay, grwindow.win, width, height, 1);
    gc = XCreateGC(grdisplay, Mask_im(im), 0, NULL);
    XPutImage(grdisplay, Mask_im(im), gc, imask, 0, 0, 0, 0, width, height);
    XDestroyImage(imask);
    XFreeGC(grdisplay, gc);
  }
  XFlush(grdisplay);
  return im;
}

