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
#include <memory.h>

static value gr_alloc_int_vect(size)
     mlsize_t size;
{
  value res;
  mlsize_t i;
  
  if (size <= Max_young_wosize) {
    res = alloc(size, 0);
  } else {
    res = alloc_shr(size, 0);
  }
  for (i = 0; i < size; i++) {
    Field(res, i) = Val_long(0);
  }
  return res;
}

value gr_dump_image(image)
     value image;
{
  int width, height, i, j;
  XImage * idata, * imask;
  Push_roots(root, 2);

#define im root[0]
#define m root[1]

  gr_check_open();
  im = image;
  width = Width_im(im);
  height = Height_im(im);
  m = gr_alloc_int_vect(height);
  for (i = 0; i < height; i++) {
    value v = gr_alloc_int_vect(width);
    modify(&Field(m, i), v);
  }
  
  idata =
    XGetImage(grdisplay, Data_im(im), 0, 0, width, height, (-1), ZPixmap);
  for (i = 0; i < height; i++)
    for (j = 0; j < width; j++)
      Field(Field(m, i), j) = Val_int(gr_rgb_pixel(XGetPixel(idata, j, i)));
  XDestroyImage(idata);

  if (Mask_im(im) != None) {
    imask =
      XGetImage(grdisplay, Mask_im(im), 0, 0, width, height, 1, ZPixmap);
    for (i = 0; i < height; i++)
      for (j = 0; j < width; j++)
        if (XGetPixel(imask, j, i) == 0)
          Field(Field(m, i), j) = Val_int(Transparent);
    XDestroyImage(imask);
  }
  Pop_roots();
  return m;

#undef im
#undef m
}


    
