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

static value gr_alloc_int_vect(mlsize_t size)
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

value gr_dump_image(value image)
{
  int width, height, i, j;
  XImage * idata, * imask;
  value m = Val_unit;

  Begin_roots2(image, m);
    gr_check_open();
    width = Width_im(image);
    height = Height_im(image);
    m = gr_alloc_int_vect(height);
    for (i = 0; i < height; i++) {
      value v = gr_alloc_int_vect(width);
      modify(&Field(m, i), v);
    }

    idata =
      XGetImage(grdisplay, Data_im(image), 0, 0, width, height, (-1), ZPixmap);
    for (i = 0; i < height; i++)
      for (j = 0; j < width; j++)
	Field(Field(m, i), j) = Val_int(gr_rgb_pixel(XGetPixel(idata, j, i)));
    XDestroyImage(idata);

    if (Mask_im(image) != None) {
      imask =
	XGetImage(grdisplay, Mask_im(image), 0, 0, width, height, 1, ZPixmap);
      for (i = 0; i < height; i++)
	for (j = 0; j < width; j++)
	  if (XGetPixel(imask, j, i) == 0)
	    Field(Field(m, i), j) = Val_int(Transparent);
      XDestroyImage(imask);
    }
  End_roots();
  return m;
}


    
