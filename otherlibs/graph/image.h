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

struct grimage {
  final_fun f;                  /* Finalization function */
  int width, height;            /* Dimensions of the image */
  Pixmap data;                  /* Pixels */
  Pixmap mask;                  /* Mask for transparent points, or None */
};

#define Grimage_wosize \
  ((sizeof(struct grimage) + sizeof(value) - 1) / sizeof(value))

#define Width_im(i) (((struct grimage *)(i))->width)
#define Height_im(i) (((struct grimage *)(i))->height)
#define Data_im(i) (((struct grimage *)(i))->data)
#define Mask_im(i) (((struct grimage *)(i))->mask)

#define Transparent (-1)

value gr_new_image(int w, int h);
