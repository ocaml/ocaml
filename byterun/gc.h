/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _gc_
#define _gc_


#include "mlvalues.h"

#define Caml_white (0 << 8)
#define Caml_gray  (1 << 8)
#define Caml_blue  (2 << 8)
#define Caml_black (3 << 8)

#define Color_hd(hd) ((color_t) ((hd) & Caml_black))
#define Color_hp(hp) Color_hd (Hd_hp (hp))

#define Is_white_hd(hd) (Color_hd (hd) == Caml_white)
#define Is_gray_hd(hd) (Color_hd (hd) == Caml_gray)
#define Is_blue_hd(hd) (Color_hd (hd) == Caml_blue)
#define Is_black_hd(hd) (Color_hd (hd) == Caml_black)

#define Whitehd_hd(hd) (((hd)  & ~Caml_black)/*| Caml_white*/)
#define Grayhd_hd(hd)  (((hd)  & ~Caml_black)  | Caml_gray)
#define Blackhd_hd(hd) (((hd)/*& ~Caml_black*/)| Caml_black)
#define Bluehd_hd(hd)  (((hd)  & ~Caml_black)  | Caml_blue)

/* This depends on the layout of the header.  See [mlvalues.h]. */
#define Make_header(wosize, tag, color)                                       \
       ((header_t) (((header_t) (wosize) << 10)                               \
                    + (color)                                                 \
                    + (tag_t) (tag)))

#define Color_val(val) (Color_hd (Hd_val (val)))

#define Is_white_val(val) (Color_val(val) == Caml_white)
#define Is_gray_val(val) (Color_val(val) == Caml_gray)
#define Is_blue_val(val) (Color_val(val) == Caml_blue)
#define Is_black_val(val) (Color_val(val) == Caml_black)


#endif /* _gc_ */
