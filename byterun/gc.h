#ifndef _gc_
#define _gc_


#include "mlvalues.h"

/* Defined in [major_gc.c]. */
extern unsigned free_mem_percent_min, free_mem_percent_max;

#define White (0 << 8)
#define Gray  (1 << 8)
#define Blue  (2 << 8)
#define Black (3 << 8)

#define Color_hd(hd) ((color_t) ((hd) & Black))
#define Color_hp(hp) Color_hd (Hd_hp (hp))

#define Is_white_hd(hd) (Color_hd (hd) == White)
#define Is_gray_hd(hd) (Color_hd (hd) == Gray)
#define Is_blue_hd(hd) (Color_hd (hd) == Blue)
#define Is_black_hd(hd) (Color_hd (hd) == Black)

#define Whitehd_hd(hd) ((hd) & ~Black)
#define Grayhd_hd(hd) (((hd) & ~Black) | Gray)
#define Blackhd_hd(hd) ((hd) | Black)
#define Bluehd_hd(hd) (((hd) & ~Black) | Blue)

/* This depends on the layout of the header.  See [mlvalues.h]. */
#define Make_header(wosize, tag, color)					      \
       ((header_t) (((header_t) (wosize) << 10)				      \
		    + (color)						      \
		    + (tag_t) (tag)))

#define Color_val(val) (Color_hd (Hd_val (val)))

#define Is_white_val(val) (Color_val(val) == White)
#define Is_gray_val(val) (Color_val(val) == Gray)
#define Is_blue_val(val) (Color_val(val) == Blue)
#define Is_black_val(val) (Color_val(val) == Black)


#endif /* _gc_ */
