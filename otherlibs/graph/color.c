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

/* Cache to speed up the translation rgb -> pixel value. */

struct color_cache_entry {
  int rgb;                      /* RGB value with format 0xRRGGBB */
  unsigned long pixel;          /* Pixel value */
};

#define Color_cache_size 512
static struct color_cache_entry color_cache[Color_cache_size];
#define Empty (-1)
#define Hash_rgb(r,g,b) \
  ((((r) & 0xE0) << 1) + (((g) & 0xE0) >> 2) + (((b) & 0xE0) >> 5))
#define Color_cache_slack 16

static int num_overflows = 0;

Bool direct_rgb = False;
int byte_order;
int bitmap_unit;
int bits_per_pixel;

void gr_init_color_cache(void)
{
  int i;
  for (i = 0; i < Color_cache_size; i++) color_cache[i].rgb = Empty;
  i = Hash_rgb(0, 0, 0);
  color_cache[i].rgb = 0;
  color_cache[i].pixel = grblack;
  i = Hash_rgb(0xFF, 0xFF, 0xFF);
  color_cache[i].rgb = 0xFFFFFF;
  color_cache[i].pixel = grwhite;
}

unsigned long gr_pixel_rgb(int rgb)
{
  unsigned int r, g, b;
  int h, i;
  XColor color;
  unsigned short tmp;

  r = (rgb >> 16) & 0xFF;
  g = (rgb >> 8) & 0xFF;
  b = rgb & 0xFF;

  if (direct_rgb){
    switch ( bits_per_pixel ){
    case 16:
      tmp = ((r >> 3) << 11) + ((g >> 2) << 5) + ((b >> 3) << 0);
      return (unsigned long) tmp;
    case 32:
      return (r << 16) + (g << 8) + (b << 0);
    }
  }

  h = Hash_rgb(r, g, b);
  i = h;
  while(1) {
    if (color_cache[i].rgb == Empty) break;
    if (color_cache[i].rgb == rgb) return color_cache[i].pixel;
    i = (i + 1) & (Color_cache_size - 1);
    if (i == h) {
      /* Cache is full.  Instead of inserting at slot h, which causes
         thrashing if many colors hash to the same value,
         insert at h + n where n is pseudo-random and
         smaller than Color_cache_slack */
      int slack = num_overflows++ & (Color_cache_slack - 1);
      i = (i + slack) & (Color_cache_size - 1);
      break;
    }
  }
  color.red = r * 0x101;
  color.green = g * 0x101;
  color.blue = b * 0x101;
  XAllocColor(grdisplay, grcolormap, &color);
  color_cache[i].rgb = rgb;
  color_cache[i].pixel = color.pixel;
  return color.pixel;
}

int gr_rgb_pixel(long unsigned int pixel)
{
  XColor color;
  int i;

  if (pixel == grblack) return 0;
  if (pixel == grwhite) return 0xFFFFFF;

  /* Probably faster to do a linear search than to query the X server. */
  for (i = 0; i < Color_cache_size; i++) {
    if (color_cache[i].rgb != Empty && color_cache[i].pixel == pixel)
      return color_cache[i].rgb;
  }
  color.pixel = pixel;
  XQueryColor(grdisplay, grcolormap, &color);
  return
    ((color.red >> 8) << 16) + ((color.green >> 8) << 8) + (color.blue >> 8);
}

value gr_set_color(value vrgb)
{
  gr_check_open();
  grcolor = gr_pixel_rgb(Int_val(vrgb));
  XSetForeground(grdisplay, grwindow.gc, grcolor);
  XSetForeground(grdisplay, grbstore.gc, grcolor);
  return Val_unit;
}
