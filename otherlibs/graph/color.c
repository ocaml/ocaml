/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"

/* Cache to speed up the translation rgb -> pixel value. */

struct color_cache_entry {
  int rgb;                      /* RGB value with format 0xRRGGBB */
  unsigned long pixel;          /* Pixel value */
};

#define Color_cache_size 64
static struct color_cache_entry color_cache[Color_cache_size];
#define Empty (-1)
#define Hash_rgb(r,g,b) \
  ((((r) & 0xC0) >> 2) + (((g) & 0xC0) >> 4) + (((b) & 0xC0) >> 6))

void gr_init_color_cache()
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

unsigned long gr_pixel_rgb(rgb)
     int rgb;

{
  unsigned int r, g, b;
  int h, i;
  XColor color;

  r = (rgb >> 16) & 0xFF;
  g = (rgb >> 8) & 0xFF;
  b = rgb & 0xFF;
  h = Hash_rgb(r, g, b);
  i = h;
  while(1) {
    if (color_cache[i].rgb == Empty) break;
    if (color_cache[i].rgb == rgb) return color_cache[i].pixel;
    i = (i + 1) & (Color_cache_size - 1);
    if (i == h) break;
  }
  color.red = r * 0x101;
  color.green = g * 0x101;
  color.blue = b * 0x101;
  XAllocColor(grdisplay, grcolormap, &color);
  color_cache[i].rgb = rgb;
  color_cache[i].pixel = color.pixel;
  return color.pixel;
}

int gr_rgb_pixel(pixel)
     unsigned long pixel;
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

value gr_set_color(vrgb)
     value vrgb;
{
  gr_check_open();
  grcolor = gr_pixel_rgb(Int_val(vrgb));
  XSetForeground(grdisplay, grwindow.gc, grcolor);
  XSetForeground(grdisplay, grbstore.gc, grcolor);
  return Val_unit;
}


                                     


