/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"
#include <X11/Xatom.h>

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

/* rgb -> pixel conversion *without* display connection */

Bool direct_rgb = False;
int red_l, red_r;
int green_l, green_r;
int blue_l, blue_r;
unsigned long red_mask, green_mask, blue_mask;

/* rgb -> pixel table */
unsigned long red_vals[256];
unsigned long green_vals[256];
unsigned long blue_vals[256];

void get_shifts( unsigned long mask, int *lsl, int *lsr )
{
  int l = 0;
  int r = 0;
  int bit = 1;
  if ( mask == 0 ){ *lsl = -1; *lsr = -1; return; }

  for( l = 0; l < 32; l++ ){
    if( bit & mask ){ break; }
    bit = bit << 1;
  }
  for( r = l; r < 32; r++ ){
    if( ! (bit & mask) ){ break; }
    bit = bit << 1;
  }
  /* fix r */
  if ( r == 32 ) { r = 31; }
  *lsl = l;
  *lsr = 16 - (r - l);
}

void gr_init_direct_rgb_to_pixel(void)
{
  Visual *visual;
  int i;
 
  visual = DefaultVisual(grdisplay,grscreen);
  
  if ( visual->class == TrueColor || visual->class == DirectColor ){
    int lsl, lsr;

    red_mask = visual->red_mask;
    green_mask = visual->green_mask;
    blue_mask = visual->blue_mask;
 
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "visual %lx %lx %lx\n", 
	    red_mask, 
	    green_mask, 
	    blue_mask);
#endif

    get_shifts(red_mask, &red_l, &red_r); 
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "red %d %d\n", red_l, red_r);
#endif
    for(i=0; i<256; i++){
      red_vals[i] = (((i << 8) + i) >> red_r) << red_l;
    }

    get_shifts(green_mask, &green_l, &green_r); 
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "green %d %d\n", green_l, green_r);
#endif
    for(i=0; i<256; i++){
      green_vals[i] = (((i << 8) + i) >> green_r) << green_l;
    }

    get_shifts(blue_mask, &blue_l, &blue_r); 
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "blue %d %d\n", blue_l, blue_r);
#endif
    for(i=0; i<256; i++){
      blue_vals[i] = (((i << 8) + i) >> blue_r) << blue_l;
    }
    
    if( red_l < 0 || red_r < 0 || 
	green_l < 0 || green_r < 0 || 
	blue_l < 0 || blue_r < 0 ){
#ifdef QUICKCOLORDEBUG
      fprintf(stderr, "Damn, boost failed\n");
#endif
      direct_rgb = False;
    } else {
#ifdef QUICKCOLORDEBUG
      fprintf(stderr, "Boost ok\n");
#endif
      direct_rgb = True;
    }
  } else {
    /* we cannot use direct_rgb_to_pixel */
#ifdef QUICKCOLORDEBUG
    fprintf(stderr, "No boost!\n");
#endif
    direct_rgb = False;
  }
}

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
    return red_vals[r] | green_vals[g] | blue_vals[b];
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
  register int r,g,b;
 
  XColor color;
  int i;

  if (direct_rgb) {
    r = (((pixel & red_mask) >> red_l) << 8) >> (16 - red_r);
    g = (((pixel & green_mask) >> green_l) << 8) >> (16 - green_r);
    b = (((pixel & blue_mask) >> blue_l) << 8) >> (16 - blue_r);
    return (r << 16) + (g << 8) + b;
  }

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
  int xcolor;
  gr_check_open();
  grcolor = Int_val(vrgb);
  if (grcolor >= 0 ){
    xcolor = gr_pixel_rgb(Int_val(vrgb));
    XSetForeground(grdisplay, grwindow.gc, xcolor);
    XSetForeground(grdisplay, grbstore.gc, xcolor);
  } else {
    XSetForeground(grdisplay, grwindow.gc, grbackground);
    XSetForeground(grdisplay, grbstore.gc, grbackground);
  }
  return Val_unit;
}
