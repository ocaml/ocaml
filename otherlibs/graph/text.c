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
#include <alloc.h>

XFontStruct * grfont = NULL;

static void gr_font(char *fontname)
{
  XFontStruct * font = XLoadQueryFont(grdisplay, fontname);
  if (font == NULL) gr_fail("cannot find font %s", fontname);
  if (grfont != NULL) XFreeFont(grdisplay, grfont);
  grfont = font;
  XSetFont(grdisplay, grwindow.gc, grfont->fid);
  XSetFont(grdisplay, grbstore.gc, grfont->fid);
}

value gr_set_font(value fontname)
{
  gr_check_open();
  gr_font(String_val(fontname));
  return Val_unit;
}

value gr_set_text_size (value sz)
{
  return Val_unit;
}

static void gr_draw_text(char *txt, int len)
{
  if (grfont == NULL) gr_font(DEFAULT_FONT);
  if (grremember_mode)
    XDrawString(grdisplay, grbstore.win, grbstore.gc,
                grx, Bcvt(gry) - grfont->descent + 1, txt, len);
  if (grdisplay_mode) {
    XDrawString(grdisplay, grwindow.win, grwindow.gc,
                grx, Wcvt(gry) - grfont->descent + 1, txt, len);
    XFlush(grdisplay);
  }
  grx += XTextWidth(grfont, txt, len);
}

value gr_draw_char(value chr)
{
  char str[1];
  gr_check_open();
  str[0] = Int_val(chr);
  gr_draw_text(str, 1);
  return Val_unit;
}

value gr_draw_string(value str)
{
  gr_check_open();
  gr_draw_text(String_val(str), string_length(str));
  return Val_unit;
}

value gr_text_size(value str)
{
  int width;
  value res;
  gr_check_open();
  if (grfont == NULL) gr_font(DEFAULT_FONT);
  width = XTextWidth(grfont, String_val(str), string_length(str));
  res = alloc_small(2, 0);
  Field(res, 0) = Val_int(width);
  Field(res, 1) = Val_int(grfont->ascent + grfont->descent);
  return res;
}
