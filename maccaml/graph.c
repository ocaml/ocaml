/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "alloc.h"
#include "callback.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"

#include "main.h"      /* Include main.h last or Assert will not work. */


/* The off-screen buffer that holds the contents of the graphics arena. */
static GWorldPtr gworld = NULL;

/* An arbitrarily large rectangle (for clipping). */
static Rect maxrect = { -SHRT_MAX, -SHRT_MAX, SHRT_MAX, SHRT_MAX };

/* Coordinates (relative to the window) of the top-left corner
   of the graphics arena. */
long x0, y0;

/* Width and height of the graphics arena. */
long w0, h0;

RGBColor fgcolor;

/* Convert from Caml coordinates to QD coordinates in the off-screen buffer. */
/* Note: these conversions are self-inverse (see gr_current_point). */
#define Bx(x) (x)
#define By(y) (h0-1 - (y))

/* Convert from Caml coordinates to QD coordinates in the window. */
#define Wx(x) (Bx(x) + x0)
#define Wy(y) (By(y) + y0)

/* Convert from QD window coordinates to Caml coordinates. */
#define Cx(x) ((x) - x0)
#define Cy(y) (h0-1 - ((y) - y0))


/***********************************************************************/
/* User interface functions                                            */
/***********************************************************************/

static void GraphUpdateGW (void)
{
  Rect r;
  WStatusH st = WinGetStatus (winGraphics);

  Assert (st != NULL);
  Assert (gworld != NULL);
  WELongRectToRect (&(*st)->destrect, &r);
  OffsetRect (&r, winGraphics->portRect.left, winGraphics->portRect.top);
  UpdateGWorld (&gworld, 0, &r, NULL, NULL, clipPix);
}

void GraphNewSizePos (void)
{
  GraphUpdateGW ();
}

/* The current port must be winGraphics when this function is called. */
void GraphUpdate (void)
{
  Rect r, src, dst;
  Boolean good;
  WStatusH st = WinGetStatus (winGraphics);
  RGBColor forecolor, backcolor;

  Assert (st != NULL);
  GraphUpdateGW ();
  good = LockPixels (GetGWorldPixMap (gworld));   Assert (good);
  WELongRectToRect (&(*st)->destrect, &r);
  WELongRectToRect (&(*st)->viewrect, &dst);
  src = dst;
  OffsetRect (&src, -r.left, -r.top);
  GetBackColor (&backcolor);
  GetForeColor (&forecolor);
  BackColor (whiteColor);
  ForeColor (blackColor);
  CopyBits (&((GrafPtr) gworld)->portBits, &((GrafPtr) winGraphics)->portBits,
            &src, &dst, srcCopy, NULL);
  RGBBackColor (&backcolor);
  RGBForeColor (&forecolor);
  UnlockPixels (GetGWorldPixMap (gworld));
}

/* All scrolling of the graphics window must go through this function
   so it can update the coordinates x0 and y0, and the pen location. */
void GraphScroll (long dx, long dy)
{
  Rect r;
  RgnHandle update = NewRgn ();
  WStatusH st = WinGetStatus (winGraphics);
  Point p;
  GrafPtr port;

  Assert (st != NULL);
  GetPort (&port);
  SetPort (winGraphics);
  WELongRectToRect (&(*st)->viewrect, &r);
  ScrollRect (&r, dx, dy, update);
  WEOffsetLongRect (&(*st)->destrect, dx, dy);
  SetClip (update);
  GraphUpdate ();
  ClipRect (&maxrect);
  DisposeRgn (update);

  x0 += dx;
  y0 += dy;
  GetPen (&p);
  MoveTo (p.h + dx, p.v + dy);
  SetPort (port);
}

/* Graphics event queue */
#define GraphQsize 15
static EventRecord graphQ[GraphQsize];
static int graphQlen = 0;

#define Succ(x) ((x) >= GraphQsize ? 0 : (x)+1)

void GraphGotEvent (EventRecord *evt)
{
  GrafPort *saveport;

  if (graphQlen < GraphQsize) ++ graphQlen;
  memmove (&(graphQ[1]), &(graphQ[0]), (graphQlen - 1) * sizeof (graphQ[0]));

  graphQ[0] = *evt;

  PushWindowPort (winGraphics);
  GlobalToLocal (&(graphQ[0].where));
  PopPort;
}
static void DequeueEvent (int i)
{
  -- graphQlen;
  memmove (&(graphQ[i]), &(graphQ[i+1]), (graphQlen - i) * sizeof (graphQ[0]));
}

/***********************************************************************/
/* Primitives for the graphics library                                 */
/***********************************************************************/

value gr_open_graph (value vgeometry);
value gr_close_graph (value unit);
value gr_sigio_signal (value unit);
value gr_sigio_handler (value unit);
value gr_display_mode (value flag);
value gr_remember_mode (value flag);
value gr_synchronize (value unit);
value gr_clear_graph (value unit);
value gr_size_x (value unit);
value gr_size_y (value unit);
value gr_set_color (value vrgb);
value gr_plot (value vx, value vy);
value gr_point_color (value vx, value vy);
value gr_moveto (value vx, value vy);
value gr_current_x (value unit);
value gr_current_y (value unit);
value gr_lineto (value vx, value vy);
value gr_draw_rect (value vx, value vy, value vw, value vh);
value gr_draw_arc (value *argv, int argc);
value gr_draw_arc_nat (value, value, value, value, value, value);
value gr_set_line_width (value vwidth);
value gr_fill_rect (value vx, value vy, value vw, value vh);
value gr_fill_poly (value vpoints);
value gr_fill_arc (value *argv, int argc);
value gr_fill_arc_nat (value, value, value, value, value, value);
value gr_draw_char (value vchr);
value gr_draw_string (value vstr);
value gr_set_font (value vfontname);
value gr_set_text_size (value vsz);
value gr_text_size (value vstr);
value gr_make_image (value varray);
value gr_dump_image (value vimage);
value gr_draw_image (value vimage, value vx, value vy);
value gr_create_image (value vw, value vh);
value gr_blit_image (value vimage, value vx, value vy);
value gr_wait_event (value veventlist);
value gr_sound (value vfreq, value vdur);


/**** Ancillary macros and function */

/* double-buffer or write-through */
static int grdisplay_mode;
static int grremember_mode;

/* Current state */
static long cur_x, cur_y;
static short cur_width, cur_font, cur_size;
/* see also fgcolor */


/* Drawing off-screen and on-screen simultaneously.  The following three
   macros must always be used together and in this order.
*/
/* 1. Begin drawing in the off-screen buffer. */
#define BeginOff { \
  CGrafPtr _saveport_; \
  GDHandle _savegdev_; \
  Rect _cliprect_; \
  if (grremember_mode) { \
    GetGWorld (&_saveport_, &_savegdev_); \
    LockPixels (GetGWorldPixMap (gworld)); \
    SetGWorld ((CGrafPtr) gworld, NULL);

/* 2. Continue with on-screen drawing. */
#define On \
    SetGWorld (_saveport_, _savegdev_); \
    UnlockPixels (GetGWorldPixMap (gworld)); \
  } \
  if (grdisplay_mode) { \
    SetPort (winGraphics); \
    ScrollCalcGraph (winGraphics, &_cliprect_); \
    ClipRect (&_cliprect_);

/* 3. Clean up after drawing. */
#define EndOffOn \
    ClipRect (&maxrect); \
    SetPort ((GrafPtr) _saveport_); \
  } \
}

/* Set up the current port unconditionally.  This is for functions that
   don't draw (measurements and setting the graphport state).
   Usage: BeginOffAlways / EndOffAlways
      or  BeginOffAlways / OnAlways / EndOffOnAlways
   */
#define BeginOffAlways { \
  CGrafPtr _saveport_; \
  GDHandle _savegdev_; \
  GetGWorld (&_saveport_, &_savegdev_); \
  LockPixels (GetGWorldPixMap (gworld)); \
  SetGWorld ((CGrafPtr) gworld, NULL);

#define EndOffAlways \
  SetGWorld (_saveport_, _savegdev_); \
  UnlockPixels (GetGWorldPixMap (gworld)); \
}

#define OnAlways \
  SetGWorld (_saveport_, _savegdev_); \
  UnlockPixels (GetGWorldPixMap (gworld)); \
  SetPort (winGraphics); \

#define EndOffOnAlways \
  SetPort ((GrafPtr) _saveport_); \
}

/* Convert a red, green, or blue value from 8 bits to 16 bits. */
#define RGB8to16(x) ((x) | ((x) << 8))

/* Declare and convert x and y from vx and vy. */
#define XY long x = Long_val (vx), y = Long_val (vy)


static value * graphic_failure_exn = NULL;

static void gr_fail(char *fmt, void *arg)
{
  char buffer[1024];

  if (graphic_failure_exn == NULL) {
    graphic_failure_exn = caml_named_value("Graphics.Graphic_failure");
    if (graphic_failure_exn == NULL){
      invalid_argument("Exception Graphics.Graphic_failure not initialized,"
                       " you must load graphics.cma");
    }
  }
  sprintf(buffer, fmt, arg);
  raise_with_string(*graphic_failure_exn, buffer);
}

static void gr_check_open (void)
{
  if (winGraphics == NULL) gr_fail("graphic screen not opened", NULL);
}

/* Max_image_mem is the number of image pixels that can be allocated
   in one major GC cycle.  The GC will speed up to match this allocation
   speed.
*/
#define Max_image_mem 1000000   /*FIXME Should use user pref. */

#define Transparent (-1)

struct grimage {
  final_fun f;                  /* Finalization function */
  long width, height;           /* Dimensions of the image */
  GWorldPtr data;               /* Pixels */
  GWorldPtr mask;               /* Mask for transparent points, or NULL */
};

#define Grimage_wosize \
  ((sizeof (struct grimage) + sizeof (value) - 1) / sizeof (value))

static void free_image (value vimage)
{
  struct grimage *im = (struct grimage *) Bp_val (vimage);

  if (im->data != NULL) DisposeGWorld (im->data);
  if (im->mask != NULL) DisposeGWorld (im->mask);
}

static value alloc_image (long w, long h)
{
  value res = alloc_final (Grimage_wosize, free_image, w*h, Max_image_mem);
  struct grimage *im = (struct grimage *) Bp_val (res);
  Rect r;
  QDErr err;

  im->width = w;
  im->height = h;
  im->mask = NULL;
  SetRect (&r, 0, 0, w, h);
  err = NewGWorld (&im->data, 32, &r, NULL, NULL, 0);
  if (err != noErr){
    im->data = NULL;
    gr_fail ("Cannot allocate image (error code %ld)", (void *) err);
  }
  return res;
}

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

/***********************************************************************/

value gr_open_graph (value vgeometry)
{
  int i;
  short err;
  Rect r;
  WStatusH st;

  if (winGraphics == NULL){
    Assert (gworld == NULL);

    i = sscanf (String_val (vgeometry), "%ldx%ld", &w0, &h0);
    if (i < 2){
      w0 = 640;
      h0 = 480;
    }
    if (w0 < kMinWindowWidth - kScrollBarWidth - 1){
      w0 = kMinWindowWidth - kScrollBarWidth - 1;
    }
    if (h0 < kMinWindowHeight - kScrollBarWidth - 1){
      h0 = kMinWindowHeight - kScrollBarWidth - 1;
    }

    err = WinOpenGraphics (w0, h0);
    if (err != noErr) goto failed;

    x0 = y0 = 0;

    st = WinGetStatus (winGraphics);   Assert (st != NULL);
    WELongRectToRect (&(*st)->destrect, &r);
    OffsetRect (&r, winGraphics->portRect.left, winGraphics->portRect.top);
    err = NewGWorld (&gworld, 0, &r, NULL, NULL, 0);
    if (err != noErr) goto failed;

    fgcolor.red = fgcolor.green = fgcolor.blue = 0;
  }
  /* Synchronise off-screen and on-screen by initialising everything. */
  grremember_mode = 1;
  grdisplay_mode = 1;
  gr_clear_graph (Val_unit);
  gr_moveto (Val_long (0), Val_long (0));
  gr_set_color (Val_long (0));
  gr_set_line_width (Val_long (0));
  gr_set_font ((value) "geneva");           /* XXX hack */
  gr_set_text_size (Val_long (12));

  return Val_unit;

  failed:
    if (gworld != NULL){
      DisposeGWorld (gworld);
      gworld = NULL;
    }
    if (winGraphics != NULL) WinCloseGraphics ();
    gr_fail ("open_graph failed (error %d)", (void *) (long) err);
    return Val_unit; /* not reached */
}

value gr_close_graph (value unit)
{
#pragma unused (unit)
  gr_check_open ();
  WinCloseGraphics ();
  DisposeGWorld (gworld);
  gworld = NULL;
  return Val_unit;
}

value gr_sigio_signal (value unit)            /* Not used on MacOS */
{
#pragma unused (unit)
  return Val_unit;
}

value gr_sigio_handler (value unit)           /* Not used on MacOS */
{
#pragma unused (unit)
  return Val_unit;
}

value gr_synchronize (value unit)
{
#pragma unused (unit)
  GrafPtr saveport;
  
  gr_check_open ();
  PushWindowPort (winGraphics);
  GraphUpdate ();
  PopPort;
  return Val_unit;
}

value gr_display_mode (value flag)
{
  grdisplay_mode = Bool_val (flag);
  return Val_unit;
}

value gr_remember_mode (value flag)
{
  grremember_mode = Bool_val (flag);
  return Val_unit;
}

value gr_clear_graph (value unit)
{
#pragma unused (unit)
  gr_check_open ();
  BeginOff
    EraseRect (&maxrect);
  On
    EraseRect (&maxrect);
  EndOffOn
  return unit;
}

value gr_size_x (value unit)
{
#pragma unused (unit)
  gr_check_open ();
  return Val_long (w0);
}

value gr_size_y (value unit)
{
#pragma unused (unit)
  gr_check_open ();
  return Val_long (h0);
}

value gr_set_color (value vrgb)
{
  long rgb = Long_val (vrgb);

  gr_check_open ();
  fgcolor.red = RGB8to16 ((rgb >> 16) & 0xFF);
  fgcolor.green = RGB8to16 ((rgb >> 8) & 0xFF);
  fgcolor.blue = RGB8to16 (rgb & 0xFF);
  BeginOffAlways
    RGBForeColor (&fgcolor);
  OnAlways
    RGBForeColor (&fgcolor);
  EndOffOnAlways
  return Val_unit;
}

value gr_plot (value vx, value vy)
{
  XY;

  gr_check_open ();
  BeginOff
    SetCPixel (Bx (x), By (y) - 1, &fgcolor);
  On
    SetCPixel (Wx (x), Wy (y) - 1, &fgcolor);
  EndOffOn
  return Val_unit;
}

value gr_point_color (value vx, value vy)
{
  XY;
  RGBColor c;

  gr_check_open ();
  if (x < 0 || x >= w0 || y < 0 || y >= h0) return Val_long (-1);
  BeginOffAlways
    GetCPixel (Bx (x), By (y) - 1, &c);
  EndOffAlways
  return Val_long (((c.red & 0xFF00) << 8)
                   | (c.green & 0xFF00)
                   | ((c.blue & 0xFF00) >> 8));
}

value gr_moveto (value vx, value vy)
{
  XY;

  gr_check_open ();
  cur_x = x; cur_y = y;
  return Val_unit;
}

value gr_current_x (value unit)
{
#pragma unused (unit)

  gr_check_open ();
  return Val_long (cur_x);
}

value gr_current_y (value unit)
{
#pragma unused (unit)

  gr_check_open ();
  return Val_long (cur_y);
}

value gr_lineto (value vx, value vy)
{
  XY;
  int delta = cur_width / 2;

  gr_check_open ();
  BeginOff
    MoveTo (Bx (cur_x) - delta, By (cur_y) - delta);
    LineTo (Bx (x) - delta, By (y) - delta);
  On
    MoveTo (Wx (cur_x) - delta, Wy (cur_y) - delta);
    LineTo (Wx (x) - delta, Wy (y) - delta);
  EndOffOn
  cur_x = x; cur_y = y;
  return Val_unit;
}

value gr_draw_rect (value vx, value vy, value vw, value vh)
{
  XY;
  long w = Long_val (vw), h = Long_val (vh);
  Rect r;
  int d1 = cur_width / 2;
  int d2 = cur_width - d1;

  gr_check_open ();
  BeginOff
    SetRect (&r, Bx (x) - d1, By (y+h) - d1, Bx (x+w) + d2, By (y) + d2);
    FrameRect (&r);
  On
    SetRect (&r, Wx (x) - d1, Wy (y+h) - d1, Wx (x+w) + d2, Wy (y) + d2);
    FrameRect (&r);
  EndOffOn
  return Val_unit;
}

value gr_draw_arc (value *argv, int argc)
{
#pragma unused (argc)
  return gr_draw_arc_nat (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value gr_draw_arc_nat (value vx, value vy, value vrx, value vry, value va1,
                       value va2)
{
  XY;
  long rx = Long_val (vrx), ry = Long_val (vry);
  long a1 = Long_val (va1), a2 = Long_val (va2);
  Rect r;
  long qda1 = 90 - a1, qda2 = 90 - a2;
  int d1 = cur_width / 2;
  int d2 = cur_width - d1;

  gr_check_open ();
  BeginOff
    SetRect (&r, Bx(x-rx) - d1, By(y+ry) - d1, Bx(x+rx) + d2, By(y-ry) + d2);
    FrameArc (&r, qda1, qda2 - qda1);
  On
    SetRect (&r, Wx(x-rx) - d1, Wy(y+ry) - d1, Wx(x+rx) + d2, Wy(y-ry) + d2);
    FrameArc (&r, qda1, qda2 - qda1);
  EndOffOn
  return Val_unit;
}

value gr_set_line_width (value vwidth)
{
  short width = Int_val (vwidth);

  if (width == 0) width = 1;
  gr_check_open ();
  BeginOffAlways
    PenSize (width, width);
  OnAlways
    PenSize (width, width);
  EndOffOnAlways
  cur_width = width;
  return Val_unit;
}

value gr_fill_rect (value vx, value vy, value vw, value vh)
{
  XY;
  long w = Long_val (vw), h = Long_val (vh);
  Rect r;

  gr_check_open ();
  BeginOff
    SetRect (&r, Bx (x), By (y+h), Bx (x+w), By (y));
    PaintRect (&r);
  On
    SetRect (&r, Wx (x), Wy (y+h), Wx (x+w), Wy (y));
    PaintRect (&r);
  EndOffOn
  return Val_unit;
}

value gr_fill_poly (value vpoints)
{
  long i, n = Wosize_val (vpoints);
  PolyHandle p;

  #define Bxx(i) Bx (Int_val (Field (Field (vpoints, (i)), 0)))
  #define Byy(i) By (Int_val (Field (Field (vpoints, (i)), 1)))

  gr_check_open ();
  if (n < 1) return Val_unit;

  p = OpenPoly ();
  MoveTo (Bxx (0), Byy (0));
  for (i = 1; i < n; i++) LineTo (Bxx (i), Byy (i));
  ClosePoly ();
  BeginOff
    PaintPoly (p);
  On
    OffsetPoly (p, x0, y0);
    PaintPoly (p);
  EndOffOn
  KillPoly (p);
  return Val_unit;
}

value gr_fill_arc (value *argv, int argc)
{
#pragma unused (argc)
  return gr_fill_arc_nat (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value gr_fill_arc_nat (value vx, value vy, value vrx, value vry, value va1,
                       value va2)
{
  XY;
  long rx = Long_val (vrx), ry = Long_val (vry);
  long a1 = Long_val (va1), a2 = Long_val (va2);
  Rect r;
  long qda1 = 90 - a1, qda2 = 90 - a2;

  gr_check_open ();
  BeginOff
    SetRect (&r, Bx (x-rx), By (y+ry), Bx (x+rx), By (y-ry));
    PaintArc (&r, qda1, qda2 - qda1);
  On
    SetRect (&r, Wx (x-rx), Wy (y+ry), Wx (x+rx), Wy (y-ry));
    PaintArc (&r, qda1, qda2 - qda1);
  EndOffOn
  return Val_unit;
}

static void draw_text (char *txt, unsigned long len)
{
  FontInfo info;
  unsigned long w;
  
  if (len > 32767) len = 32767;

  BeginOffAlways
    GetFontInfo (&info);
    w = TextWidth (txt, 0, len);
  EndOffAlways

  gr_check_open ();
  BeginOff
    MoveTo (Bx (cur_x), By (cur_y) - info.descent);
    DrawText (txt, 0, len);
  On
    MoveTo (Wx (cur_x), Wy (cur_y) - info.descent);
    DrawText (txt, 0, len);
  EndOffOn
  cur_x += w;
}

value gr_draw_char (value vchr)
{
  char c = Int_val (vchr);

  draw_text (&c, 1);
  return Val_unit;
}

value gr_draw_string (value vstr)
{
  mlsize_t len = string_length (vstr);
  char *str = String_val (vstr);

  draw_text (str, len);
  return Val_unit;
}

value gr_set_font (value vfontname)
{
  Str255 pfontname;
  short fontnum;

  gr_check_open ();
  CopyCStringToPascal (String_val (vfontname), pfontname);
  GetFNum (pfontname, &fontnum);
  BeginOffAlways
    TextFont (fontnum);
  OnAlways
    TextFont (fontnum);
  EndOffOnAlways
  cur_font = fontnum;
  return Val_unit;
}

value gr_set_text_size (value vsz)
{
  short sz = Int_val (vsz);

  gr_check_open ();
  BeginOffAlways
    TextSize (sz);
  OnAlways
    TextSize (sz);
  EndOffOnAlways
  cur_size = sz;
  return Val_unit;
}

value gr_text_size (value vstr)
{
  mlsize_t len = string_length (vstr);
  char *str = String_val (vstr);
  value result = alloc_tuple (2);
  FontInfo info;
  long w, h;

  BeginOffAlways
    GetFontInfo (&info);
    w = TextWidth (str, 0, len);
    h = info.ascent + info.descent;
  EndOffAlways
  Field (result, 0) = Val_long (w);
  Field (result, 1) = Val_long (h);
  return result;
}

value gr_make_image (value varray)
{
  long height = Wosize_val (varray);
  long width;
  long x, y;
  GWorldPtr w;
  value result, line;
  long color;
  RGBColor qdcolor;
  int has_transp = 0;
  CGrafPtr saveport;
  GDHandle savegdev;

  gr_check_open ();
  if (height == 0) return alloc_image (0, 0);
  width = Wosize_val (Field (varray, 0));
  for (y = 1; y < height; y++){
    if (Wosize_val (Field (varray, y)) != width){
      gr_fail("make_image: lines of different lengths", NULL);
    }
  }

  result = alloc_image (width, height);
  w = ((struct grimage *) Bp_val (result))->data;

  LockPixels (GetGWorldPixMap (w));
  GetGWorld (&saveport, &savegdev);
  SetGWorld ((CGrafPtr) w, NULL);
  for (y = 0; y < height; y++){
    line = Field (varray, y);
    for (x = 0; x < width; x++){
      color = Long_val (Field (line, x));
      if (color == Transparent) has_transp = 1;
      qdcolor.red = ((color >> 16) & 0xFF) | ((color >> 8) & 0xFF00);
      qdcolor.green = ((color >> 8) & 0xFF) | (color & 0xFF00);
      qdcolor.blue = (color & 0xFF) | ((color << 8) & 0xFF00);
      SetCPixel (x, y, &qdcolor);
    }
  }
  UnlockPixels (GetGWorldPixMap (w));

  if (has_transp){
    Rect r;
    QDErr err;

    SetRect (&r, 0, 0, width, height);
    err = NewGWorld (&w, 1, &r, NULL, NULL, 0);
    if (err != noErr){
      SetGWorld (saveport, savegdev);
      gr_fail ("Cannot allocate image (error code %d)", (void *) err);
    }
    LockPixels (GetGWorldPixMap (w));
    SetGWorld ((CGrafPtr) w, NULL);
    EraseRect (&maxrect);
    qdcolor.red = qdcolor.green = qdcolor.blue = 0;
    for (y = 0; y < height; y++){
      line = Field (varray, y);
      for (x = 0; x < width; x++){
        color = Long_val (Field (line, x));
        if (color != Transparent) SetCPixel (x, y, &qdcolor);
      }
    }
    UnlockPixels (GetGWorldPixMap (w));
    ((struct grimage *) Bp_val (result))->mask = w;
  }

  SetGWorld (saveport, savegdev);

  return result;
}

value gr_dump_image (value vimage)
{
  value result = Val_unit;
  struct grimage *im = (struct grimage *) Bp_val (vimage);
  long width = im->width;
  long height = im->height;
  long x, y;
  GWorldPtr wdata = im->data;
  GWorldPtr wmask = im->mask;
  CGrafPtr saveport;
  GDHandle savegdev;
  RGBColor qdcolor;
  value line;

  gr_check_open ();
  Begin_roots2 (vimage, result);
    result = gr_alloc_int_vect (height);
    for (y = 0; y < height; y++){
      value v = gr_alloc_int_vect (width);
      modify (&Field (result, y), v);
    }
  End_roots ();
  GetGWorld (&saveport, &savegdev);
  LockPixels (GetGWorldPixMap (wdata));
  SetGWorld (wdata, NULL);
  for (y = 0; y < height; y++){
    line = Field (result, y);
    for (x = 0; x < width; x++){
      GetCPixel (x, y, &qdcolor);
      Field (line, x) = Val_long (((qdcolor.red & 0xFF00) << 8)
                                  | (qdcolor.green & 0xFF00)
                                  | ((qdcolor.blue & 0xFF00) >> 8));
    }
  }
  UnlockPixels (GetGWorldPixMap (wdata));
  if (wmask != NULL){
    LockPixels (GetGWorldPixMap (wmask));
    SetGWorld (wmask, NULL);
    for (y = 0; y < height; y++){
      line = Field (result, y);
      for (x = 0; x < width; x++){
        if (!GetPixel (x, y)) Field (line, x) = Val_long (Transparent);
      }
    }
    UnlockPixels (GetGWorldPixMap (wmask));
  }
  SetGWorld (saveport, savegdev);
  return result;
}

value gr_draw_image (value vimage, value vx, value vy)
{
  XY;
  struct grimage *im = (struct grimage *) Bp_val (vimage);
  RGBColor forecolor, backcolor;
  Rect srcrect, dstrect;

  SetRect (&srcrect, 0, 0, im->width, im->height);
  if (im->mask != NULL){
    LockPixels (GetGWorldPixMap (im->data));
    LockPixels (GetGWorldPixMap (im->mask));
    BeginOff
      SetRect (&dstrect, Bx (x), By (y+im->height), Bx (x+im->width), By (y));
      GetBackColor (&backcolor);
      GetForeColor (&forecolor);
      BackColor (whiteColor);
      ForeColor (blackColor);
      CopyMask (&((GrafPtr) im->data)->portBits,
                &((GrafPtr) im->mask)->portBits,
                &((GrafPtr) gworld)->portBits,
                &srcrect, &srcrect, &dstrect);
      RGBBackColor (&backcolor);
      RGBForeColor (&forecolor);
    On
      SetRect (&dstrect, Wx (x), Wy (y+im->height), Wx (x+im->width), Wy (y));
      GetBackColor (&backcolor);
      GetForeColor (&forecolor);
      BackColor (whiteColor);
      ForeColor (blackColor);
      CopyMask (&((GrafPtr) im->data)->portBits,
                &((GrafPtr) im->mask)->portBits,
                &((GrafPtr) winGraphics)->portBits,
                &srcrect, &srcrect, &dstrect);
      RGBBackColor (&backcolor);
      RGBForeColor (&forecolor);
    EndOffOn
    UnlockPixels (GetGWorldPixMap (im->data));
    UnlockPixels (GetGWorldPixMap (im->mask));
  }else{
    LockPixels (GetGWorldPixMap (im->data));
    BeginOff
      SetRect (&dstrect, Bx (x), By (y+im->height), Bx (x+im->width), By (y));
      GetBackColor (&backcolor);
      GetForeColor (&forecolor);
      BackColor (whiteColor);
      ForeColor (blackColor);
      CopyBits (&((GrafPtr) im->data)->portBits, &((GrafPtr) gworld)->portBits,
                &srcrect, &dstrect, srcCopy, NULL);
      RGBBackColor (&backcolor);
      RGBForeColor (&forecolor);
    On
      SetRect (&dstrect, Wx (x), Wy (y+im->height), Wx (x+im->width), Wy (y));
      GetBackColor (&backcolor);
      GetForeColor (&forecolor);
      BackColor (whiteColor);
      ForeColor (blackColor);
      CopyBits (&((GrafPtr) im->data)->portBits,
                &((GrafPtr) winGraphics)->portBits, &srcrect, &dstrect, srcCopy,
                NULL);
      RGBBackColor (&backcolor);
      RGBForeColor (&forecolor);
    EndOffOn
    UnlockPixels (GetGWorldPixMap (im->data));
  }
  return Val_unit;
}

value gr_create_image (value vw, value vh)
{
  return alloc_image (Long_val (vw), Long_val (vh));
}

value gr_blit_image (value vimage, value vx, value vy)
{
  XY;
  struct grimage *im = (struct grimage *) Bp_val (vimage);
  Rect srcrect, dstrect, worldrect;
  CGrafPtr saveport;
  GDHandle savegdev;

  SetRect (&worldrect, 0, 0, w0, h0);
  SetRect (&srcrect, Bx (x), By (y+im->height), Bx (x+im->width), By (y));
  SectRect (&srcrect, &worldrect, &srcrect);
  dstrect = srcrect;
  OffsetRect (&dstrect, -Bx (x), -By (y+im->height));

  LockPixels (GetGWorldPixMap (im->data));
  LockPixels (GetGWorldPixMap (gworld));
  GetGWorld (&saveport, &savegdev);
  SetGWorld (im->data, NULL);
  BackColor (whiteColor);
  ForeColor (blackColor);
  CopyBits (&((GrafPtr) gworld)->portBits, &((GrafPtr) im->data)->portBits,
            &srcrect, &dstrect, srcCopy, NULL);
  SetGWorld (saveport, savegdev);
  UnlockPixels (GetGWorldPixMap (im->data));
  UnlockPixels (GetGWorldPixMap (gworld));
  return Val_unit;
}

int motion_requested = 0;
short motion_oldx, motion_oldy;
/* local coord versions of motion_oldx, motion_oldy */
static Point lastpt = {SHRT_MAX - 1, SHRT_MAX - 1};

#define Button_down_val 0
#define Button_up_val 1
#define Key_pressed_val 2
#define Mouse_motion_val 3
#define Poll_val 4

value gr_wait_event (value veventlist)
{
  int askmousedown = 0, askmouseup = 0, askkey = 0, askmotion = 0, askpoll = 0;
  GrafPtr saveport;
  value result;
  int mouse_x, mouse_y, button, keypressed, key;
  Point pt;
  int i;

  gr_check_open();
  PushWindowPort (winGraphics);

  while (veventlist != Val_int (0)) {
    switch (Int_val(Field (veventlist, 0))) {
    case Button_down_val:  askmousedown = 1; break;
    case Button_up_val:    askmouseup = 1;   break;
    case Key_pressed_val:  askkey = 1;       break;
    case Mouse_motion_val: askmotion = 1;    break;
    case Poll_val:         askpoll = 1;      break;
    default: Assert (0);
    }
    veventlist = Field (veventlist, 1);
  }

  enter_blocking_section ();

  while (1){
    while (graphQlen > 0 && graphQ[0].when + 300 < TickCount ()){
      DequeueEvent (0);
    }
    for (i = graphQlen - 1; i >= 0; i--){
      int what = graphQ[i].what;
      if (askpoll){
        if (what == keyDown || what == autoKey){
          GetMouse (&pt);
          mouse_x = pt.h;
          mouse_y = pt.v;
          button = Button ();
          keypressed = 1;
          key = graphQ[i].message & charCodeMask;
          goto gotevent;
        }
      }else if (   askmousedown && what == mouseDown
                || askmouseup && what == mouseUp){
        mouse_x = graphQ[i].where.h;
        mouse_y = graphQ[i].where.v;
        button = graphQ[i].what == mouseDown;
        keypressed = 0;
        DequeueEvent (i);
        goto gotevent;
      }else if (askkey && (what == keyDown || what == autoKey)){
        mouse_x = graphQ[i].where.h;
        mouse_y = graphQ[i].where.v;
        button = Button ();
        keypressed = 1;
        key = graphQ[i].message & charCodeMask;
        DequeueEvent (i);
        goto gotevent;
      }
    }
    GetMouse (&pt);
    if (askpoll || askmotion && (pt.h != lastpt.h || pt.v != lastpt.v)){
      mouse_x = pt.h;
      mouse_y = pt.v;
      button = Button ();
      keypressed = 0;
      goto gotevent;
    }
    if (askmotion){
      motion_requested = 1;
      pt = lastpt;
      LocalToGlobal (&pt);
      motion_oldx = pt.h;
      motion_oldy = pt.v;
    }
    sched_yield ();
    if (pending_signal != 0 && pending_signal != SIGVTALRM){
      /* handle signals, but not the tick thread stuff because:
         1. We don't hold the master lock so it is not needed.
         2. It would execute some Caml code and prevent processor idle.
      */
      PopPort;
      intr_requested = 0;
      raise (SIGINT);
      leave_blocking_section ();
      enter_blocking_section ();
      PushWindowPort (winGraphics);
    }
  }

  gotevent:
    PopPort;
    leave_blocking_section ();  /* acquire master lock, handle signals */
    lastpt.h = mouse_x;
    lastpt.v = mouse_y;
    motion_requested = 0;
  
    result = alloc_tuple (5);
    Field (result, 0) = Val_int (Cx (mouse_x));
    Field (result, 1) = Val_int (Cy (mouse_y));
    Field (result, 2) = Val_bool (button);
    Field (result, 3) = Val_bool (keypressed);
    Field (result, 4) = Val_int (key);
    return result;
}

value gr_sound (value vfreq, value vdur)
{
  long freq = Long_val (vfreq);
  long dur = Long_val (vdur);
  long scale;
  Handle h;
  OSErr err;

  if (dur <= 0 || freq <= 0) return Val_unit;
  if (dur > 5000) dur = 5000;
  if (freq > 20000) gr_fail ("sound: frequency is too high", NULL);

  if      (freq > 11025) scale =   2;
  else if (freq >  5513) scale =   4;
  else if (freq >  1378) scale =   8;
  else if (freq >   345) scale =  32;
  else if (freq >    86) scale = 128;
  else                   scale = 512;

  h = GetResource ('snd ', 1000 + scale);
  if (h == NULL){
    gr_fail ("sound: resource error (code = %ld)", (void *) (long) ResError ());
  }
  err = HandToHand (&h);
  if (err != noErr) gr_fail ("sound: out of memory", NULL);
  *(unsigned short *)((*h)+kDurationOffset) = dur * 2;
  Assert (scale * freq < 0x10000);
  *(unsigned short *)((*h)+kSampleRateOffset) = scale * freq;
  HLock (h);
  err = SndPlay (NULL, (SndListHandle) h, false);
  HUnlock (h);
  if (err != noErr){
    gr_fail ("sound: cannot play sound (error code %ld)", (void *) (long) err);
  }

  return Val_unit;
}
