/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "alloc.h"
#include "callback.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"
#include "str.h"

#include "main.h"      /* Include main.h last or Assert will not work. */


/* The off-screen buffer that holds the contents of the graphics
   arena. */
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
#define By(y) (h0 - (y))

/* Convert from Caml coordinates to QD coordinates in the window. */
#define Wx(x) ((x) + x0)
#define Wy(y) ((h0 - (y)) + y0)

/* Convert from QD window coordinates to Caml coordinates. */
#define Cx(x) ((x) - x0)
#define Cy(y) (h0 - ((y) - y0))


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

  Assert (st != NULL);
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
}

/* keyboard event queue */
typedef struct {
  short mouse_x, mouse_y;
  short button;
  char key;
} GraphEvent;

#define Queue_length 256
static GraphEvent graphQ [Queue_length];
static long graphQhd = 0, graphQtl = 0;

#define Incr(x){ \
  ++ (x); \
  if ((x) >= Queue_length) (x) = 0; \
}

static void GraphQPush (GraphEvent *evt)
{
  graphQ [graphQtl] = *evt;
  Incr (graphQtl);
  if (graphQtl == graphQhd) { Incr (graphQhd); }
}

static GraphEvent *GraphQPop (void)
{
  if (graphQhd == graphQtl){
    return NULL;
  }else{
    GraphEvent *result = &graphQ [graphQhd];
    Incr (graphQhd);
    return result;
  }
}

static GraphEvent *GraphQHead (void)
{
  if (graphQhd == graphQtl){
    return NULL;
  }else{
    return &graphQ [graphQhd];
  }
}

#define Button_down_val 0
#define Button_up_val 1
#define Key_pressed_val 2
#define Mouse_motion_val 3
#define Poll_val 4

/* The latest mouse event that took place in the graphics window. */
static struct {
  int valid;
  long type;
  long mouse_x;
  long mouse_y;
} latestevent;

void GraphGotEvent (EventRecord *evt)
{
  GrafPtr saveport;
  Point pt = evt->where;
  GraphEvent grevt;
  
  GetPort (&saveport);
  SetPort (winGraphics);
  GlobalToLocal (&pt);
  SetPort (saveport);
  

  switch (evt->what){
  case mouseDown:
    latestevent.type = Button_down_val;
    latestevent.valid = 1;
    latestevent.mouse_x = Cx (pt.h);
    latestevent.mouse_y = Cy (pt.v);
    break;
  case mouseUp:
    latestevent.type = Button_up_val;
    latestevent.valid = 1;
    latestevent.mouse_x = Cx (pt.h);
    latestevent.mouse_y = Cy (pt.v);
    break;
  case keyDown:
  case autoKey:
    grevt.mouse_x = Cx (pt.h);
    grevt.mouse_y = Cy (pt.v);
    grevt.button = evt->modifiers & btnState;
    grevt.key = evt->message & charCodeMask;
    GraphQPush (&grevt);
    break;
  default: Assert (0);
  }
}

/***********************************************************************/
/* Primitives for the graphics library                                 */
/***********************************************************************/

value gr_open_graph (value vgeometry);
value gr_close_graph (value unit);
value gr_sigio_signal (value unit);
value gr_sigio_handler (value unit);
value gr_clear_graph (value unit);
value gr_size_x (value unit);
value gr_size_y (value unit);
value gr_set_color (value vrgb);
value gr_plot (value vx, value vy);
value gr_point_color (value vx, value vy);
value gr_moveto (value vx, value vy);
value gr_current_point (value unit);
value gr_lineto (value vx, value vy);
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

/* Drawing off-screen and on-screen simultaneously.  The following three
   macros must always be used together and in this order.
*/
/* 1. Begin drawing in the off-screen buffer. */
#define BeginOff { \
  CGrafPtr _saveport_; \
  GDHandle _savegdev_; \
  Rect _cliprect_; \
  GetGWorld (&_saveport_, &_savegdev_); \
  LockPixels (GetGWorldPixMap (gworld)); \
  SetGWorld ((CGrafPtr) gworld, NULL);

/* 2. Continue with on-screen drawing. */
#define On \
  SetGWorld (_saveport_, _savegdev_); \
  UnlockPixels (GetGWorldPixMap (gworld)); \
  SetPort (winGraphics); \
  ScrollCalcGraph (winGraphics, &_cliprect_); \
  ClipRect (&_cliprect_);

/* 3. Clean up after drawing. */
#define End \
  ClipRect (&maxrect); \
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
#define Max_image_mem 1000000   /* XXX Should use 20% of total memory */

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
  gr_clear_graph (Val_unit);
  gr_moveto (Val_long (0), Val_long (0));
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
  return Val_unit;
}

value gr_sigio_handler (value unit)           /* Not used on MacOS */
{
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
  End
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
  BeginOff
    RGBForeColor (&fgcolor);
  On
    RGBForeColor (&fgcolor);
  End
  return Val_unit;
}

value gr_plot (value vx, value vy)
{
  XY;

  gr_check_open ();
  BeginOff
    SetCPixel (Bx (x), By (y+1), &fgcolor);
  On
    SetCPixel (Wx (x), Wy (y+1), &fgcolor);
  End
  return Val_unit;
}

value gr_point_color (value vx, value vy)
{
  XY;
  RGBColor c;

  gr_check_open ();
  if (x < 0 || x >= w0 || y < 0 || y >= h0) return Val_long (-1);
  BeginOff
    GetCPixel (Bx (x), By (y+1), &c);
  On
  End

  return Val_long (((c.red & 0xFF00) << 8)
                   | (c.green & 0xFF00)
                   | ((c.blue & 0xFF00) >> 8));
}

value gr_moveto (value vx, value vy)
{
  XY;

  gr_check_open ();
  BeginOff
    MoveTo (Bx (x), By (y));
  On
    MoveTo (Wx (x), Wy (y));
  End
  return Val_unit;
}

value gr_current_point (value unit)
{
#pragma unused (unit)
  value result = alloc_tuple (2);
  Point p;
  
  gr_check_open ();
  BeginOff
    GetPen (&p);
  On
  End
  Field (result, 0) = Val_long (Bx (p.h));
  Field (result, 1) = Val_long (By (p.v));
  return result;
}

value gr_lineto (value vx, value vy)
{
  XY;
  
  gr_check_open ();
  BeginOff
    LineTo (Bx (x), By (y));
  On
    LineTo (Wx (x), Wy (y));
  End
  
  return Val_unit;
}

value gr_draw_arc (value *argv, int argc)
{
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
  
  gr_check_open ();
  BeginOff
    SetRect (&r, Bx (x-rx), By (y+ry), Bx (x+rx), By (y-ry));
    FrameArc (&r, qda1, qda2 - qda1);
  On
    SetRect (&r, Wx (x-rx), Wy (y+ry), Wx (x+rx), Wy (y-ry));
    FrameArc (&r, qda1, qda2 - qda1);
  End
  return Val_unit;
}

value gr_set_line_width (value vwidth)
{
  short width = Int_val (vwidth);

  if (width == 0) width = 1;  
  gr_check_open ();
  BeginOff
    PenSize (width, width);
  On
    PenSize (width, width);
  End
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
  End
  return Val_unit;
}

value gr_fill_poly (value vpoints)
{
  long i, n = Wosize_val (vpoints);
  PolyHandle p;
  
  #define Bxx(i) Bx (Field (Field (vpoints, (i)), 0))
  #define Byy(i) By (Field (Field (vpoints, (i)), 1))
  
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
  End
  KillPoly (p);
  return Val_unit;
}

value gr_fill_arc (value *argv, int argc)
{
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
  End
  return Val_unit;
}

value gr_draw_char (value vchr)
{
  char c = Int_val (vchr);
  
  gr_check_open ();
  BeginOff
    DrawChar (c);
  On
    DrawChar (c);
  End
  return Val_unit;
}

value gr_draw_string (value vstr)
{
  mlsize_t len = string_length (vstr);
  char *str = String_val (vstr);

  gr_check_open ();
  if (len > 32767) len = 32767;
  BeginOff
    DrawText (str, 0, len);
  On
    DrawText (str, 0, len);
  End
  return Val_unit;
}

value gr_set_font (value vfontname)
{
  Str255 pfontname;
  short fontnum;
  
  gr_check_open ();
  CopyCStringToPascal (String_val (vfontname), pfontname);
  GetFNum (pfontname, &fontnum);
  BeginOff
    TextFont (fontnum);
  On
    TextFont (fontnum);
  End
  return Val_unit;
}

value gr_set_text_size (value vsz)
{
  short sz = Int_val (vsz);
  
  gr_check_open ();
  BeginOff
    TextSize (sz);
  On
    TextSize (sz);
  End
  return Val_unit;
}

value gr_text_size (value vstr)
{
  mlsize_t len = string_length (vstr);
  char *str = String_val (vstr);
  value result = alloc_tuple (2);
  FontInfo info;
  long w, h;
  
  BeginOff
    GetFontInfo (&info);
    w = TextWidth (str, 0, len);
    h = info.ascent + info.descent;
  On
  End
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
    End
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
    End
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

static long oldx = SHRT_MAX - 1, oldy = SHRT_MAX - 1;

value gr_wait_event (value veventlist)
{
  int askmousedown = 0, askmouseup = 0, askkey = 0, askmotion = 0, askpoll = 0;
  long mouse_x, mouse_y;
  int button, keypressed;
  char key = 0;
  GraphEvent *evt;
  GrafPtr saveport;
  value result;
  Point pt;

  gr_check_open();
  GetPort (&saveport);
  SetPort (winGraphics);

  while (veventlist != Val_int (0)) {
    switch (Int_val(Field (veventlist, 0))) {
    case Button_down_val:  askmousedown = 1; break;
    case Button_up_val:    askmouseup = 1;   break;
    case Key_pressed_val:  askkey = 1;       break;
    case Mouse_motion_val: askmotion = 1;    break;
    case Poll_val:         askpoll = 1;      break;
    }
    veventlist = Field (veventlist, 1);
  }

  if (askpoll){
    GetMouse (&pt);
    mouse_x = Cx (pt.h);
    mouse_y = Cy (pt.v);
    button = Button ();
    evt = GraphQHead ();
    if (evt != NULL) {
      keypressed = 1;
      key = evt->key;
    }else{
      keypressed = 0;
    }
    goto gotevent;
  }
  if (askkey && (evt = GraphQPop ()) != NULL){
    mouse_x = evt->mouse_x;
    mouse_y = evt->mouse_y;
    button = evt->button;
    keypressed = 1;
    key = evt->key;
    goto gotevent;
  }
  /* Change from Caml coordinates to global QD coordinates. */
  pt.h = Wx (oldx);
  pt.v = Wy (oldy);
  LocalToGlobal (&pt);
  /* Restore the grafport now because GetAndProcessEvents may longjmp
     directly out of here.
  */
  SetPort (saveport);
  while (1){
    latestevent.valid = 0;
    enter_blocking_section ();
    Caml_working (0);
    GetAndProcessEvents (askmotion ? waitMove : waitEvent, pt.h, pt.v);
    Caml_working (1);
    leave_blocking_section ();
    if (askkey && (evt = GraphQPop ()) != NULL){
      mouse_x = evt->mouse_x;
      mouse_y = evt->mouse_y;
      button = evt->button;
      keypressed = 1;
      key = evt->key;
      goto gotevent;
    }
    if (latestevent.valid){
      if (askmousedown && latestevent.type == Button_down_val){
        mouse_x = latestevent.mouse_x;
        mouse_y = latestevent.mouse_y;
        button = 1;
        keypressed = 0;
        goto gotevent;
      }
      if (askmouseup && latestevent.type == Button_up_val){
        mouse_x = latestevent.mouse_x;
        mouse_y = latestevent.mouse_y;
        button = 0;
        keypressed = 0;
        goto gotevent;
      }
    }
    if (askmotion){
      SetPort (winGraphics);
      GetMouse (&pt);
      SetPort (saveport);
      mouse_x = Cx (pt.h);
      mouse_y = Cy (pt.v);
      if (mouse_x != oldx || mouse_y != oldy){
        button = Button ();
        keypressed = 0;
        goto gotevent;
      }
    }
  }
  gotevent:
  oldx = mouse_x;
  oldy = mouse_y;

  result = alloc_tuple (5);
  Field (result, 0) = Val_int (mouse_x);
  Field (result, 1) = Val_int (mouse_y);
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
