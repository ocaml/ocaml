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

#include <stdio.h>
#include <Xlib.h>
#include <Xutil.h>
#include <mlvalues.h>

struct canvas {
  int w, h;                     /* Dimensions of the drawable */
  Drawable win;                 /* The drawable itself */
  GC gc;                        /* The associated graphics context */
};

extern Display * grdisplay;     /* The display connection */
int grscreen;                   /* The screen number */
Colormap grcolormap;            /* The color map */
struct canvas grwindow;         /* The graphics window */
struct canvas grbstore;         /* The pixmap used for backing store */
int grwhite, grblack;           /* Black and white pixels */
int grx, gry;                   /* Coordinates of the current point */
unsigned long grcolor;          /* Current drawing color */
extern XFontStruct * grfont;    /* Current font */

#define Wcvt(y) (grwindow.h - 1 - (y))
#define Bcvt(y) (grbstore.h - 1 - (y))
#define WtoB(y) ((y) + grbstore.h - grwindow.h)
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#define DEFAULT_SCREEN_WIDTH 600
#define DEFAULT_SCREEN_HEIGHT 450
#define BORDER_WIDTH 2
#define WINDOW_NAME "Caml Light graphics"
#define ICON_NAME "Caml Light graphics"
#define DEFAULT_EVENT_MASK \
          (ExposureMask | KeyPressMask | StructureNotifyMask)
#define DEFAULT_FONT "fixed"
#define SIZE_QUEUE 256

/* To handle events asynchronously */
#ifdef HAS_ASYNC_IO
#define USE_ASYNC_IO
#define EVENT_SIGNAL SIGIO
#else
#ifdef HAS_SETITIMER
#define USE_INTERVAL_TIMER
#define EVENT_SIGNAL SIGALRM
#else
#define USE_ALARM
#define EVENT_SIGNAL SIGALRM
#endif
#endif

void gr_fail();
void gr_check_open();
unsigned long gr_pixel_rgb();
int gr_rgb_pixel();
void gr_handle_simple_event();
void gr_enqueue_char();
void gr_init_color_cache();
