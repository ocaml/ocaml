/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <mlvalues.h>

struct canvas {
  int w, h;                     /* Dimensions of the drawable */
  Drawable win;                 /* The drawable itself */
  GC gc;                        /* The associated graphics context */
};

extern Display * grdisplay;     /* The display connection */
extern int grscreen;            /* The screen number */
extern Colormap grcolormap;     /* The color map */
extern struct canvas grwindow;  /* The graphics window */
extern struct canvas grbstore;  /* The pixmap used for backing store */
extern int grwhite, grblack;    /* Black and white pixels */
extern int grx, gry;            /* Coordinates of the current point */
extern unsigned long grcolor;   /* Current drawing color */
extern XFontStruct * grfont;    /* Current font */

#define Wcvt(y) (grwindow.h - 1 - (y))
#define Bcvt(y) (grbstore.h - 1 - (y))
#define WtoB(y) ((y) + grbstore.h - grwindow.h)
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#define DEFAULT_SCREEN_WIDTH 600
#define DEFAULT_SCREEN_HEIGHT 450
#define BORDER_WIDTH 2
#define WINDOW_NAME "Caml graphics"
#define ICON_NAME "Caml graphics"
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

void gr_fail(char *fmt, char *arg);
void gr_check_open(void);
unsigned long gr_pixel_rgb(int rgb);
int gr_rgb_pixel(long unsigned int pixel);
void gr_handle_simple_event(XEvent *e);
void gr_enqueue_char(unsigned char c);
void gr_init_color_cache(void);
