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
extern int grwhite, grblack;    /* Black and white pixels for X */
extern int grbackground;        /* Background color for X 
                                     (used for CAML color -1) */
extern Bool grdisplay_mode;     /* Display-mode flag */
extern Bool grremember_mode;    /* Remember-mode flag */
extern int grx, gry;            /* Coordinates of the current point */
extern int grcolor;             /* Current *CAML* drawing color (can be -1) */
extern XFontStruct * grfont;    /* Current font */
extern long grselected_events;  /* Events we are interested in */

extern Bool direct_rgb;
extern int byte_order;
extern int bitmap_unit;
extern int bits_per_pixel;

#define Wcvt(y) (grwindow.h - 1 - (y))
#define Bcvt(y) (grbstore.h - 1 - (y))
#define WtoB(y) ((y) + grbstore.h - grwindow.h)
#define BtoW(y) ((y) + grwindow.h - grbstore.h)
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#define DEFAULT_SCREEN_WIDTH 600
#define DEFAULT_SCREEN_HEIGHT 450
#define BORDER_WIDTH 2
#define WINDOW_NAME "Caml graphics"
#define ICON_NAME "Caml graphics"
#define DEFAULT_SELECTED_EVENTS \
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

extern void gr_fail(char *fmt, char *arg);
extern void gr_check_open(void);
extern unsigned long gr_pixel_rgb(int rgb);
extern int gr_rgb_pixel(long unsigned int pixel);
extern void gr_handle_event(XEvent *e);
extern void gr_init_color_cache(void);
extern void gr_init_direct_rgb_to_pixel(void);
extern value id_of_window( Window w );
