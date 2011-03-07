/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Jacob Navia, after Xavier Leroy                          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdio.h>
#include <windows.h>

struct canvas {
  int w, h;                     /* Dimensions of the drawable */
  HWND win;                     /* The drawable itself */
  HDC gc;                        /* The associated graphics context */
};

extern HWND grdisplay;     /* The display connection */
//extern int grscreen;            /* The screen number */
//extern Colormap grcolormap;     /* The color map */
//extern struct canvas grwindow;  /* The graphics window */
//extern struct canvas grbstore;  /* The pixmap used for backing store */
//extern int grwhite, grblack;    /* Black and white pixels for X */
//extern int grbackground;        /* Background color for X
//                                   (used for CAML color -1) */
extern COLORREF grbackground;
extern BOOL grdisplay_mode;     /* Display-mode flag */
extern BOOL grremember_mode;    /* Remember-mode flag */
extern int grx, gry;            /* Coordinates of the current point */
extern int grcolor;             /* Current *CAML* drawing color (can be -1) */
extern HFONT * grfont;          /* Current font */

extern BOOL direct_rgb;
extern int byte_order;
extern int bitmap_unit;
extern int bits_per_pixel;

#define Wcvt(y) (grwindow.height - 1 - (y))
#define Bcvt(y) (grwindow.height - 1 - (y))
#define WtoB(y) ((y) + WindowRect.bottom - grwindow.h)
//#define BtoW(y) ((y) + WindowRect.bottom - grbstore.h)

#define DEFAULT_SCREEN_WIDTH 1024
#define DEFAULT_SCREEN_HEIGHT 768
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
void gr_enqueue_char(unsigned char c);
void gr_init_color_cache(void);

// Windows specific definitions
extern RECT WindowRect;
extern int grCurrentColor;

typedef struct tagWindow {
        HDC gc;
        HDC gcBitmap;
        HWND hwnd;
        HBRUSH CurrentBrush;
        HPEN CurrentPen;
        DWORD CurrentColor;
        int width;
        int height;
        int grx;
        int gry;
        HBITMAP hBitmap;
        HFONT CurrentFont;
        int CurrentFontSize;
        HDC tempDC; // For image operations;
} GR_WINDOW;

extern GR_WINDOW grwindow;
HFONT CreationFont(char *name);
extern int MouseLbuttonDown,MouseMbuttonDown,MouseRbuttonDown;
extern HANDLE EventHandle;
extern int InspectMessages;
extern MSG msg;

