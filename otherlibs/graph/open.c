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

#include <fcntl.h>
#include <signal.h>
#include "libgraph.h"
#include <alloc.h>
#include <callback.h>
#include <fail.h>
#include <memory.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_SETITIMER
#include <sys/time.h>
#endif

Display * grdisplay = NULL;
int grscreen;
Colormap grcolormap;
int grwhite, grblack, grbackground;
struct canvas grwindow;
struct canvas grbstore;
Bool grdisplay_mode;
Bool grremember_mode;
int grx, gry;
int grcolor;
extern XFontStruct * grfont;

static Bool gr_initialized = False;

static int gr_error_handler(Display *display, XErrorEvent *error);
static int gr_ioerror_handler(Display *display);
value gr_clear_graph(void);

value gr_open_graph(value arg)
{
  char display_name[64], geometry_spec[64];
  char * p, * q;
  XSizeHints hints;
  int ret;
  XEvent event;
  int x, y, w, h;
  XWindowAttributes attributes;

  if (gr_initialized) {
    gr_clear_graph();
  } else {

    /* Parse the argument */
    for (p = String_val(arg), q = display_name; *p != 0 && *p != ' '; p++)
      if (q < display_name + sizeof(display_name) - 1) *q++ = *p;
    *q = 0;
    while (*p == ' ') p++;
    for (q = geometry_spec; *p != 0; p++)
      if (q < geometry_spec + sizeof(geometry_spec) - 1) *q++ = *p;
    *q = 0;

    /* Open the display */
    if (grdisplay == NULL) {
      grdisplay = XOpenDisplay(display_name);
      if (grdisplay == NULL)
        gr_fail("Cannot open display %s", XDisplayName(display_name));
      grscreen = DefaultScreen(grdisplay);
      grblack = BlackPixel(grdisplay, grscreen);
      grwhite = WhitePixel(grdisplay, grscreen);
      grbackground = grwhite;
      grcolormap = DefaultColormap(grdisplay, grscreen);
    }

    /* Set up the error handlers */
    XSetErrorHandler(gr_error_handler);
    XSetIOErrorHandler(gr_ioerror_handler);

    /* Parse the geometry specification */
    hints.x = 0;
    hints.y = 0;
    hints.width = DEFAULT_SCREEN_WIDTH;
    hints.height = DEFAULT_SCREEN_HEIGHT;
    hints.flags = PPosition | PSize;
    hints.win_gravity = 0;

    ret = XWMGeometry(grdisplay, grscreen, geometry_spec, "", BORDER_WIDTH,
                      &hints, &x, &y, &w, &h, &hints.win_gravity);
    if (ret & (XValue | YValue)) {
      hints.x = x; hints.y = y; hints.flags |= USPosition;
    }
    if (ret & (WidthValue | HeightValue)) {
      hints.width = w; hints.height = h; hints.flags |= USSize;
    }

    /* Initial drawing color is black */
    grcolor = 0; /* CAML COLOR */

    /* Create the on-screen window */
    grwindow.w = hints.width;
    grwindow.h = hints.height;
    grwindow.win =
      XCreateSimpleWindow(grdisplay, DefaultRootWindow(grdisplay),
                          hints.x, hints.y, hints.width, hints.height,
                          BORDER_WIDTH, grblack, grbackground);
    XSetStandardProperties(grdisplay, grwindow.win, WINDOW_NAME, ICON_NAME,
                           None, NULL, 0, &hints);
    grwindow.gc = XCreateGC(grdisplay, grwindow.win, 0, NULL);
    XSetBackground(grdisplay, grwindow.gc, grbackground);
    XSetForeground(grdisplay, grwindow.gc, grblack);

    /* Require exposure, resize and keyboard events */
    XSelectInput(grdisplay, grwindow.win, DEFAULT_EVENT_MASK);

    /* Map the window on the screen and wait for the first Expose event */
    XMapWindow(grdisplay, grwindow.win);
    do { XNextEvent(grdisplay, &event); } while (event.type != Expose);

    /* Get the actual window dimensions */

    XGetWindowAttributes(grdisplay, grwindow.win, &attributes);
    grwindow.w = attributes.width;
    grwindow.h = attributes.height;

    /* Create the pixmap used for backing store */
    grbstore.w = grwindow.w;
    grbstore.h = grwindow.h;
    grbstore.win =
      XCreatePixmap(grdisplay, grwindow.win, grbstore.w, grbstore.h,
                    XDefaultDepth(grdisplay, grscreen));
    grbstore.gc = XCreateGC(grdisplay, grbstore.win, 0, NULL);
    XSetBackground(grdisplay, grbstore.gc, grbackground);

    /* Clear the pixmap */
    XSetForeground(grdisplay, grbstore.gc, grbackground);
    XFillRectangle(grdisplay, grbstore.win, grbstore.gc,
                   0, 0, grbstore.w, grbstore.h);
    XSetForeground(grdisplay, grbstore.gc, grblack);

    /* Set the display and remember modes on */
    grdisplay_mode = True ;
    grremember_mode = True ;

    /* The global data structures are now correctly initialized.
       In particular, gr_sigio_handler can now handle events safely. */
    gr_initialized = True;

    /* If possible, request that system calls be restarted after
       the EVENT_SIGNAL signal. */
#ifdef POSIX_SIGNALS
#ifdef SA_RESTART
    { struct sigaction action;
      sigaction(EVENT_SIGNAL, NULL, &action);
      action.sa_flags |= SA_RESTART;
      sigaction(EVENT_SIGNAL, &action, NULL);
    }
#endif
#endif

#ifdef USE_ASYNC_IO
    /* If BSD-style asynchronous I/O are supported:
       arrange for I/O on the connection to trigger the SIGIO signal */
    ret = fcntl(ConnectionNumber(grdisplay), F_GETFL, 0);
    fcntl(ConnectionNumber(grdisplay), F_SETFL, ret | FASYNC);
    fcntl(ConnectionNumber(grdisplay), F_SETOWN, getpid());
#endif
  }
#ifdef USE_INTERVAL_TIMER
  /* If BSD-style interval timers are provided, use the real-time timer
     to poll events. */
  { struct itimerval it;
    it.it_interval.tv_sec = 0;
    it.it_interval.tv_usec = 250000;
    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 250000;
    setitimer(ITIMER_REAL, &it, NULL);
  }
#endif
#ifdef USE_ALARM
  /* The poor man's solution: use alarm to poll events. */
  alarm(1);
#endif
  /* Position the current point at origin */
  grx = 0;
  gry = 0;
  /* Reset the color cache */
  gr_init_color_cache();
  return Val_unit;
}

value gr_close_graph(void)
{
  if (gr_initialized) {
#ifdef USE_INTERVAL_TIMER
    struct itimerval it;
    it.it_value.tv_sec = 0;
    it.it_value.tv_usec = 0;
    setitimer(ITIMER_REAL, &it, NULL);
#endif
    gr_initialized = False;
    if (grfont != NULL) { XFreeFont(grdisplay, grfont); grfont = NULL; }
    XFreeGC(grdisplay, grwindow.gc);
    XDestroyWindow(grdisplay, grwindow.win);
    XFreeGC(grdisplay, grbstore.gc);
    XFreePixmap(grdisplay, grbstore.win);
    XFlush(grdisplay);
  }
  return Val_unit;
}

value id_of_window(Window win)
{
  char tmp[256];

  sprintf(tmp, "%lu", (unsigned long)win);
  return copy_string( tmp );
}

value gr_window_id(void)
{
  gr_check_open();
  return id_of_window(grwindow.win);
}

value gr_clear_graph(void)
{
  gr_check_open();
  if(grremember_mode) {
    XSetForeground(grdisplay, grbstore.gc, grwhite);
    XFillRectangle(grdisplay, grbstore.win, grbstore.gc,
                   0, 0, grbstore.w, grbstore.h);
    XSetForeground(grdisplay, grbstore.gc, grcolor);
  }
  if(grdisplay_mode) {
    XSetForeground(grdisplay, grwindow.gc, grwhite);
    XFillRectangle(grdisplay, grwindow.win, grwindow.gc,
                   0, 0, grwindow.w, grwindow.h);
    XSetForeground(grdisplay, grwindow.gc, grcolor);
    XFlush(grdisplay);
  }
  gr_init_color_cache();
  return Val_unit;
}

value gr_size_x(void)
{
  gr_check_open();
  return Val_int(grwindow.w);
}

value gr_size_y(void)
{
  gr_check_open();
  return Val_int(grwindow.h);
}

value gr_synchronize(void)
{
  gr_check_open();
  XCopyArea(grdisplay, grbstore.win, grwindow.win, grwindow.gc,
            0, grbstore.h - grwindow.h,
            grwindow.w, grwindow.h,
            0, 0);
  XFlush(grdisplay);
  return Val_unit ;
}

value gr_display_mode(value flag)
{
  grdisplay_mode = Bool_val (flag);
  return Val_unit ;
}

value gr_remember_mode(value flag)
{
  grremember_mode = Bool_val(flag);
  return Val_unit ;
}

/* The gr_sigio_handler is called via the signal machinery in the bytecode
   interpreter. The signal system ensures that this function will be
   called either between two bytecode instructions, or during a blocking
   primitive. In either case, not in the middle of an Xlib call.
   (There is no blocking primitives in this library, not even
   wait_next_event, for various reasons.) */

void gr_handle_simple_event(XEvent *e);

value gr_sigio_signal(value unit)
{
  return Val_int(EVENT_SIGNAL);
}

value gr_sigio_handler(void)
{
  XEvent grevent;

  if (gr_initialized) {
    while (XCheckMaskEvent(grdisplay, -1 /*all events*/, &grevent))
      gr_handle_simple_event(&grevent);
  }
#ifdef USE_ALARM
  alarm(1);
#endif
  return Val_unit;
}

void gr_handle_simple_event(XEvent *e)
{
  switch (e->type) {

  case Expose:
    XCopyArea(grdisplay, grbstore.win, grwindow.win, grwindow.gc,
              e->xexpose.x, e->xexpose.y + grbstore.h - grwindow.h,
              e->xexpose.width, e->xexpose.height,
              e->xexpose.x, e->xexpose.y);
    XFlush(grdisplay);
    break;

  case ConfigureNotify:
    grwindow.w = e->xconfigure.width;
    grwindow.h = e->xconfigure.height;
    if (grwindow.w > grbstore.w || grwindow.h > grbstore.h) {

      /* Allocate a new backing store large enough to accomodate
         both the old backing store and the current window. */
      struct canvas newbstore;
      newbstore.w = max(grwindow.w, grbstore.w);
      newbstore.h = max(grwindow.h, grbstore.h);
      newbstore.win =
        XCreatePixmap(grdisplay, grwindow.win, newbstore.w, newbstore.h,
                      XDefaultDepth(grdisplay, grscreen));
      newbstore.gc = XCreateGC(grdisplay, newbstore.win, 0, NULL);
      XSetBackground(grdisplay, newbstore.gc, grwhite);
      XSetForeground(grdisplay, newbstore.gc, grwhite);
      XFillRectangle(grdisplay, newbstore.win, newbstore.gc,
                     0, 0, newbstore.w, newbstore.h);
      XSetForeground(grdisplay, newbstore.gc, grcolor);
      if (grfont != NULL)
        XSetFont(grdisplay, newbstore.gc, grfont->fid);

      /* Copy the old backing store into the new one */
      XCopyArea(grdisplay, grbstore.win, newbstore.win, newbstore.gc,
                0, 0, grbstore.w, grbstore.h, 0, newbstore.h - grbstore.h);

      /* Free the old backing store */
      XFreeGC(grdisplay, grbstore.gc);
      XFreePixmap(grdisplay, grbstore.win);

      /* Use the new backing store */
      grbstore = newbstore;
      XFlush(grdisplay);
    }
    break;

  case MappingNotify:
    XRefreshKeyboardMapping(&(e->xmapping));
    break;

  case KeyPress:
    { KeySym thekey;
      char keytxt[256];
      int nchars;
      char * p;
      nchars = XLookupString(&(e->xkey), keytxt, sizeof(keytxt), &thekey, 0);
      for (p = keytxt; nchars > 0; p++, nchars--) gr_enqueue_char(*p);
      break;
    }
  }
}

/* Processing of graphic errors */

static value * graphic_failure_exn = NULL;

void gr_fail(char *fmt, char *arg)
{
  char buffer[1024];

  if (graphic_failure_exn == NULL) {
    graphic_failure_exn = caml_named_value("Graphics.Graphic_failure");
    if (graphic_failure_exn == NULL)
      invalid_argument("Exception Graphics.Graphic_failure not initialized, must link graphics.cma");
  }
  sprintf(buffer, fmt, arg);
  raise_with_string(*graphic_failure_exn, buffer);
}

void gr_check_open(void)
{
  if (!gr_initialized) gr_fail("graphic screen not opened", NULL);
}

static int gr_error_handler(Display *display, XErrorEvent *error)
{
  char errmsg[512];
  XGetErrorText(error->display, error->error_code, errmsg, sizeof(errmsg));
  gr_fail("Xlib error: %s", errmsg);
  return 0;
}

static int gr_ioerror_handler(Display *display)
{
  gr_fail("fatal I/O error", NULL);
  return 0;
}
