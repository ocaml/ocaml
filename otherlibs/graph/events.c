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

#include <signal.h>
#include "libgraph.h"
#include <alloc.h>
#include <signals.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

struct event_data {
  short kind;
  short mouse_x, mouse_y;
  unsigned char button;
  unsigned char key;
};

static struct event_data gr_queue[SIZE_QUEUE];
static unsigned int gr_head = 0;       /* position of next read */
static unsigned int gr_tail = 0;       /* position of next write */

#define QueueIsEmpty (gr_tail == gr_head)
#define QueueIsFull  ((gr_tail + 1) % SIZE_QUEUE == gr_head)

static void gr_enqueue_event(int kind, int mouse_x, int mouse_y,
                             int button, int key)
{
  struct event_data * ev;

  if (QueueIsFull) return;
  ev = &(gr_queue[gr_tail]);
  ev->kind = kind;
  ev->mouse_x = mouse_x;
  ev->mouse_y = mouse_y;
  ev->button = (button != 0);
  ev->key = key;
  gr_tail = (gr_tail + 1) % SIZE_QUEUE;
}

#define BUTTON_STATE(state) \
  ((state) & (Button1Mask|Button2Mask|Button3Mask|Button4Mask|Button5Mask))

void gr_handle_event(XEvent * event)
{
  switch (event->type) {

  case Expose:
    XCopyArea(grdisplay, grbstore.win, grwindow.win, grwindow.gc,
              event->xexpose.x, event->xexpose.y + grbstore.h - grwindow.h,
              event->xexpose.width, event->xexpose.height,
              event->xexpose.x, event->xexpose.y);
    XFlush(grdisplay);
    break;

  case ConfigureNotify:
    grwindow.w = event->xconfigure.width;
    grwindow.h = event->xconfigure.height;
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
    XRefreshKeyboardMapping(&(event->xmapping));
    break;

  case KeyPress:
    { KeySym thekey;
      char keytxt[256];
      int nchars;
      char * p;
      nchars = XLookupString(&(event->xkey), keytxt, sizeof(keytxt),
                             &thekey, 0);
      for (p = keytxt; nchars > 0; p++, nchars--)
        gr_enqueue_event(event->type, event->xkey.x, event->xkey.y,
                         BUTTON_STATE(event->xkey.state), *p);
      break;
    }

  case ButtonPress:
  case ButtonRelease:
    gr_enqueue_event(event->type, event->xbutton.x, event->xbutton.y,
                     event->type == ButtonPress, 0);
    break;

  case MotionNotify:
    gr_enqueue_event(event->type, event->xmotion.x, event->xmotion.y,
                     BUTTON_STATE(event->xmotion.state), 0);
    break;
  }
}

static value gr_wait_allocate_result(int mouse_x, int mouse_y, int button,
                                     int keypressed, int key)
{
  value res = alloc_small(5, 0);
  Field(res, 0) = Val_int(mouse_x);
  Field(res, 1) = Val_int(mouse_y == -1 ? -1 : Wcvt(mouse_y));
  Field(res, 2) = Val_bool(button);
  Field(res, 3) = Val_bool(keypressed);
  Field(res, 4) = Val_int(key & 0xFF);
  return res;
}

static value gr_wait_event_poll(void)
{
  int mouse_x, mouse_y, button, key, keypressed;
  Window rootwin, childwin;
  int root_x, root_y, win_x, win_y;
  unsigned int modifiers;
  unsigned int i;

  if (XQueryPointer(grdisplay, grwindow.win,
                    &rootwin, &childwin,
                    &root_x, &root_y, &win_x, &win_y,
                    &modifiers)) {
    mouse_x = win_x;
    mouse_y = win_y;
  } else {
    mouse_x = -1;
    mouse_y = -1;
  }
  button = modifiers & (Button1Mask | Button2Mask | Button3Mask 
                          | Button4Mask | Button5Mask);
  /* Look inside event queue for pending KeyPress events */
  key = 0;
  keypressed = False;
  for (i = gr_head; i != gr_tail; i = (i + 1) % SIZE_QUEUE) {
    if (gr_queue[i].kind == KeyPress) {
      keypressed = True;
      key = gr_queue[i].key;
      break;
    }
  }
  return gr_wait_allocate_result(mouse_x, mouse_y, button, keypressed, key);
}

static value gr_wait_event_in_queue(long mask)
{
  struct event_data * ev;
  /* Pop events in queue until one matches mask. */
  while (gr_head != gr_tail) {
    ev = &(gr_queue[gr_head]);
    gr_head = (gr_head + 1) % SIZE_QUEUE;
    if ((ev->kind == KeyPress && (mask & KeyPressMask))
        || (ev->kind == ButtonPress && (mask & ButtonPressMask))
        || (ev->kind == ButtonRelease && (mask & ButtonReleaseMask))
        || (ev->kind == MotionNotify && (mask & PointerMotionMask)))
      return gr_wait_allocate_result(ev->mouse_x, ev->mouse_y,
                                     ev->button, ev->kind == KeyPress,
                                     ev->key);
  }
  return Val_false;
}

static value gr_wait_event_blocking(long mask)
{
#ifdef POSIX_SIGNALS
  sigset_t sigset;
#else
  void (*oldsig)();
#endif
  XEvent event;
  fd_set readfds;
  value res;

  /* First see if we have a matching event in the queue */
  res = gr_wait_event_in_queue(mask);
  if (res != Val_false) return res;

  /* Increase the selected events if required */
  if ((mask & ~grselected_events) != 0) {
    grselected_events |= mask;
    XSelectInput(grdisplay, grwindow.win, grselected_events);
  }

  /* Block or deactivate the EVENT signal */
#ifdef POSIX_SIGNALS
  sigemptyset(&sigset);
  sigaddset(&sigset, EVENT_SIGNAL);
  sigprocmask(SIG_BLOCK, &sigset, NULL);
#else
  oldsig = signal(EVENT_SIGNAL, SIG_IGN);
#endif

  /* Replenish our event queue from that of X11 */
  while (1) {
    if (XCheckMaskEvent(grdisplay, -1 /*all events*/, &event)) {
      /* One event available: add it to our queue */
      gr_handle_event(&event);
      /* See if we now have a matching event */
      res = gr_wait_event_in_queue(mask);
      if (res != Val_false) break;
    } else {
      /* No event available: block on input socket until one is */
      FD_ZERO(&readfds);
      FD_SET(ConnectionNumber(grdisplay), &readfds);
      enter_blocking_section();
      select(FD_SETSIZE, &readfds, NULL, NULL, NULL);
      leave_blocking_section();
    }
  }

  /* Restore the EVENT signal to its initial state */
#ifdef POSIX_SIGNALS
  sigprocmask(SIG_UNBLOCK, &sigset, NULL);
#else
  signal(EVENT_SIGNAL, oldsig);
#endif

  /* Return result */
  return res;
}

value gr_wait_event(value eventlist) /* ML */
{
  int mask;
  Bool poll;

  gr_check_open();
  mask = 0;
  poll = False;
  while (eventlist != Val_int(0)) {
    switch (Int_val(Field(eventlist, 0))) {
    case 0:                     /* Button_down */
      mask |= ButtonPressMask | OwnerGrabButtonMask; break;
    case 1:                     /* Button_up */
      mask |= ButtonReleaseMask | OwnerGrabButtonMask; break;
    case 2:                     /* Key_pressed */
      mask |= KeyPressMask; break;
    case 3:                     /* Mouse_motion */
      mask |= PointerMotionMask; break;
    case 4:                     /* Poll */
      poll = True; break;
    }
    eventlist = Field(eventlist, 1);
  }
  if (poll)
    return gr_wait_event_poll();
  else
    return gr_wait_event_blocking(mask);
}
