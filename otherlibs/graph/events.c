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

#include <signal.h>
#include "libgraph.h"
#include <alloc.h>

static unsigned char gr_queue[SIZE_QUEUE];
static int gr_head = 0;       /* position of next read */
static int gr_tail = 0;       /* position of next write */

#define QueueIsEmpty (gr_head == gr_tail)
#define QueueIsFull  (gr_head == gr_tail + 1)

void gr_enqueue_char(c)
     unsigned char c;
{
  if (QueueIsFull) return;
  gr_queue[gr_tail] = c;
  gr_tail++;
  if (gr_tail >= SIZE_QUEUE) gr_tail = 0;
}

value gr_wait_event(eventlist)
     value eventlist;
{
  value res;
  int mask;
  Bool poll;
  int mouse_x, mouse_y, button, key;
  Window rootwin, childwin;
  int root_x, root_y, win_x, win_y;
  unsigned int modifiers;
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsig;
#else
  void (*oldsig)();
#endif
  XEvent event;

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
  mouse_x = -1;
  mouse_y = -1;
  button = 0;
  key = 0x100;

  if (poll) {
    if (XQueryPointer(grdisplay, grwindow.win,
                      &rootwin, &childwin,
                      &root_x, &root_y, &win_x, &win_y,
                      &modifiers)) {
      mouse_x = win_x;
      mouse_y = win_y;
    }
    button = modifiers & Button1Mask;
    if (!QueueIsEmpty) key = gr_queue[gr_head];
  } else {
    if ((mask & KeyPressMask) && !QueueIsEmpty) {
      key = gr_queue[gr_head];
      gr_head++;
      if (gr_head >= SIZE_QUEUE) gr_head = 0;
    } else {
#ifdef POSIX_SIGNALS
      sigact.sa_handler = SIG_IGN;
      sigaction(EVENT_SIGNAL, &sigact, &oldsig);
#else
      oldsig = signal(EVENT_SIGNAL, SIG_IGN);
#endif
      XSelectInput(grdisplay, grwindow.win, DEFAULT_EVENT_MASK | mask);
    again:
      XNextEvent(grdisplay, &event);
      switch(event.type) {
      case ButtonPress:
      case ButtonRelease:
        mouse_x = event.xbutton.x;
        mouse_y = event.xbutton.y;
        button = event.type == ButtonPress;
        break;
      case MotionNotify:
        mouse_x = event.xmotion.x;
        mouse_y = event.xmotion.y;
        button = event.xmotion.state & Button1Mask;
        break;
      case KeyPress:
        gr_handle_simple_event(&event);
        /* Some KeyPress events do not enqueue any characters (e.g. pressing
           Ctrl), because they expand via XLookupString to the empty string.
           Therefore we need to check again whether the char queue is empty. */
        if ((mask & KeyPressMask) == 0 || QueueIsEmpty) goto again;
        key = gr_queue[gr_head];
        gr_head++;
        if (gr_head >= SIZE_QUEUE) gr_head = 0;
        break;
      default:
        gr_handle_simple_event(&event);
        goto again;
      }
#ifdef POSIX_SIGNALS
      sigaction(EVENT_SIGNAL, &oldsig, NULL);
#else
      signal(EVENT_SIGNAL, oldsig);
#endif
      XSelectInput(grdisplay, grwindow.win, DEFAULT_EVENT_MASK);
      XFlush(grdisplay);
    }
  }
  res = alloc_tuple(5);
  Field(res, 0) = Val_int(mouse_x);
  Field(res, 1) = Val_int(mouse_y == -1 ? -1 : Wcvt(mouse_y));
  Field(res, 2) = Val_bool(button);
  Field(res, 3) = Val_bool(key != 0x100);
  Field(res, 4) = Val_int(key & 0xFF);
  return res;
}
