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

#include "main.h"

WEScrollUPP scrollFollowUPP;
static ControlActionUPP scrollUPP, scrollGraphUPP;

static long scroll_step = 1;

/* Bring destRect in sync with the scroll bars. */
static void AdjustView (WStatusH st)
{
  WEReference we = (*st)->we;
  ControlHandle hbar = (*st)->scrollbars[H];
  ControlHandle vbar = (*st)->scrollbars[V];
  LongRect view, dest;
  long dx, dy;

  Assert (hbar != NULL && vbar != NULL);
  if ((*st)->kind != kWinGraphics){
    Assert (we != NULL);
    WEGetViewRect (&view, we);
    WEGetDestRect (&dest, we);
    dx = view.left - dest.left - LCGetValue (hbar);
    dy = view.top - dest.top - LCGetValue (vbar);
    WEScroll (dx, dy, we);
  }else{
    dx = (*st)->viewrect.left - (*st)->destrect.left - LCGetValue (hbar);
    dy = (*st)->viewrect.top - (*st)->destrect.top - LCGetValue (vbar);
    GraphScroll (dx, dy);
  }
}

/* Recompute the max values and the thumb positions. */
void AdjustScrollBars (WindowPtr w)
{
  GrafPtr saveport;
  WStatusH st;
  LongRect view, dest;
  long xmax, xval, ymax, yval;
  long h;

  GetPort (&saveport);
  SetPort (w);

  st = WinGetStatus (w);
  Assert (st != NULL);
  if ((*st)->kind == kWinGraphics){
    view = (*st)->viewrect;
    dest = (*st)->destrect;
  }else{
    WEGetViewRect (&view, (*st)->we);
    WEGetDestRect (&dest, (*st)->we);
  }

  yval = view.top - dest.top;
  ymax = yval + (dest.bottom - view.bottom);
  if (ymax < 0) ymax = 0;

  /* round up to nearest line_height */
  h = (*st)->line_height;
  ymax = (ymax + h - 1) / h * h;

  LCSetMax ((*st)->scrollbars[V], ymax);
  LCSetValue ((*st)->scrollbars[V], yval);

  xval = view.left - dest.left;
  xmax = xval + (dest.right - view.right);
  if (xmax < 0) xmax = 0;
  LCSetMax ((*st)->scrollbars[H], xmax);
  LCSetValue ((*st)->scrollbars[H], xval);

  if (xval > xmax || yval > ymax) AdjustView (st);

  SetPort (saveport);
}

/* Callback procedure for auto-scrolling the text. (called by WASTE) */
static pascal void Follow (WEReference we)
{
  WindowPtr w;
  OSErr err;

  err = WEGetInfo (weRefCon, &w, we);
  Assert (err == noErr);
  AdjustScrollBars (w);
}

/* Callback procedure for scrolling the text. (called by the Control Manager) */
static pascal void Scroll (ControlHandle bar, ControlPartCode partcode)
{
  long value;

  if (partcode == kControlNoPart) return;
  value = LCGetValue (bar);
  if (value < LCGetMax (bar) && scroll_step > 0
      || value > 0 && scroll_step < 0){
    LCSetValue (bar, value + scroll_step);
    AdjustView (WinGetStatus (FrontWindow ()));
  }
}

/* Callback procedure for scrolling the graphics. */
static pascal void ScrollGraph (ControlHandle bar, ControlPartCode partcode)
{
  long value;

  if (partcode == kControlNoPart) return;
  value = LCGetValue (bar);
  if (value < LCGetMax (bar) && scroll_step > 0
      || value > 0 && scroll_step < 0){
    LCSetValue (bar, value + scroll_step);
    AdjustView (WinGetStatus (FrontWindow ()));
  }
}

OSErr InitialiseScroll (void)
{
  scrollFollowUPP = NewWEScrollProc (Follow);
  scrollUPP = NewControlActionProc (Scroll);
  scrollGraphUPP = NewControlActionProc (ScrollGraph);
  return noErr;
}

/* Calculate the contents rectangle for a text window with scrollbars. */
void ScrollCalcText (WindowPtr w, Rect *r)
{
  *r = w->portRect;
  r->bottom -= kScrollBarWidth;
  r->right -= kScrollBarWidth;
  InsetRect (r, kTextMarginH, kTextMarginV);
}

/* Calculate the contents rectangle for the graphics window. */
void ScrollCalcGraph (WindowPtr w, Rect *r)
{
  *r = w->portRect;
  r->bottom -= kScrollBarWidth;
  r->right -= kScrollBarWidth;
}

void ScrollDoClick (WindowPtr w, Point where, EventModifiers mods)
{
  switch (WinGetKind (w)){
  case kWinToplevel:
  case kWinDocument: {
    WEReference we = WinGetWE (w);
    WStatusH st = WinGetStatus (w);
    LongRect view;
    ControlPartCode partcode;
    ControlHandle bar;
    long scrolldelta, pagesize;

    Assert (we != NULL && st != NULL);
    WEGetViewRect (&view, we);
    partcode = FindControl (where, w, &bar);
    if (bar == (*st)->scrollbars[V]){
      pagesize = view.bottom - view.top;
      scrolldelta = (*st)->line_height;
    }else if (bar == (*st)->scrollbars [H]){
      pagesize = view.right - view.left;
      scrolldelta = kHorizScrollDelta;
    }else{
      return;
    }
    switch (partcode){
    case kControlIndicatorPart:
      TrackControl (bar, where, NULL);
      LCSynch (bar);
      AdjustView (st);
      return;
    case kControlUpButtonPart:
      scroll_step = - (mods & optionKey ? 1 : scrolldelta);
      break;
    case kControlDownButtonPart:
      scroll_step = + (mods & optionKey ? 1 : scrolldelta);
      break;
    case kControlPageUpPart:
      scroll_step = - (pagesize - scrolldelta) / scrolldelta * scrolldelta;
      break;
    case kControlPageDownPart:
      scroll_step = + (pagesize - scrolldelta) / scrolldelta * scrolldelta;
      break;
    }
    TrackControl (bar, where, scrollUPP);
    break;
  }
  case kWinGraphics: {
    WStatusH st = WinGetStatus (w);
    ControlPartCode partcode;
    ControlHandle bar;
    long scrolldelta, pagesize;

    Assert (st != NULL);
    partcode = FindControl (where, w, &bar);
    scrolldelta = kGraphScrollDelta;
    if (bar == (*st)->scrollbars[V]){
      pagesize = (*st)->viewrect.bottom - (*st)->viewrect.top;
    }else if (bar == (*st)->scrollbars [H]){
      pagesize = (*st)->viewrect.right - (*st)->viewrect.left;
    }else{
      return;
    }
    switch (partcode){
    case kControlIndicatorPart:
      TrackControl (bar, where, NULL);
      LCSynch (bar);
      AdjustView (st);
      return;
    case kControlUpButtonPart:
      scroll_step = - (mods & optionKey ? 1 : scrolldelta);
      break;
    case kControlDownButtonPart:
      scroll_step = + (mods & optionKey ? 1 : scrolldelta);
      break;
    case kControlPageUpPart:
      scroll_step = - (pagesize - scrolldelta) / scrolldelta * scrolldelta;
      break;
    case kControlPageDownPart:
      scroll_step = + (pagesize - scrolldelta) / scrolldelta * scrolldelta;
      break;
    }
    TrackControl (bar, where, scrollGraphUPP);
    break;
  }
  case kWinPrefs:
  case kWinAbout:
  case kWinClipboard:
  default:
    Assert (0); /* These windows have no scroll bars. */
    break;
  }
}

/* Calculate and set the position of the scroll bars for w.
   Draw the scroll bars and the grow icon, and validate their region.
   Where applicable, this function must be called after WinWEResize or
   WinGraphResize.
 */
void ScrollNewSize (WindowPtr w)
{
  Rect port = w->portRect;
  WStatusH st = WinGetStatus (w);
  Rect r;
  ControlHandle bar;
  GrafPtr saveport;

  Assert (st != NULL);

  GetPort (&saveport);
  SetPort (w);

  bar = (*st)->scrollbars[H];
  r.left = port.left - 1;
  r.right = port.right - kScrollBarWidth + 1;
  r.top = port.bottom - kScrollBarWidth;
  r.bottom = port.bottom + 1;
  HideControl (bar);                          /* Invalidates the rectangle */
  MoveControl (bar, r.left, r.top);
  SizeControl (bar, r.right - r.left, r.bottom - r.top);
  /* Only show the scrollbar if the window is active. */
  if (FrontWindow () == w){
    ValidRect (&r);
    ShowControl (bar);
  }

  bar = (*st)->scrollbars[V];
  r.left = port.right - kScrollBarWidth;
  r.right = port.right + 1;
  r.top = port.top - 1;
  r.bottom = port.bottom - kScrollBarWidth + 1;
  HideControl (bar);                          /* Invalidates the rectangle */
  MoveControl (bar, r.left, r.top);
  SizeControl (bar, r.right - r.left, r.bottom - r.top);
  /* Only show the scrollbar if the window is active. */
  if (FrontWindow () == w){
    ValidRect (&r);
    ShowControl (bar);
  }
  
  r = w->portRect;
  r.left = r.right - kScrollBarWidth;
  r.top = r.bottom - kScrollBarWidth;
  ValidRect (&r);
  DrawGrowIcon (w);

  AdjustScrollBars (w);

  SetPort (saveport);
}

/* Return 1 if the vertical scroll bar is at its max setting, 0 otherwise.
   (With 1/2 line fudge factor.)
*/
int ScrollAtEnd (WindowPtr w)
{
  WStatusH st = WinGetStatus (w);
  long val, max;
  
  Assert (st != NULL);
  val = LCGetValue ((*st)->scrollbars[V]);
  max = LCGetMax ((*st)->scrollbars[V]);
  return (val >= max - (*st)->line_height / 2);
}

/* Scroll to the bottom of the document. */
void ScrollToEnd (WindowPtr w)
{
  WStatusH st = WinGetStatus (w);
  
  Assert (st != NULL);
  LCSetValue ((*st)->scrollbars[V], LCGetMax ((*st)->scrollbars[V]));
  AdjustView (st);
}
