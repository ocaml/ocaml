/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "main.h"

UInt32 evtSleep = 0;
static RgnHandle mouseRegion = NULL;
static RgnHandle pointRegion = NULL;

static void AdjustCursor (Point mouse, RgnHandle mouseRegion)
{
  WindowPtr w = FrontWindow ();
  WEHandle we = WinGetWE (w);
  int k = WinGetKind (w);

  if (caml_at_work && w == winToplevel) return;
  SetRectRgn (mouseRegion, -SHRT_MAX, -SHRT_MAX, SHRT_MAX, SHRT_MAX);
  if (we != NULL && k != kWinAbout){
    if (WEAdjustCursor (mouse, mouseRegion, we)) return;
  }
  SetCursor (&qd.arrow);
}

static void DoActivate (EventRecord *evt)
{
  WindowPtr w = (WindowPtr) evt->message;

  if (GetWindowKind (w) != userKind) return;  /*XXX*/
  WinActivateDeactivate (evt->modifiers & activeFlag, w);
}

static void DoDiskEvent (EventRecord *evt)
{
  OSErr err;
  Point pt;
  
  if (evt->message >> 16 != noErr){
    DILoad ();
    err = DIBadMount (pt, evt->message);         /* [pt] is ignored */
    if (err != noErr) ErrorAlertGeneric (err);  /* XXX or nothing ? */
    DIUnload ();
  }
}

static void DoKeyDown (EventRecord *evt)
{
  short chr = evt->message & charCodeMask;
  Boolean isCmdKey = (evt->modifiers & cmdKey) != 0;

  if (chr == 0x10){
    switch ((evt->message & keyCodeMask) >> 8){
    case keyF1:
      isCmdKey = 1;
      chr = 'z';
      break;
    case keyF2:
      isCmdKey = 1;
      chr = 'x';
      break;
    case keyF3:
      isCmdKey = 1;
      chr = 'c';
      break;
    case keyF4:
      isCmdKey = 1;
      chr = 'v';
      break;
    default:
      chr = -1;
    }
  }
  if (isCmdKey && chr == '.' && FrontWindow () == winToplevel){
    if (evt->what != autoKey){
      /* If the signal handler calls longjmp, then it will jump into the
         bytecode interpreter and Caml will be working.  If it does not,
         then Caml must have been working already because we are not in
         a blocking section.  In all cases, raising a signal puts Caml
         to work.
      */
      Caml_working (1);
      raise (SIGINT);
    }
  }else if (isCmdKey && chr >= 0x20){
    UpdateMenus ();
    DoMenuChoice (MenuKey (chr), evt->modifiers);
  }else{
    WindowPtr w = FrontWindow ();
    if (chr != -1 && w != NULL){
      WinDoKey (w, chr, evt);
    }
  }
}

static void DoMouseDown (EventRecord *event)
{
  WindowPtr w;
  short partCode;

  partCode = FindWindow (event->where, &w);
  switch (partCode){
  case inMenuBar:
    UpdateMenus ();
    DoMenuChoice (MenuSelect (event->where), event->modifiers);
    break;
  case inSysWindow:
    SystemClick (event, w);
    break;
  case inContent:
    WinDoContentClick (event, w);
    break;
  case inDrag:
    WinDoDrag (event->where, w);
    break;
  case inGrow:
    WinDoGrow (event->where, w);
    break;
  case inGoAway:
    if (TrackGoAway (w, event->where)) WinDoClose (closingWindow, w);
    break;
  case inZoomIn:
  case inZoomOut:
    if (TrackBox (w, event->where, partCode)) WinDoZoom (w, partCode);
    break;
  }
}

/* XXX recuperer les mouse-up pour matcher les mouse-down ? */
static void DoMouseUp (EventRecord *e)
{
  short partCode;
  WindowPtr w;
  Point hitpt;
  GrafPtr saveport;
  Rect r;

  if (FrontWindow () != winGraphics) return;
  partCode = FindWindow (e->where, &w);
  if (partCode != inContent) return;
  GetPort (&saveport);
  SetPort (winGraphics);
  hitpt = e->where;
  GlobalToLocal (&hitpt);
  ScrollCalcGraph (winGraphics, &r);
  if (PtInRect (hitpt, &r)) GraphGotEvent (e);
  SetPort (saveport);
  return;
}

static void DoNullEvent (EventRecord *event)
{
  WindowPtr w = FrontWindow ();

  if (w != NULL) WinDoIdle (w);
}

static void DoOSEvent (EventRecord *event)
{
  int msg = (event->message & osEvtMessageMask) >> 24;
  WindowPtr w;

  switch (msg){
    case suspendResumeMessage:
      w = FrontWindow ();
      if (w != NULL){
        Boolean state = !! (event->message & resumeFlag);
        WinActivateDeactivate (state, w);
      }
      if (event->message & convertClipboardFlag) ClipChanged ();
    case mouseMovedMessage: ;
  }
}

static void DoUpdate (EventRecord *evt)
{
  WindowPtr w = (WindowPtr) evt->message;

  if (GetWindowKind (w) != userKind) return;  /*XXX*/
  WinUpdate (w);
}

static void DoDialogEvent (EventRecord *evt)
{
  DialogPtr dlg;
  short itm;

  if (evt->what == diskEvt){
    DoDiskEvent (evt);
    return;
  }else if (evt->what == keyDown || evt->what == autoKey){
    if (evt->modifiers & cmdKey){
      DoKeyDown (evt);
      return;
    }else{
      switch ((evt->message & charCodeMask) >> 8){
      case '\n':
        XXX (); /*XXX return key*/
        return;
      case '\033':
        XXX (); /*XXX escape key */
        return;
      default: break;
      }
    }
  }
  if (DialogSelect (evt, &dlg, &itm)){
    switch (WinGetKind (dlg)){
    case kWinAbout:
      Assert (0); /* No item is enabled. */
      break;
    case kWinPrefs:
      XXX ();
      break;
    default:
      Assert (0); /* Other windows are not dialogs. */
      break;
    }
  }
}

static pascal Boolean ProcessEvent (EventRecord *evt, long *sleep,
                                    RgnHandle *rgn)
{
  if (evt->what <= osEvt) AdjustCursor (evt->where, mouseRegion);
  if (IsDialogEvent (evt)){
    DoDialogEvent (evt);
  }else{
    switch (evt->what){
    case nullEvent:
      DoNullEvent (evt);
      break;
    case mouseDown:
      DoMouseDown (evt);
      break;
    case mouseUp:               /* Needed for the graphics window. */
      DoMouseUp (evt);
      break;
    case keyDown:
    case autoKey:
      DoKeyDown (evt);
      break;
    case updateEvt:
      DoUpdate (evt);
      break;
    case activateEvt:
      DoActivate (evt);
      break;
    case diskEvt:
      DoDiskEvent (evt);
      break;
    case osEvt:
      DoOSEvent (evt);
      break;
    case kHighLevelEvent:
      AEProcessAppleEvent (evt);
      break;
    }
  }
  *sleep = evt->what == nullEvent ? evtSleep : 0;
  *rgn = mouseRegion;
  return false;
}

void GetAndProcessEvents (WaitEventOption wait, short oldx, short oldy)
{
  EventRecord evt;
  long dummysleep;
  RgnHandle dummyregion;
  UInt32 cursleep = wait != noWait ? evtSleep : 0;
  RgnHandle currgn;
  
  if (wait == waitMove){
    currgn = pointRegion;
    SetRectRgn (pointRegion, oldx, oldy, oldx+1, oldy+1);
  }else{
    currgn = mouseRegion;
  }

  WaitNextEvent (everyEvent, &evt, cursleep, currgn);
  ProcessEvent (&evt, &dummysleep, &dummyregion);
  if (quit_requested) ExitApplication ();

  while (evt.what != nullEvent){
    WaitNextEvent (everyEvent, &evt, 0, NULL);
    ProcessEvent (&evt, &dummysleep, &dummyregion);
    if (quit_requested) ExitApplication ();
  }
}

AEIdleUPP ProcessEventUPP;

OSErr InitialiseEvents (void)
{
  OSErr err;

  mouseRegion = NewRgn ();     /* XXX out of memory ? */
  pointRegion = NewRgn ();     /* XXX out of memory ? */
  ProcessEventUPP = NewAEIdleProc (ProcessEvent);
  err = InstallAEHandlers ();
  return err;
}
