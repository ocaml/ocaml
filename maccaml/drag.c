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

static DragTrackingHandlerUPP MyTrackingHandlerUPP = NULL;
static DragReceiveHandlerUPP MyReceiveHandlerUPP = NULL;

static OSErr ToplevelTrackDrag (DragTrackingMessage message, DragReference drag)
{
  static int canacceptdrag = 0;
  static int hilited = 0;
  WEReference we = WinGetWE (winToplevel);
  short readonly;
  Point mouse;
  RgnHandle rgn = NewRgn ();
  Rect viewrect;
  LongRect lviewrect;
  OSErr err;
  DragAttributes attributes;

  Assert (we != NULL);
  switch (message){

  case kDragTrackingEnterWindow:
    readonly = WEFeatureFlag (weFReadOnly, weBitTest, we);
    WEFeatureFlag (weFReadOnly, weBitClear, we);
    canacceptdrag = WECanAcceptDrag (drag, we);
    if (readonly) WEFeatureFlag (weFReadOnly, weBitSet, we);
    break;

  case kDragTrackingInWindow:
    if (canacceptdrag){
      err = GetDragAttributes (drag, &attributes);
      if (err != noErr) goto failed;
      err = GetDragMouse (drag, &mouse, nil);
      if (err != noErr) goto failed;
      GlobalToLocal (&mouse);
      WEGetViewRect (&lviewrect, we);
      WELongRectToRect (&lviewrect, &viewrect);
      InsetRect (&viewrect, -kTextMarginH, 0);
      if (PtInRect (mouse, &viewrect)){
        if (!hilited && (attributes & kDragHasLeftSenderWindow)){
          RectRgn (rgn, &viewrect);
          InsetRgn (rgn, 0, -kTextMarginV);
          ShowDragHilite (drag, rgn, true);
          DisposeRgn (rgn);
          hilited = 1;
        }
      }else{
        if (hilited){
          HideDragHilite (drag);
          hilited = 0;
        }
      }
    }
    break;

  case kDragTrackingLeaveWindow:
    if (hilited){
      HideDragHilite (drag);
      hilited = 0;
    }
    break;

  default: break;
  }
  return noErr;

  failed: return err;
}

static pascal OSErr MyTrackingHandler (DragTrackingMessage message, WindowPtr w,
                                       void *refCon, DragReference drag)
{
  #pragma unused (refCon)
  WEReference we;

  switch (WinGetKind (w)){
  case kWinUnknown:
  case kWinUninitialised:
  case kWinAbout:
  case kWinGraphics:
  case kWinPrefs:
  case kWinClipboard:
    return noErr;

  case kWinToplevel:
    return ToplevelTrackDrag (message, drag);

  case kWinDocument:
    we = WinGetWE (w);   Assert (we != NULL);
    return WETrackDrag (message, drag, we);

  default:
    Assert (0);
    return noErr;
  }
}

static OSErr ToplevelReceiveDrag (DragReference drag, WEReference we)
{
  GrafPtr (saveport);
  short readonly = 0;
  Boolean canaccept;
  OSErr err;
  Point mouse;
  LongRect lviewrect;
  Rect viewrect;
  UInt16 nitems;
  UInt16 i;
  ItemReference itemref;
  Handle h = NULL;
  Size sz, curlen;
  long dest, selstart, selend = -1;

  GetPort (&saveport);
  SetPortWindowPort (winToplevel);

  readonly = WEFeatureFlag (weFReadOnly, weBitTest, we);
  if (readonly) WEFeatureFlag (weFReadOnly, weBitClear, we);
  canaccept = WECanAcceptDrag (drag, we);
  if (!canaccept){ err = badDragFlavorErr; goto failed; }

  err = GetDragMouse (drag, &mouse, nil);
  if (err != noErr) goto failed;
  GlobalToLocal (&mouse);
  WEGetViewRect (&lviewrect, we);
  WELongRectToRect (&lviewrect, &viewrect);
  if (!PtInRect (mouse, &viewrect)){ err = dragNotAcceptedErr; goto failed; }

  /* XXX Ne pas coller si le drag vient de la même fenêtre et la souris
         est revenue dans la sélection. */

  h = NewHandle (0);
  err = MemError (); if (err != noErr) goto failed;
  curlen = 0;

  err = CountDragItems (drag, &nitems);
  if (err != noErr) goto failed;

  for (i = 1; i <= nitems; i++){
    err = GetDragItemReferenceNumber (drag, i, &itemref);
    if (err != noErr) goto failed;
    err = GetFlavorDataSize (drag, itemref, kTypeText, &sz);
    if (err != noErr) goto failed;
    SetHandleSize (h, curlen + sz);
    err = MemError (); if (err != noErr) goto failed;
    HLock (h);
    err = GetFlavorData (drag, itemref, kTypeText, (*h)+curlen, &sz, 0);
    HUnlock (h);
    if (err != noErr) goto failed;
    curlen += sz;
  }
  dest = WEGetTextLength (we);
  WEGetSelection (&selstart, &selend, we);
  WESetSelection (dest, dest, we);
  WESetStyle (weDoFont + weDoFace + weDoSize + weDoColor + weDoReplaceFace,
              &prefs.unread, we);
  HLock (h);
  err = WEInsert (*h, curlen, NULL, NULL, we);
  HUnlock (h);
  if (err != noErr) goto failed;
  WESetSelection (dest + curlen, dest + curlen, we);

  DisposeHandle (h);
  SetPort (saveport);
  return noErr;

  failed:
    if (h != NULL) DisposeHandle (h);
    if (selend != -1) WESetSelection (selstart, selend, we);
    if (readonly) WEFeatureFlag (weFReadOnly, weBitSet, we);
    SetPort (saveport);
    return err;
}

static pascal OSErr MyReceiveHandler (WindowPtr w, void *refCon,
                                      DragReference drag)
{
  #pragma unused (refCon)
  WEReference we;

  switch (WinGetKind (w)){
  case kWinUnknown:
  case kWinUninitialised:
  case kWinAbout:
  case kWinGraphics:
  case kWinPrefs:
  case kWinClipboard:
    return noErr;
  case kWinToplevel:
    we = WinGetWE (w);   Assert (we != NULL);
    return ToplevelReceiveDrag (drag, we);
  case kWinDocument:
    we = WinGetWE (w);   Assert (we != NULL);
    return WEReceiveDrag (drag, we);
  default:
    Assert (0);
    return noErr;
  }
}

OSErr InstallDragHandlers (void)
{
  OSErr err;

  MyTrackingHandlerUPP = NewDragTrackingHandlerProc (MyTrackingHandler);
  MyReceiveHandlerUPP = NewDragReceiveHandlerProc (MyReceiveHandler);

  err = InstallTrackingHandler (MyTrackingHandlerUPP, NULL, NULL);
  if (err != noErr) return err;
  err = InstallReceiveHandler (MyReceiveHandlerUPP, NULL, NULL);
  if (err != noErr){
    RemoveTrackingHandler (MyTrackingHandlerUPP, NULL);
    return err;
  }
  return noErr;
}

OSErr RemoveDragHandlers (void)
{
  OSErr err1, err2;

  err1 = RemoveTrackingHandler (MyTrackingHandlerUPP, NULL);
  err2 = RemoveReceiveHandler (MyReceiveHandlerUPP, NULL);
  if (err2 != noErr && err1 == noErr) return err2;
  return err1;
}
