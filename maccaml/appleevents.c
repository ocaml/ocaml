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

static OSErr GotRequiredParams (const AppleEvent *ae)
{
  OSErr err;
  DescType type;
  Size sz;

  err = AEGetAttributePtr (ae, keyMissedKeywordAttr, typeWildCard, &type, NULL,
                           0, &sz);
  if (err == errAEDescNotFound) return noErr;
  if (err == noErr) return errAEParamMissed;
  return err;
}

static pascal OSErr HandleOpenApplication (const AppleEvent *ae,
                                           AppleEvent *reply, long refCon)
{
#pragma unused (refCon)
  launch_toplevel_requested = 1;
  return noErr;
}

static pascal OSErr HandleQuitApplication (const AppleEvent *ae,
                                           AppleEvent *reply, long refCon)
{
#pragma unused (refCon)
  WindowPtr w = FrontWindow ();
  WStatusH st;
  int request_interaction = prefs.asksavetop && winToplevel != NULL;
  OSErr err;

  while (w != NULL){
    WinUpdateStatus (w);
    st = WinGetStatus (w);
    if (st != NULL && (*st)->dirty){
      request_interaction = 1;
    }
    w = GetNextWindow (w);
  }
  if (request_interaction){
    err = AEInteractWithUser (kAEDefaultTimeout, NULL, ProcessEventUPP);
    if (err != noErr) return err;
  }
  err = DoQuit ();
  if (err != noErr) return err;

  return noErr;
}

static pascal OSErr HandleOpenDocuments (const AppleEvent *ae,
                                         AppleEvent *reply, long refCon)
{
#pragma unused (refCon)
  FSSpec filespec;
  AEDescList doclist = {0, NULL};
  OSErr err;
  long i, len;
  Size sz;
  AEKeyword key;
  DescType type;

  launch_toplevel_requested = 1;

  err = AEGetParamDesc (ae, keyDirectObject, typeAEList, &doclist);
  if (err != noErr) goto failed;

  err = GotRequiredParams (ae);
  if (err != noErr) goto failed;

  err = AECountItems (&doclist, &len);
  if (err != noErr) goto failed;

  for (i = 1; i <= len; i++){
    err = AEGetNthPtr (&doclist, i, typeFSS, &key, &type, &filespec,
                       sizeof (filespec), &sz);
    if (err != noErr) goto failed;
    err = FileOpen (&filespec);
    if (err != noErr){
      OSErr err2 = AEInteractWithUser (kAEDefaultTimeout, NULL,ProcessEventUPP);
      if (err2 == noErr){
        ErrorAlertCantOpen (filespec.name, err);
      }else{
        if (err2 == errAENoUserInteraction) err = err2;
        goto failed;
      }
    }
  }
  AEDisposeDesc (&doclist);
  return noErr;

  failed:
    if (doclist.dataHandle != NULL) AEDisposeDesc (&doclist);
    return err;
}

static pascal OSErr HandlePrintDocuments (const AppleEvent *ae,
                                          AppleEvent *reply, long refCon)
{
#pragma unused (refCon)
  return errAEEventNotHandled; /* XXX */
}

OSErr InstallAEHandlers (void)
{
  OSErr err;

  err = AEInstallEventHandler (kCoreEventClass, kAEOpenApplication,
                               NewAEEventHandlerProc (HandleOpenApplication),
                               0, false);
  if (err != noErr) goto failed;

  err = AEInstallEventHandler (kCoreEventClass, kAEQuitApplication,
                               NewAEEventHandlerProc (HandleQuitApplication),
                               0, false);
  if (err != noErr) goto failed;

  err = AEInstallEventHandler (kCoreEventClass, kAEOpenDocuments,
                               NewAEEventHandlerProc (HandleOpenDocuments),
                               0, false);
  if (err != noErr) goto failed;

  err = AEInstallEventHandler (kCoreEventClass, kAEPrintDocuments,
                               NewAEEventHandlerProc (HandlePrintDocuments),
                               0, false);
  if (err != noErr) goto failed;

  return noErr;

  failed:
    return err;
}
