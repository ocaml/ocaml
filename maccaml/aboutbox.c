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

static WindowPtr aboutbox = NULL;
static UserItemUPP DrawAboutUPP = NULL;

#define kItemText 2

static pascal void DrawAbout (DialogPtr d, short item)
{
  WEHandle we = WinGetWE (d);

  Assert (we != NULL);
  WEUpdate (d->visRgn, we);
}

void OpenAboutBox (void)
{
  OSErr err;
  short itemtype;
  Handle item;
  Rect itemrect;
  LongRect lr;
  WEHandle we = NULL;
  WStatusH st = NULL;
  Handle txt = NULL;
  TextStyle ts;

  if (DrawAboutUPP == NULL) DrawAboutUPP = NewUserItemProc (DrawAbout);

  if (aboutbox != NULL){
    SelectWindow (aboutbox);
  }else{
    aboutbox = GetNewDialog (kDialogAbout, NULL, (WindowPtr) -1L);
    if (aboutbox == NULL){
      err = memFullErr;
      goto failed;
    }
    SetPort (aboutbox);

    err = WinAllocStatus (aboutbox);
    if (err != noErr) goto failed;

    st = WinGetStatus (aboutbox);
    Assert (st != NULL);
    (*st)->kind = kWinAbout;

    GetDialogItem (aboutbox, kItemText, &itemtype, &item, &itemrect);
    SetDialogItem (aboutbox, kItemText, itemtype, (Handle) DrawAboutUPP, &itemrect);
    WERectToLongRect (&itemrect, &lr);
    err = WENew (&lr, &lr, 0, &we);
    if (err != noErr) goto failed;

    (*st)->we = we;

    GetFNum ("\pGeneva", &ts.tsFont);
    ts.tsSize = 10;
    err = WESetStyle (weDoFont + weDoSize, &ts, we);
    if (err != noErr) goto failed;

    txt = GetResource ('TEXT', kAboutText);
    err = ResError (); if (err != noErr){ err = noErr; goto failed; }
    DetachResource (txt);

    err = WEUseText (txt, we);
    if (err != noErr) goto failed;
    err = WECalText (we);
    if (err != noErr) goto failed;

    WEFeatureFlag (weFReadOnly, weBitSet, we);

    return;

    failed:
    if (txt != NULL) DisposeHandle (txt);
    if (we != NULL) WEDispose (we);
    if (st != NULL) DisposeHandle ((Handle) st);
    if (aboutbox != NULL) DisposeWindow (aboutbox);
    aboutbox = NULL;
    ErrorAlertGeneric (err);
  }
}

void CloseAboutBox (WindowPtr w)
{
  WStatusH st = WinGetStatus (w);
  WEHandle we = WinGetWE (w);

  Assert (w == aboutbox);

  Assert (we != NULL);
  WEDispose (we);
  Assert (st != NULL);
  DisposeHandle ((Handle) st);
  Assert (w != NULL);
  DisposeDialog (w);
  aboutbox = NULL;
}
