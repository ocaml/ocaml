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

static unsigned long nuntitled = 0;
static unsigned long count = 2;

/* XXX prŽvoir le cas o on peut Žcrire le texte mais pas les ressources
   -> resrefnum peut tre -1 quand datarefnum est valide
*/

static void MakeUntitledTitle (Str255 result)
{
  char buffer [15];

  GetIndString (result, kMiscStrings, kUntitledIdx);
  if (nuntitled !=  0){
    if (result [0] > 240) result [0] = 240;
    sprintf (buffer, " %lu", count);   Assert (strlen (buffer) < 15);
    strcpy ((char *) result + result [0] + 1, buffer);
    result [0] += strlen (buffer);
    ++ count;
  }else{
    count = 2;
  }
  ++ nuntitled;
}

static void FreeUntitledTitle ()
{
  -- nuntitled;
}

/* Close the file associated with the window, saving it if needed. */
OSErr FileDoClose (WindowPtr w, ClosingOption close)
{
  WStatusH st;
  WEHandle we;
  Str255 savingprompt, filename;
  short item;
  OSErr err;

  Assert (WinGetKind (w) == kWinDocument);
  WinUpdateStatus (w);
  st = WinGetStatus (w);  Assert (st != NULL);
  we = WinGetWE (w);      Assert (we != NULL);
  GetWTitle (w, filename);
  if ((*st)->menuflags.save){
    GetIndString (savingprompt, kMiscStrings, kClosingIdx + close);
    ParamText (filename, savingprompt, NULL, NULL);
    InitCursor ();
    modalkeys = kKeysSaveDontCancel;
    item = Alert (kAlertSaveAsk, myModalFilterUPP);
    switch (item){
    case 1: /* Yes */
      err = FileDoSave (w, 0);
      if (err != noErr) return err;
      break;
    case 2: /* Cancel */
      return userCanceledErr;
    case 3: /* No */
      break;
    default: Assert (0);
    }
  }else{
    if ((*st)->resrefnum != -1){
      /* XXX sauver fenetre, selection, scrollbars */
    }
  }
  if ((*st)->datarefnum == -1){
    Assert ((*st)->resrefnum == -1);
    FreeUntitledTitle ();
  }else{
    FSClose ((*st)->datarefnum);
    if ((*st)->resrefnum != -1) CloseResFile ((*st)->resrefnum);
  }
  return noErr;
}

/* Open a new untitled window. */
void FileNew (void)
{
  Str255 titlebuf;
  WindowPtr w;
  OSErr err;
  WStatusH st;

  MakeUntitledTitle (titlebuf);
  w = WinOpenDocument ((StringPtr) titlebuf);
  if (w == NULL) {err = 0/*XXX*/; goto failed; }
  st = WinGetStatus (w);  Assert (st != NULL);
  (*st)->datarefnum = (*st)->resrefnum = -1;
  return;

  failed:
    if (w != NULL) WinDoClose (closingWindow, w);
    ErrorAlertGeneric (err);
}

/* Open the specified file in a new window. */
OSErr FileOpen (FSSpec *filespec)
 {
  WindowPtr w = NULL;
  WStatusH st;
  StringPtr title;
  Str255 titlebuf;
  short resrefnum = -1, datarefnum = -1;
  Size textsize;
  Handle texthandle = NULL;
  OSErr err;
  int template;
  SignedByte perm;
  FInfo fileinfo;

  err = FSpGetFInfo (filespec, &fileinfo);
  if (err != noErr) goto failed;
  if (fileinfo.fdFlags & kIsStationery){
    MakeUntitledTitle (titlebuf);
    title = (StringPtr) titlebuf;
    template = 1;
  }else{
    title = (StringPtr) filespec->name;
    template = 0;
  }
  perm = template ? fsRdPerm : fsRdWrPerm;

  err = FSpOpenDF (filespec, perm, &datarefnum);
  if (err != noErr){ datarefnum = -1; goto failed; }
  err = GetEOF (datarefnum, &textsize);
  if (err != noErr) goto failed;
  err = SetFPos (datarefnum, fsFromStart, 0L);
  if (err != noErr) goto failed;
  err = AllocHandle (textsize, &texthandle);
  if (err != noErr) goto failed;
  HLock (texthandle);
  err = FSRead (datarefnum, &textsize, *texthandle);
  HUnlock (texthandle);
  if (err != noErr) goto failed;

  /*XXX FSpCreateResFile (filespec, creator, type, 0); */
  resrefnum = FSpOpenResFile (filespec, perm);
  if (resrefnum != -1){
    /* XXX lire la position de la fentre, la sŽlection, les scrollbars */
  }

  w = WinOpenDocument (title);
  if (w == NULL) { err = 0/*XXX*/; goto failed; }
  st = WinGetStatus (w);  Assert (st != NULL);

  WEUseText (texthandle, (*st)->we);
  WECalText ((*st)->we);
  WESetSelection (0, 0, (*st)->we);  /* XXX */
  AdjustScrollBars (w);
  WEResetModCount ((*st)->we);
  (*st)->basemodcount = 0;

  if (template){
    FSClose (datarefnum);
    if (resrefnum != -1) CloseResFile (resrefnum);
    (*st)->datarefnum = (*st)->resrefnum = -1;
  }else{
    (*st)->datarefnum = datarefnum;
    (*st)->resrefnum = resrefnum;
  }
  return noErr;

  failed:
    if (texthandle != NULL) DisposeHandle (texthandle);
    if (datarefnum != -1) FSClose (datarefnum);
    if (resrefnum != -1) CloseResFile (resrefnum);
    if (w != NULL) WinDoClose (closingWindow, w);
    return err;
}

/* Get a file with the standard dialog and open it in a new window. */
void FileDoGetOpen (void)
{
  OSErr err;
  StandardFileReply sfreply;
  SFTypeList types = { 'TEXT' };

  StandardGetFile (NULL, 1, types, &sfreply);
  if (sfreply.sfGood){
    err = FileOpen (&sfreply.sfFile);
    if (err != noErr) ErrorAlertCantOpen (sfreply.sfFile.name, err);
  }
}

/* Revert w to the contents of its associated file. */
void FileRevert (WindowPtr w)
{
  WStatusH st;
  short err;
  Size textsize;
  Handle texthandle;

  /*XXX demander confirmation */

  st = WinGetStatus (w);
  Assert (st != NULL);
  Assert ((*st)->datarefnum != -1);
  Assert ((*st)->we != NULL);

  err = GetEOF ((*st)->datarefnum, &textsize);
  if (err != noErr) goto failed;
  err = SetFPos ((*st)->datarefnum, fsFromStart, 0L);
  if (err != noErr) goto failed;
  err = AllocHandle (textsize, &texthandle);
  if (err != noErr) goto failed;
  HLock (texthandle);
  err = FSRead ((*st)->datarefnum, &textsize, *texthandle);
  HUnlock (texthandle);
  if (err != noErr) goto failed;

  /* XXX lire la sŽlection (pas la scrollbar ?) */

  SetPortWindowPort (w);
  WEUseText (texthandle, (*st)->we);
  WECalText ((*st)->we);
  WEUpdate (NULL, (*st)->we);
  WESetSelection (0, 0, (*st)->we);  /* XXX */
  AdjustScrollBars (w);
  WEResetModCount ((*st)->we);
  (*st)->basemodcount = 0;
  return;

  failed:
    if (texthandle != NULL) DisposeHandle (texthandle);
    ErrorAlertGeneric (err);
}

/* Save the text to datarefnum.
   If resrefnum != -1, save the window position and the current selection.
*/
static OSErr SaveText (WindowPtr w, short datarefnum, short resrefnum)
{
  WStatusH st = WinGetStatus (w);
  Handle text;
  Size textsize;
  OSErr err;

  Assert (st != NULL);
  Assert ((*st)->we != NULL);
  err = SetEOF (datarefnum, 0L);
  if (err != noErr) goto failed;
  text = WEGetText ((*st)->we);
  textsize = GetHandleSize (text);
  HLock (text);
  err = FSWrite (datarefnum, &textsize, *text);
  HUnlock (text);
  if (err != noErr) goto failed;
  (*st)->basemodcount = WEGetModCount ((*st)->we);

  if (resrefnum != -1){
    /* XXX Žcrire la sŽlection et la position des scrollbars
     attention: pas de fail. */
  }
  return noErr;

  failed:
    return err;
}

/* Ask the user for a new file name, open both forks, and return
   the refnums.
*/
static OSErr PrepSaveAs (WindowPtr w, short *datarefnum, short *resrefnum,
                         StandardFileReply *reply)
{
  Str255 prompt, title;
  OSErr err;
  short auxrefnum = -1;

  *datarefnum = *resrefnum = -1;

  GetIndString (prompt, kMiscStrings, kSaveAsPromptIdx);
  GetWTitle (w, title);
  StandardPutFile (prompt, title, reply);

  if (reply->sfGood){
    if (reply->sfReplacing){
      err = FSpOpenDF (&reply->sfFile, fsRdWrPerm, datarefnum);
      if (err != noErr) *datarefnum = -1;
      if (err == opWrErr || err == fLckdErr || err == afpObjectLocked
          || err == permErr || err == afpAccessDenied || err == wrPermErr){
        ErrorAlert (kCannotWriteIdx, reply->sfFile.name, kCloseQuoteIdx, err);
      }
      if (err != noErr) goto failed;

      err = FSpOpenRF (&reply->sfFile, fsRdWrPerm, &auxrefnum);
      if (err != noErr) auxrefnum = -1;
      if (err == opWrErr || err == fLckdErr || err == afpObjectLocked
          || err == permErr || err == afpAccessDenied){
        ErrorAlert (kCannotWriteIdx, reply->sfFile.name, kCloseQuoteIdx, err);
      }
      if (err != noErr) goto failed;

      err = SetEOF (auxrefnum, 0L);
      if (err != noErr) goto failed;
      FSClose (auxrefnum); auxrefnum = -1;
      FSpCreateResFile (&reply->sfFile, kCreatorCaml,kTypeText,reply->sfScript);
      err = ResError (); if (err != noErr) goto failed;
      *resrefnum = FSpOpenResFile (&reply->sfFile, fsRdWrPerm);
      if (*resrefnum == -1){ err = ResError (); goto failed; } /*XXX ?? */

      err = SetEOF (*datarefnum, 0L);
      if (err != noErr) goto failed;

    }else{
      err = FSpCreate (&reply->sfFile, kCreatorCaml, kTypeText,reply->sfScript);
      if (err != noErr) goto failed;
      FSpCreateResFile (&reply->sfFile, kCreatorCaml,kTypeText,reply->sfScript);
      err = ResError (); if (err != noErr) goto failed;
      err = FSpOpenDF (&reply->sfFile, fsRdWrPerm, datarefnum);
      if (err != noErr){ *datarefnum = -1; goto failed; }
      *resrefnum = FSpOpenResFile (&reply->sfFile, fsRdWrPerm);
      if (*resrefnum == -1){ err = ResError (); goto failed; } /*XXX ?? */
    }
  }else{
    err = userCanceledErr;
    goto failed;
  }
  return noErr;

  failed:
    if (*datarefnum != -1) FSClose (*datarefnum);
    if (*resrefnum != -1) CloseResFile (*resrefnum);
    if (auxrefnum != -1) FSClose (auxrefnum);
    return err;
}

/* If saveasflag is true or there is no associated file,
   then ask for a new file name with the standard dialog
   and associate it with w.

   Save the contents of w to its associated file.
*/
static OSErr SaveDocument (WindowPtr w, int saveasflag)
{
  WStatusH st = WinGetStatus (w);
  OSErr err;
  int changetitle = 0;
  short datarefnum = -1, resrefnum = -1;

  Assert (st != NULL);
  if (saveasflag || (*st)->datarefnum == -1){
    StandardFileReply reply;

    err = PrepSaveAs (w, &datarefnum, &resrefnum, &reply);
    if (err != noErr) goto failed;

    if ((*st)->datarefnum == -1){
      Assert ((*st)->resrefnum == -1);
      FreeUntitledTitle ();
    }else{
      Assert ((*st)->resrefnum != -1);
      FSClose ((*st)->datarefnum);
      if ((*st)->resrefnum != -1) CloseResFile ((*st)->resrefnum);
      (*st)->datarefnum = (*st)->resrefnum = -1;
    }
    (*st)->datarefnum = datarefnum;
    (*st)->resrefnum = resrefnum;
    SetWTitle (w, reply.sfFile.name);
    datarefnum = resrefnum = -1;
  }
  err = SaveText (w, (*st)->datarefnum, (*st)->resrefnum);
  if (err != noErr) goto failed;
  return noErr;

  failed:
    if (datarefnum != -1) FSClose (datarefnum);
    if (resrefnum != -1) CloseResFile (resrefnum);
    return err;
}

/* Save the toplevel window to a new file.  Do not save the window
   position or the current selection.
*/
static OSErr SaveToplevel (void)
{
  WStatusH st;
  StandardFileReply reply;
  short datarefnum = -1, resrefnum = -1;
  OSErr err;

  Assert (winToplevel != NULL);
  st = WinGetStatus (winToplevel);
  Assert (st != NULL);

  err = PrepSaveAs (winToplevel, &datarefnum, &resrefnum, &reply);
  if (err != noErr) goto failed;
  err = SaveText (winToplevel, datarefnum, -1);
  if (err != noErr) goto failed;
  FSClose (datarefnum);
  if (resrefnum != -1) CloseResFile (resrefnum);
  return noErr;

  failed:
    if (datarefnum != -1) FSClose (datarefnum);
    if (resrefnum != -1) CloseResFile (resrefnum);
    return err;
}

static OSErr SaveGraphics (void)
{
  XXX ();
  return noErr;
}

OSErr FileDoSave (WindowPtr w, int saveasflag)
{
  if (w == winToplevel) return SaveToplevel ();
  else if (w == winGraphics) return SaveGraphics ();
  else return SaveDocument (w, saveasflag);
}
