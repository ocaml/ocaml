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

static void DoAppleChoice (short item, EventModifiers mods)
{
  if (item == kItemAbout){
    OpenAboutBox ();
  }else{
    Str255 deskAccName;
    GetMenuItemText (GetMenuHandle (kMenuApple), item, deskAccName);
    OpenDeskAcc (deskAccName);
  }
}

OSErr DoQuit ()
{
  WindowPtr w;
  OSErr err;

  while (1){
    w = FrontWindow ();
    while (1){
      if (w == NULL) goto done;
      if (GetWindowGoAwayFlag (w) && w != winGraphics) break;
      w = GetNextWindow (w);
    }
    err = WinDoClose (closingApp, w);
    if (err != noErr) return err;
  }
  done:
  if (winGraphics != NULL) WinCloseGraphics ();
  WinCloseToplevel ();
  quit_requested = 1;
  return noErr;
}

static void DoFileChoice (short item, EventModifiers mods)
{
  WindowPtr w = FrontWindow ();

  switch (item){
  case kItemNew:
    FileNew ();
    break;
  case kItemOpen:
    FileDoGetOpen ();
    break;
  case kItemClose:
    WinDoClose (closingWindow, w);
    break;
  case kItemSave:
    FileDoSave (w, 0);
    break;
  case kItemSaveAs:
    FileDoSave (w, 1);
    break;
  case kItemRevert:
    FileRevert (w);
    break;
  case kItemPageSetup:
    XXX ();
    break;
  case kItemPrint:
    XXX ();
    break;
  case kItemQuit:
    DoQuit ();
    break;
  default: Assert (0);
  }
}

static void DoEditChoice (short item, EventModifiers mods)
{
  WindowPtr w = FrontWindow ();
  WEReference we = WinGetWE (w);

  switch (item){
  case kItemUndo:
    WEUndo (we);
    break;
  case kItemCut:
    WECut (we);
    ClipChanged ();
    break;
  case kItemCopy:
    WECopy (we);
    ClipChanged ();
    break;
  case kItemPaste:
    if (w == winToplevel){
      long selstart, selend;
      WEGetSelection (&selstart, &selend, we);
      if (selstart < wintopfrontier){
        selstart = selend = WEGetTextLength (we);
        WESetSelection (selstart, selend, we);
        WEFeatureFlag (weFReadOnly, weBitClear, we);
      }
      if (selstart == wintopfrontier && selend == selstart){
        WESetStyle (weDoFont + weDoSize + weDoColor + weDoFace+weDoReplaceFace,
                    &prefs.unread, we);
      }
      WEFeatureFlag (weFMonoStyled, weBitSet, we);
      WEPaste (we);
      WEFeatureFlag (weFMonoStyled, weBitClear, we);
    }else{
      WEPaste (we);
    }
    break;
  case kItemClear:
    WEDelete (we);
    break;
  case kItemSelectAll:
    WESetSelection (0, LONG_MAX, we);
    break;
  case kItemShowClipboard:
    ClipShow ();
    break;
  case kItemFind:
    XXX ();
    break;
  case kItemReplace:
    XXX ();
    break;
  case kItemPreferences:
    XXX ();
    break;
  default: Assert (0);
  }
}

static WindowPtr **winTable;  /* a handle */
static long winTableLen = 0;  /* number of entries in the table */

static void DoWindowsChoice (short item, EventModifiers mods)
{
  switch (item){
  case 1:
    Assert (winToplevel != NULL);
    SelectWindow (winToplevel);
    break;
  case 2:
    Assert (winGraphics != NULL);
    ShowWindow (winGraphics);
    SelectWindow (winGraphics);
    break;
  case 3:
    Assert (0);
  default:
    Assert (item - 4 >= 0 && item - 4 < winTableLen);
    SelectWindow ((*winTable)[item - 4]);
    break;
  }
}

void DoMenuChoice (long choice, EventModifiers mods)
{
  short menu = HiWord (choice);
  short item = LoWord (choice);

  switch (menu){
  case 0: break;
  case kMenuApple:
    DoAppleChoice (item, mods);
    HiliteMenu (0);
    break;
  case kMenuFile:
    DoFileChoice (item, mods);
    HiliteMenu (0);
    break;
  case kMenuEdit:
    DoEditChoice (item, mods);
    HiliteMenu (0);
    break;
  case kMenuWindows:
    DoWindowsChoice (item, mods);
    HiliteMenu (0);
    break;
  default: Assert (0);
  }
}

OSErr InitialiseMenus (void)
{
  OSErr err;
  Size s = 10;

  err = AllocHandle (s * sizeof (WindowPtr), (Handle *) &winTable);
  if (err != noErr) return err;

  SetMenuBar (GetNewMBar (kMenuBar));
  AppendResMenu (GetMenuHandle (kMenuApple), 'DRVR');
  DrawMenuBar ();
  return 0;
}

static void EnableDisableItem (MenuHandle menu, short item, int enable)
{
  if (enable){
    EnableItem (menu, item);
  }else{
    DisableItem (menu, item);
  }
}

/* Add w to the windows menu. */
OSErr MenuWinAdd (WindowPtr w)
{
  MenuHandle m;
  Str255 title;
  Size s = GetHandleSize ((Handle) winTable) / sizeof (WindowPtr);

  if (s <= winTableLen){
    OSErr err;
    SetHandleSize ((Handle) winTable, (s + 10) * sizeof (WindowPtr));
    err = MemError (); if (err != noErr) return err;
  }
  (*winTable)[winTableLen] = w;
  ++ winTableLen;

  m = GetMenuHandle (kMenuWindows);
  AppendMenu (m, "\px");
  GetWTitle (w, title);
  SetMenuItemText (m, (winTableLen-1) + 4, title);

  return noErr;
}

/* Remove w from the windows menu; do nothing if w is not there. */
void MenuWinRemove (WindowPtr w)
{
  long i;
  MenuHandle m;

  i = 0;
  while (1){
    if (i >= winTableLen) return;
    if ((*winTable)[i] == w) break;
    ++ i;
  }
  Assert (i < winTableLen);
  m = GetMenuHandle (kMenuWindows);
  DeleteMenuItem (m, kItemDocuments + i);
  for (++i; i < winTableLen; i++) (*winTable)[i-1] = (*winTable)[i];
  -- winTableLen;
}

static void MenuWinUpdate (void)
{
  long i;
  MenuHandle m = GetMenuHandle (kMenuWindows);
  WindowPtr w = FrontWindow ();

  SetItemMark (m, kItemToplevel, w == winToplevel ? diamondMark : noMark);
  SetItemMark (m, kItemGraphics, w == winGraphics ? diamondMark : noMark);
  for (i = 0; i < winTableLen; i++){
    SetItemMark (m, kItemDocuments + i,
                 w == (*winTable)[i] ? diamondMark : noMark);
  }
}

void UpdateMenus (void)
{
  WindowPtr w;
  WStatusH st;
  WEHandle we;
  MenuHandle m;
  Str255 text;
  struct menuflags flags = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  w = FrontWindow ();
  st = WinGetStatus (w);
  we = WinGetWE (w);

  WinUpdateStatus (w);
  
  if (st != NULL) flags = (*st)->menuflags;

  m = GetMenuHandle (kMenuFile);
  /* New is always enabled. */
  /* Open is always enabled. */
  EnableDisableItem (m, kItemClose, w != NULL && GetWindowGoAwayFlag (w));
  EnableDisableItem (m, kItemSave, flags.save);
  EnableDisableItem (m, kItemSaveAs, flags.save_as);
  EnableDisableItem (m, kItemRevert, flags.revert);
  EnableDisableItem (m, kItemPageSetup, flags.page_setup);
  EnableDisableItem (m, kItemPrint, flags.print);
  /* Quit is always enabled. */

  m = GetMenuHandle (kMenuEdit);
  DisableItem (m, kItemUndo);
  GetIndString (text, kUndoStrings, 1);
  SetMenuItemText (m, kItemUndo, text);
  if (we != NULL){
    Boolean temp;
    WEActionKind ak;

    Assert (st != NULL);

    ak = WEGetUndoInfo (&temp, we);
    if (ak != weAKNone){
      GetIndString (text, kUndoStrings, 2*ak + temp);
      SetMenuItemText (m, kItemUndo, text);
      EnableItem (m, kItemUndo);
    }
  }
  EnableDisableItem (m, kItemCut, flags.cut);
  EnableDisableItem (m, kItemCopy, flags.copy);
  EnableDisableItem (m, kItemPaste, flags.paste);
  EnableDisableItem (m, kItemClear, flags.clear);
  EnableDisableItem (m, kItemSelectAll, flags.select_all);
  /* Show Clipboard is always enabled. */
  EnableDisableItem (m, kItemFind, flags.find);
  EnableDisableItem (m, kItemReplace, flags.replace);
  /* Preferences… is always enabled. */

  MenuWinUpdate ();
  m = GetMenuHandle (kMenuWindows);
  EnableDisableItem (m, kItemGraphics, winGraphics != NULL);
}
