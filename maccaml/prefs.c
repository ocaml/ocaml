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

#define kPrefResource 1000

struct prefs prefs;
static struct prefs defpref;

static void InitPrefs (void)
{
  TextStyle defstyle;

  defpref.version = PREF_VERSION;
  defpref.asksavetop = 0;
  WinToplevelStdState (&defpref.toppos);
  WinClipboardStdState (&defpref.clippos);
  GetFNum ("\pmonaco", &defstyle.tsFont);
  defstyle.tsSize = 9;
  defstyle.tsFace = 0;
  defstyle.tsColor.red = 0;
  defstyle.tsColor.green = 0;
  defstyle.tsColor.blue = 0;
  defpref.text = defpref.unread = defpref.input = defpref.output
    = defpref.errors = defstyle;

  defpref.unread.tsColor.green = 42000;
  defpref.output.tsColor.blue = 65535;
  defpref.errors.tsColor.red = 65535;
  defpref.errors.tsFace = underline;
}

void ReadPrefs (void)
{
  short err;
  short vrefnum;
  long dirid;
  short refnum = -1;
  Handle prefsH = NULL;
  Str255 prefsfilename;
  FSSpec spec;

  InitPrefs ();
  GetIndString (prefsfilename, kMiscStrings, kPrefsFileNameIdx);
  err = FindFolder (kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
                    &vrefnum, &dirid);
  if (err != noErr) goto cantread;
  err = FSMakeFSSpec (vrefnum, dirid, prefsfilename, &spec);
  if (err != noErr) goto cantread;
  refnum = FSpOpenResFile (&spec, fsRdPerm);
  if (refnum == -1) goto cantread;
  prefsH = Get1Resource ('Oprf', kPrefResource);
  if (prefsH == NULL) goto cantread;
  if (GetHandleSize (prefsH) != sizeof (prefs)) goto cantread;
  if (**(long **)prefsH != PREF_VERSION) goto cantread;
  memcpy (&prefs, *prefsH, sizeof (prefs));
  CloseResFile (refnum);
  return;

  cantread:
  if (refnum != -1) CloseResFile (refnum);
  prefs = defpref;
}

void WritePrefs (void)
{
  short err;
  short vrefnum;
  long dirid;
  short refnum = -1;
  Handle prefsH = NULL;
  Str255 prefsfilename;
  FSSpec spec;
  Handle h;

  GetIndString (prefsfilename, kMiscStrings, kPrefsFileNameIdx);
  err = FindFolder (kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
                    &vrefnum, &dirid);
  if (err != noErr) goto cantwrite;
  err = FSMakeFSSpec (vrefnum, dirid, prefsfilename, &spec);
  if (err != noErr && err != fnfErr) goto cantwrite;

  if (err == fnfErr){
    if (!memcmp (&prefs, &defpref, sizeof (prefs))) return;
    else FSpCreateResFile (&spec, 0, 0, smSystemScript);
  }
  refnum = FSpOpenResFile (&spec, fsRdWrPerm);
  if (refnum == -1) goto cantwrite;

  prefsH = Get1Resource ('Oprf', kPrefResource);
  if (prefsH == NULL){
    err = AllocHandle (sizeof (prefs), (Handle *) &prefsH);
    if (err != noErr) goto cantwrite;
    AddResource (prefsH, 'Oprf', kPrefResource, "\pO'Caml prefs");
  }
  SetHandleSize (prefsH, sizeof (prefs));
  if (MemError () != noErr) goto cantwrite;
  memcpy (*prefsH, &prefs, sizeof (prefs));
  ChangedResource (prefsH);

  h = GetResource ('STR ', kPrefsDescriptionStr);
  if (h != NULL){
    DetachResource (h);
    AddResource (h, 'STR ', kApplicationMissing, NULL);
    ChangedResource (h);
  }

  CloseResFile (refnum);
  return;

  cantwrite:
  if (refnum != -1) CloseResFile (refnum);
}
