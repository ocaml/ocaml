/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <CursorCtl.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>

#include "alloc.h"
#include "mlvalues.h"
#include "rotatecursor.h"
#include "signals.h"
#include "ui.h"

#include "main.h"

/* These are defined by the ocamlrun library. */
void caml_main(char **argv);
Handle macos_getfullpathname (short vrefnum, long dirid);

/* This pointer contains the environment variables. */
char *envPtr = NULL;

/* True if the Caml program is reading from the console. */
static int caml_reading_console = 0;

/* [Caml_working] is used to manage the processor idle state on
   PowerBooks.  [Caml_working (1)] disables the idle state, and
   [Caml_working (0)] enables it.
*/
static int caml_at_work = 0;
static void Caml_working (int newstate)
{
  if (gHasPowerManager){
    if (caml_at_work && !newstate) EnableIdle ();
    if (!caml_at_work && newstate) DisableIdle ();
  }
  caml_at_work = newstate;
}

/*
  Animated cursor (only when toplevel window is frontmost).
*/
typedef struct {
  short nframes;
  short current;
  union {
    CursHandle h;
    struct { short id; short fill; } i;
  } frames [1];
} **AnimCursHandle;

static AnimCursHandle acurh = NULL;

pascal void InitCursorCtl (acurHandle newCursors)
{
#pragma unused (newCursors)
  long i;

  if (acurh != NULL) return;
  acurh = (AnimCursHandle) GetResource ('acur', 0);
  for (i = 0; i < (*acurh)->nframes; i++){
    (*acurh)->frames[i].h = GetCursor ((*acurh)->frames[i].i.id);
    if ((*acurh)->frames[i].h == NULL){
      (*acurh)->frames[i].h = GetCursor (watchCursor);
      Assert ((*acurh)->frames[i].h != NULL);
    }
  }
  (*acurh)->current = 0;
}

pascal void RotateCursor (long counter)
{
#pragma unused (counter)
  if (acurh == NULL) InitCursorCtl (NULL);
  /* (*acurh)->current += (*acurh)->nframes + (counter >= 0 ? 1 : -1); */
  (*acurh)->current += (*acurh)->nframes + (caml_at_work ? 1 : -1);
  (*acurh)->current %= (*acurh)->nframes;
}

int AdjustRotatingCursor (void)
{
  static Point oldmouse = {-1, -1};
  Point mouse;
  int res = 0;

  if (acurh == NULL) InitCursorCtl (NULL);

  GetMouse (&mouse);
  if (mouse.h != oldmouse.h || mouse.v != oldmouse.v){
    last_event_date = TickCount ();
  }
  if (caml_reading_console == 0 && TickCount () > last_event_date + 60){
    SetCursor (*((*acurh)->frames[(*acurh)->current].h));
    ShowCursor ();
    res = 1;
  }
  oldmouse = mouse;
  return res;
}

static pascal void interp_yield (long counter)
{
  RotateCursor (counter);
  GetAndProcessEvents (noWait, 0, 0);
  if (intr_requested){
    intr_requested = 0;
    raise (SIGINT);
  }
}

/* Expand the percent escapes in the string specified by s.
   The escapes are:
   %a application file name
   %d full pathname of the current working directory (ends in ':')
   %t full pathname of the temporary directory (ends in ':')
   %% a percent sign "%"
*/
static OSErr expand_escapes (Handle s)
{
  Size i, j, l;
  OSErr err;
  Handle curdir = NULL, tmpdir = NULL;
  char *ptr2;
  long len2;

  l = GetHandleSize (s) - 1;
  i = j = 0;
  while (i < l){
    if ((*s)[j] == '%'){
      switch ((*s)[j+1]){
      case 'a':
        ptr2 = (char *) LMGetCurApName () + 1;
        len2 = * (LMGetCurApName ());
        break;
      case 'd':
        if (curdir == NULL) curdir = macos_getfullpathname (0, 0);
        if (curdir == NULL){ err = fnfErr; goto failed; }
        HLock (curdir);
        ptr2 = *curdir;
        len2 = GetHandleSize (curdir);
        break;
      case 't':
        if (tmpdir == NULL){
          short vrefnum;
          long dirid;
          err = FindFolder (kOnSystemDisk, kTemporaryFolderType, true,
                            &vrefnum, &dirid);
          tmpdir = macos_getfullpathname (vrefnum, dirid);
          if (tmpdir == NULL){ err = fnfErr; goto failed; }
        }
        HLock (tmpdir);
        ptr2 = *tmpdir;
        len2 = GetHandleSize (tmpdir);
        break;
      case '%':
        ptr2 = "%";
        len2 = 1;
        break;
      default:
        ptr2 = "";
        len2 = 0;
        break;
      }
      Munger (s, j, NULL, 2, ptr2, len2);
      j += len2 - 2;
      i += 1;
    }
    ++ i;
    ++ j;
  }
  if (curdir != NULL) DisposeHandle (curdir);
  if (tmpdir != NULL) DisposeHandle (tmpdir);
  return noErr;

  failed:
    if (curdir != NULL) DisposeHandle (curdir);
    if (tmpdir != NULL) DisposeHandle (tmpdir);
    return err;
}

/* [build_command_line] creates the array of strings that represents
   the command line according to the template found in
   the 'Line'(kCommandLineTemplate) resource and the environment
   variables according to the 'Line'(kEnvironmentTemplate).

   Each of these resources is a sequence of strings terminated by null
   bytes.  In each string, percent escapes are expanded (see above for
   a description of percent escapes).

   Each resource ends with a null byte.
*/
static OSErr build_command_line (char ***p_argv)
{
  Handle template = NULL;
  Size len, i, j;
  char *args = NULL;
  int argc;
  char **argv = NULL;
  OSErr err;

  template = GetResource ('Line', kCommandLineTemplate);
  if (template == NULL){ err = ResError (); goto failed; }
  err = expand_escapes (template);   if (err != noErr) goto failed;
  len = GetHandleSize (template);

  args = malloc (len);
  if (args == NULL){ err = memFullErr; goto failed; }
  memmove (args, *template, len);

  argc = 0;
  for (i = 0; i < len; i++){
    if (args[i] == '\000') ++ argc;
  }
  argv = malloc ((argc+1) * sizeof (char *));
  if (argv == NULL){ err = memFullErr; goto failed; }

  i = j = 0;
  do{
    argv[j++] = args + i;
    while (args [i] != '\000') ++ i;
    ++ i;
  }while (i < len);
  argv [argc] = NULL;

  ReleaseResource (template);

  template = GetResource ('Line', kEnvironmentTemplate);
  if (template == NULL){ err = ResError (); goto failed; }
  err = expand_escapes (template);   if (err != noErr) goto failed;
  len = GetHandleSize (template);
  envPtr = NewPtr (len);
  if (envPtr == NULL){ err = MemError (); goto failed; }
  memmove (envPtr, *template, len);

  *p_argv = argv;
  return noErr;

  failed:
    if (template != NULL) ReleaseResource (template);
    if (args != NULL) free (args);
    if (argv != NULL) free (argv);
    return err;
}

/* [launch_caml_main] is called by [main].
   
   After building the command line, [launch_caml_main] launches [caml_main]
   in a thread, then executes the GUI event loop in the main thread.
*/

OSErr launch_caml_main (void)
{ 
  char **argv;
  OSErr err;

  rotatecursor_options (&something_to_do, 0, &interp_yield);
  err = WinOpenToplevel ();
  if (err != noErr) goto failed;

  err = build_command_line (&argv);
  if (err) goto failed;

  Caml_working (1);
  caml_main (argv);
  ui_exit (0);

  failed:
    return err;
}

/* console I/O functions */

/* Management of error highlighting. */
static int erroring = 0;
static long error_curpos;
static long error_anchor = -1;

void FlushUnreadInput (void)
{
  WEReference we;
  int active;

  we = WinGetWE (winToplevel);
  Assert (we != NULL);

  WEFeatureFlag (weFReadOnly, weBitClear, we);
  WESetSelection (wintopfrontier, wintopfrontier, we);
  WEFeatureFlag (weFOutlineHilite, weBitClear, we);
  active = WEIsActive (we);
  if (active) WEDeactivate (we);
  WESetSelection (wintopfrontier, WEGetTextLength (we), we);
  WEDelete (we);
  if (active) WEActivate (we);
  WEFeatureFlag (weFOutlineHilite, weBitSet, we);
}

int ui_read (int fd, char *buffer, unsigned int nCharsDesired)
{
  long len, i;
  char **htext;
  WEReference we;
  long selstart, selend;
  Boolean active;
  short readonly, autoscroll;
  int atend;

  if (fd != 0) return read (fd, buffer, nCharsDesired);

  we = WinGetWE (winToplevel);
  Assert (we != NULL);
  htext = (char **) WEGetText (we);

  ++ caml_reading_console;

  while (1){
    char *p;

    len = WEGetTextLength (we);
    p = *htext;
    for (i = wintopfrontier; i < len; i++){
      if (p[i] == '\n') goto gotit;
    }
    GetAndProcessEvents (waitEvent, 0, 0);
  }

  gotit:

  len = i+1 - wintopfrontier;
  if (len > nCharsDesired) len = nCharsDesired;
  memmove (buffer, (*htext)+wintopfrontier, len);

  atend = ScrollAtEnd (winToplevel);
  autoscroll = WEFeatureFlag (weFAutoScroll, weBitTest, we);
  WEFeatureFlag (weFAutoScroll, weBitClear, we);
  WEGetSelection (&selstart, &selend, we);
  readonly = WEFeatureFlag (weFReadOnly, weBitTest, we);
  WEFeatureFlag (weFReadOnly, weBitClear, we);
  /* Always set an empty selection before changing OutlineHilite or
     the active status. */
  WESetSelection (wintopfrontier, wintopfrontier, we);
  WEFeatureFlag (weFOutlineHilite, weBitClear, we);
  active = WEIsActive (we);
  if (active) WEDeactivate (we);
  WESetSelection (wintopfrontier, wintopfrontier+len, we);
  WESetStyle (weDoFont + weDoFace + weDoSize + weDoColor + weDoReplaceFace,
              &prefs.input, we);
  WESetSelection (wintopfrontier, wintopfrontier, we);
  if (active) WEActivate (we);
  WEFeatureFlag (weFOutlineHilite, weBitSet, we);
  WESetSelection (selstart, selend, we);
  if (readonly) WEFeatureFlag (weFReadOnly, weBitSet, we);
  if (autoscroll) WEFeatureFlag (weFAutoScroll, weBitSet, we);
  AdjustScrollBars (winToplevel);
  if (atend) ScrollToEnd (winToplevel);

  WinAdvanceTopFrontier (len);

  -- caml_reading_console;
  return len;
}

int ui_write (int fd, char *buffer, unsigned int nChars)
{
  long selstart, selend;
  WEReference we;
  OSErr err;
  short readonly, autoscroll;
  int atend;

  if (fd != 1 && fd != 2) return write (fd, buffer, nChars);

  Assert (nChars >= 0);
  we = WinGetWE (winToplevel);
  Assert (we != NULL);
  
  if (erroring){  /* overwrite mode to display errors; see terminfo_* */
    error_curpos += nChars;
    if (error_curpos > wintopfrontier) error_curpos = wintopfrontier;
    return nChars;
  }

  atend = ScrollAtEnd (winToplevel);
  autoscroll = WEFeatureFlag (weFAutoScroll, weBitTest, we);
  WEFeatureFlag (weFAutoScroll, weBitClear, we);
  WEGetSelection (&selstart, &selend, we);
  readonly = WEFeatureFlag (weFReadOnly, weBitTest, we);
  WEFeatureFlag (weFReadOnly, weBitClear, we);
  WESetSelection (wintopfrontier, wintopfrontier, we);
  WESetStyle (weDoFont + weDoFace + weDoSize + weDoColor + weDoReplaceFace,
              &prefs.output, we);
  err = WEInsert (buffer, nChars, NULL, NULL, we);
  if (err != noErr){
    WESetSelection (selstart, selend, we);
    return nChars;
  }
  if (selstart >= wintopfrontier){
    selstart += nChars;
    selend += nChars;
  }else if (selend > wintopfrontier){
    selend += nChars;
  }
  WESetSelection (selstart, selend, we);
  if (autoscroll) WEFeatureFlag (weFAutoScroll, weBitSet, we);
  AdjustScrollBars (winToplevel);
  if (atend) ScrollToEnd (winToplevel);

  WinAdvanceTopFrontier (nChars);
  
  return nChars;
}

void ui_print_stderr (char *msg, void *arg)
{
  char buf [1000];
  
  sprintf (buf, msg, arg);
  ui_write (2, buf, strlen (buf));
}

void ui_exit (int return_code)
{
#pragma unused (return_code)
  Str255 buf0;
  Str255 buf1;

  caml_reading_console = 1;  /* hack: don't display rotating cursor */

  if (return_code != 0){
    GetIndString (buf0, kMiscStrings, kWithErrorCodeIdx);
    NumToString ((long) return_code, buf1);
  }else{
    buf0[0] = 0;
    buf1[0] = 0;
  }
  ParamText (buf0, buf1, NULL, NULL);
  InitCursor ();
  modalkeys = kKeysOK;
  NoteAlert (kAlertExit, myModalFilterUPP);

  while (1) GetAndProcessEvents (waitEvent, 0, 0);

  if (winGraphics != NULL) WinCloseGraphics ();
  WinCloseToplevel ();
  rotatecursor_final ();
  FinaliseAndQuit ();
}


/*
  [getenv] in the standalone application
  envPtr is set up by launch_caml_main
*/
char *getenv (const char *name)
{
  Size envlen, i, namelen;

  Assert (envPtr != NULL);
  envlen = GetPtrSize (envPtr);
  namelen = strlen (name);
  i = 0;
  do{
    if (!strncmp (envPtr + i, name, namelen) && envPtr [i+namelen] == '='){
      return envPtr + i + namelen + 1;
    }
    while (envPtr [i] != '\000') ++ i;
    ++ i;
  }while (i < envlen);
  return NULL;
}


/*
  [terminfo] stuff: change the style of displayed text to show the
  error locations.  See also ui_write.
*/

value terminfo_setup (value vchan);
value terminfo_backup (value lines);
value terminfo_standout (value start);
value terminfo_resume (value lines);

#define Good_term_tag 0

value terminfo_setup (value vchan)
{
#pragma unused (vchan)
  value result = alloc (1, Good_term_tag);
  Field (result, 0) = Val_int (1000000000);
  return result;
}

value terminfo_backup (value lines)
{
  long i, j;
  Handle txt;
  char *p;
  WEReference we = WinGetWE (winToplevel);

  Assert (we != NULL);
  txt = WEGetText (we);
  p = (char *) *txt;
  j = wintopfrontier - 1;

  while (j >= 0 && p[j] != '\n') --j;
  for (i = 0; i < Long_val (lines); i++){
    Assert (p[j] == '\n' || j == -1);
    do{ --j; }while (j >= 0 && p[j] != '\n');
  }
  Assert (p[j] == '\n' || j == -1);
  error_curpos = j + 1;
  erroring = 1;
  error_anchor = -1;
  return Val_unit;
}

value terminfo_standout (value start)
{
  if (Bool_val (start) && error_anchor == -1){
    error_anchor = error_curpos;
  }else if (!Bool_val (start) && error_anchor != -1){
    long selstart, selend;
    WEReference we = WinGetWE (winToplevel);
    short readonly;

    Assert (we != NULL);
    WEGetSelection (&selstart, &selend, we);
    readonly = WEFeatureFlag (weFReadOnly, weBitTest, we);
    if (readonly) WEFeatureFlag (weFReadOnly, weBitClear, we);
    WESetSelection (error_anchor, error_curpos, we);
    WESetStyle (weDoFont + weDoFace + weDoSize + weDoColor + weDoReplaceFace,
                &prefs.errors, we);
    if (readonly) WEFeatureFlag (weFReadOnly, weBitSet, we);
    WESetSelection (selstart, selend, we);
    error_anchor = -1;
  }
  return Val_unit;
}

value terminfo_resume (value lines)
{
#pragma unused (lines)
  erroring = 0;
  return Val_unit;
}
