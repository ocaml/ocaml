/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <CursorCtl.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

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


/* The last time that O'Caml was seen doing some compute-bound work,
   as returned by [TickCount]. */
static UInt32 caml_working_date;

/* The number of threads that are blocked reading from the console. */
static int caml_reading_console = 0;

/* [Caml_working] is used to manage the processor idle state on
   PowerBooks.  [Caml_working (1)] disable the idle state, and
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
  caml_working_date = TickCount ();
  RotateCursor (counter);
  sched_yield ();
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

static void caml_main_then_exit (char **argv)
{
  caml_main (argv);
  exit (0);
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
  pthread_t topthread;
  pthread_attr_t attr;
  OSErr err;
  int res;
  sigset_t mask;

  rotatecursor_options (&something_to_do, 0, &interp_yield);
  err = WinOpenToplevel ();
  if (err != noErr) goto failed;

  err = build_command_line (&argv);
  if (err) goto failed;
  pthread_attr_init (&attr);
  pthread_attr_setstacksize (&attr, 256 * 1024);
  res = pthread_create (&topthread, &attr, (void *) caml_main_then_exit,
                        (void *) argv);
  pthread_attr_destroy (&attr);
  if (res != 0){
    err = -1;
    goto failed;
  }

  /* Block all signals in this thread. */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, NULL);

  caml_working_date = TickCount ();
  Caml_working (1);
  while (!quit_requested){
    int towait = waitEvent;
    if (motion_requested) towait = waitMove;
    if (TickCount () < caml_working_date + 120){
      towait = noWait;
      Caml_working (1);
    }else{
      Caml_working (0);
    }
    GetAndProcessEvents (towait, motion_oldx, motion_oldy);
    sched_yield ();
  }
  return noErr;

  failed:
    return err;
}

/* SIO stubs */

/* Management of error highlighting. */
static int erroring = 0;
static long error_curpos;
static long error_anchor = -1;


static pascal void caml_sio_init (int *mainArgc, char ***mainArgv)
{
#pragma unused (mainArgc, mainArgv)
}
pascal void (*__sioInit) (int *, char ***) = &caml_sio_init;

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

static pascal void caml_sio_read (char *buffer, SInt32 nCharsDesired,
                                  SInt32 *nCharsUsed, SInt16 *eofFlag)
{
#pragma unused (eofFlag)
  long len, i;
  char **htext;
  WEReference we;
  long selstart, selend;
  Boolean active;
  short readonly, autoscroll;
  int atend;

  we = WinGetWE (winToplevel);
  Assert (we != NULL);
  htext = (char **) WEGetText (we);

  while (1){
    char *p;

    len = WEGetTextLength (we);
    p = *htext;
    for (i = wintopfrontier; i < len; i++){
      if (p[i] == '\n') goto gotit;
    }
    ++ caml_reading_console;
    sched_yield ();
    -- caml_reading_console;
    if (pending_signal != 0 && pending_signal != SIGVTALRM){
      /* handle signals, but not the tick thread stuff because:
         1. We don't hold the master lock so it is not needed.
         2. It would execute some Caml code and prevent processor idle.
      */
      leave_blocking_section ();
      enter_blocking_section ();
    }
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
  *nCharsUsed = len;

  caml_working_date = TickCount ();
  return;
}
pascal void (*__sioRead) (char *, SInt32, SInt32 *, SInt16 *) = &caml_sio_read;

static pascal void caml_sio_write (SInt16 filenum, char *buffer, SInt32 nChars)
{
#pragma unused (filenum)
  long selstart, selend;
  WEReference we = WinGetWE (winToplevel);
  OSErr err;
  short readonly, autoscroll;
  int atend;

  Assert (nChars >= 0);
  Assert (we != NULL);
  
  if (erroring){  /* overwrite mode to display errors; see terminfo_* */
    error_curpos += nChars;
    if (error_curpos > wintopfrontier) error_curpos = wintopfrontier;
    return;
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
    return; /* FIXME raise an exception ? */
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
  
  caml_working_date = TickCount ();
  return;
}
pascal void (*__sioWrite) (SInt16, char *, SInt32) = &caml_sio_write;

static pascal void caml_sio_exit (void)
{
  caml_reading_console = 1;  /* hack: don't display rotating cursor */
  if (!quit_requested){
    modalkeys = kKeysOK;
    InitCursor ();
    NoteAlert (kAlertExit, myModalFilterUPP);
  }
  while (!quit_requested) GetAndProcessEvents (waitEvent, 0, 0);
  if (winGraphics != NULL) WinCloseGraphics ();
  WinCloseToplevel ();
  rotatecursor_final ();
  Finalise ();
}
pascal void (*__sioExit) (void) = &caml_sio_exit;


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
