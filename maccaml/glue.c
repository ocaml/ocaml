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
char *getcwd (char *buf, long size);
Handle macos_getfullpathname (short vrefnum, long dirid);

static int erroring = 0;
static long error_curpos;
static long error_anchor = -1;

/* This handle contains the environment variables. */
char *envPtr = NULL;


/* caml_at_work and Caml_working are used to manage the processor idle
   state on PowerBooks (and also the beachball cursor: see AdjustCursor)
*/
int caml_at_work = 0;

/* Set caml_at_work to true or false.  caml_at_work must always be
   changed through this function, never directly. */
void Caml_working (int newstate)
{
  if (gHasPowerManager){
    if (caml_at_work && !newstate) EnableIdle ();
    if (!caml_at_work && newstate) DisableIdle ();
  }
  caml_at_work = newstate;
}

/* Expand the percent escapes in the string specified by s.
   The escapes are:
   %a application file name
   %d full pathname of the current working directory (ends in ':')
   %t full pathname of the temporary directory (ends in ':')
   %% %
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

/* [launch_caml_main] is called by [main].
   It builds the command line according to the template found in
   the 'Line'(kCommandLineTemplate) resource and the environment
   variables according to the 'Line'(kEnvironmentTemplate).

   Each of these resources is a sequence of strings separated by null
   bytes.  In each string, percent escapes are expanded (see above for
   a description of percent escapes).

   Each resource must end with a null byte.
*/

OSErr launch_caml_main (void)
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
  memcpy (args, *template, len);

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
  memcpy (envPtr, *template, len);

  rotatecursor_init (&something_to_do);
  err = WinOpenToplevel ();
  if (err != noErr) ExitApplication ();

  Assert (!caml_at_work);
  Caml_working (1);

  caml_main (argv);
  return noErr;   /* Not reached */

  failed:
    if (template != NULL) ReleaseResource (template);
    if (args != NULL) free (args);
    if (argv != NULL) free (argv);
    return err;
}


/***
  ui_* stubs for I/O
*/

static void (**atexit_list) (void) = NULL;
static long atexit_size = 0;
static long atexit_len = 0;

void ui_exit (int return_code)
{
  int i;

  for (i = 0; i < atexit_len; i++) (*(atexit_list [i])) ();

  Assert (caml_at_work);
  Caml_working (0);

  if (return_code != 0){
    Str255 errorstr;

    NumToString ((long) return_code, errorstr);
    ParamText (errorstr, NULL, NULL, NULL);
    modalkeys = kKeysOK;
    InitCursor ();
    NoteAlert (kAlertNonzeroExit, myModalFilterUPP);
  }
  while (1) GetAndProcessEvents (waitEvent, 0, 0);
}

int atexit (void (*f) (void))
{
  if (atexit_list == NULL){
    atexit_list = malloc (5 * sizeof (atexit_list [0]));
    if (atexit_list == NULL) goto failed;
    atexit_size = 5;
  }else if (atexit_len >= atexit_size){
    void *p = realloc (atexit_list, (atexit_size+10) * sizeof (atexit_list[0]));
    if (p == NULL) goto failed;
    atexit_list = p;
    atexit_size += 10;
  }
  Assert (atexit_size > atexit_len);
  atexit_list [atexit_len++] = f;
  return 0;

  failed:
    /* errno = ENOMEM;    est-ce que malloc positionne errno ? */
    return -1;
}

int ui_read (int file_desc, char *buf, unsigned int length)
{
  if (file_desc == 0){                  /* Read from the toplevel window. */
    long len, i;
    char **htext;
    WEReference we = WinGetWE (winToplevel);
    long selstart, selend;
    Boolean active;
    short readonly, autoscroll;
    int atend;

    Assert (we != NULL);
    htext = (char **) WEGetText (we);

    Assert (caml_at_work);
    Caml_working (0);

    while (1){
      char *p = *htext;    /* The Handle is not locked.  Be careful with p. */
      len = WEGetTextLength (we);
      for (i = wintopfrontier; i < len; i++){
        if (p[i] == '\n') goto gotit;
      }
      GetAndProcessEvents (waitEvent, 0, 0);
    }

    gotit:

    Assert (!caml_at_work);
    Caml_working (1);

    len = i+1 - wintopfrontier;
    if (len > length) len = length;
    memcpy (buf, (*htext)+wintopfrontier, len);

    atend = ScrollAtEnd (winToplevel);
    autoscroll = WEFeatureFlag (weFAutoScroll, weBitTest, we);
    WEFeatureFlag (weFAutoScroll, weBitClear, we);
    WEGetSelection (&selstart, &selend, we);
    readonly = WEFeatureFlag (weFReadOnly, weBitTest, we);
    WEFeatureFlag (weFReadOnly, weBitClear, we);
    /* Always set an empty selection before changing OutlineHilite. */
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
    return len;
  }else{
    return read (file_desc, buf, length);
  }
}

int ui_write (int file_desc, char *buf, unsigned int length)
{
  if (file_desc == 1 || file_desc == 2){  /* Send to the toplevel window. */
    long selstart, selend;
    WEReference we = WinGetWE (winToplevel);
    OSErr err;
    short readonly, autoscroll;
    int atend;

    if (erroring){  /* overwrite mode to display errors; see terminfo_* */
      error_curpos += length;  Assert (error_curpos <= wintopfrontier);
      return length;
    }

    Assert (we != NULL);
    
    atend = ScrollAtEnd (winToplevel);
    autoscroll = WEFeatureFlag (weFAutoScroll, weBitTest, we);
    WEFeatureFlag (weFAutoScroll, weBitClear, we);
    WEGetSelection (&selstart, &selend, we);
    readonly = WEFeatureFlag (weFReadOnly, weBitTest, we);
    WEFeatureFlag (weFReadOnly, weBitClear, we);
    WESetSelection (wintopfrontier, wintopfrontier, we);
    WESetStyle (weDoFont + weDoFace + weDoSize + weDoColor + weDoReplaceFace,
                &prefs.output, we);
    err = WEInsert (buf, (SInt32) length, NULL, NULL, we);
    if (err != noErr){
      WESetSelection (selstart, selend, we);
      /* XXX should set errno */
      return -1;
    }
    if (selstart >= wintopfrontier){
      selstart += length;
      selend += length;
    }else if (selend > wintopfrontier){
      selend += length;
    }
    WESetSelection (selstart, selend, we);
    if (autoscroll) WEFeatureFlag (weFAutoScroll, weBitSet, we);
    AdjustScrollBars (winToplevel);
    if (atend) ScrollToEnd (winToplevel);

    WinAdvanceTopFrontier (length);
    return length;
  }else{
    return write (file_desc, buf, length);
  }
}

void ui_print_stderr (char *format, void *arg)
{
  char buf [1000];  /* XXX fixed size buffer :-( */

  sprintf (buf, format, arg);   Assert (strlen (buf) < 1000);
  ui_write (2, buf, strlen (buf));
}


/***
  animated cursor (only when toplevel window is frontmost)
*/
typedef struct {
  short nframes;
  short current;
  union {
    CursHandle h;
    struct { short id; short fill; } i;
  } frames [1];
} **AnimCursHandle;

static AnimCursHandle acurh;

pascal void InitCursorCtl (acurHandle newCursors)
{
#pragma unused (newCursors)
  long i;

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

/* In O'Caml, counter is always a multiple of 32. */
pascal void RotateCursor (long counter)
{
  (*acurh)->current += (*acurh)->nframes + (counter >= 0 ? 1 : -1);
  (*acurh)->current %= (*acurh)->nframes;
  GetAndProcessEvents (noWait, 0, 0);
}

void DisplayRotatingCursor (void)
{
  SetCursor (*((*acurh)->frames[(*acurh)->current].h));
}

/***
  "getenv" in the standalone application
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


/***
  "terminfo" stuff: change the style of displayed text to show the
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

  for (i = 0; i < Long_val (lines); i++){
    Assert (p[j] == '\n');
    do{ --j; }while (p[j] != '\n');
  }
  Assert (p[j] == '\n');
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
