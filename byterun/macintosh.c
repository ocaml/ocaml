/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Macintosh-specific stuff */

#include <CursorCtl.h>
#include <Files.h>
#include <IntEnv.h>
#include <stdio.h>
#include <stdlib.h>
#include <Strings.h>
#include <TextUtils.h>
#include <Timer.h>
#include <Types.h>

/* The user interface defaults to MPW tool.  The standalone application
   overrides the ui_* functions, as well as [main], [InitCursorCtl],
   [RotateCursor], [atexit], [getenv], and the terminfo functions.
*/

void ui_exit (int return_code)
{
  exit (return_code);
}

int ui_read (int file_desc, char *buf, unsigned int length)
{
  return read (file_desc, buf, length);
}

int ui_write (int file_desc, char *buf, unsigned int length)
{
  return write (file_desc, buf, length);
}

void ui_print_stderr (char *format, void *arg)
{
  fprintf (stderr, format, arg);
}


/* Unix emulation stuff */

static short prevdir = 0;

int chdir (char *dir)
{
  WDPBRec pb;
  int result;
  short curdir;

  pb.ioCompletion = NULL;
  pb.ioNamePtr = c2pstr (dir);
  pb.ioVRefNum = 0;
  pb.ioWDProcID = 'Caml';
  pb.ioWDDirID = 0;
  result = PBOpenWDSync (&pb);
  p2cstr ((unsigned char *) dir);
  if (result != noErr) return -1;
  curdir = pb.ioVRefNum;
  result = SetVol (NULL, curdir);
  if (result != noErr) return -1;
  if (prevdir != 0){
    pb.ioVRefNum = prevdir;
    PBCloseWDSync (&pb);
  }
  prevdir = curdir;
  return 0;
}

Handle macos_getfullpathname (short vrefnum, long dirid)
{
  Handle result = NewHandle (0);
  CInfoPBRec mypb;
  Str255 dirname;
  OSErr err;

  if (result == NULL) goto failed;

  mypb.dirInfo.ioNamePtr = dirname;
  mypb.dirInfo.ioVRefNum = vrefnum;
  mypb.dirInfo.ioDrParID = dirid;
  mypb.dirInfo.ioFDirIndex = -1;

  do{
    mypb.dirInfo.ioDrDirID = mypb.dirInfo.ioDrParID;
    err = PBGetCatInfo (&mypb, false);
    if (err) goto failed;
    Munger (result, 0, NULL, 0, ":", 1);
    Munger (result, 0, NULL, 0, dirname+1, dirname[0]);
    /* XXX out of memory ?! */
  }while (mypb.dirInfo.ioDrDirID != fsRtDirID);
  return result;

  failed:
    if (result != NULL) DisposeHandle (result);
    return NULL;
}

char *getcwd (char *buf, long size)
{
  size_t len;

  Handle path = macos_getfullpathname (0, 0);
  if (path == NULL) return NULL;

  len = GetHandleSize (path);

  if (len+1 >= size){
    DisposeHandle (path);
    return NULL;
  }
  if (buf == NULL){
    buf = malloc (len+1);
    if (buf == NULL) return NULL;
  }
  memcpy (buf, *path, len);
  buf [len] = '\000';
  DisposeHandle (path);
  return buf;
}

int system (char const *cmd)
{
  char *filename;
  FILE *f;

  if (StandAlone) return -1;

  filename = getenv ("ocamlcommands");
  if (filename == NULL) return 1;
  f = fopen (filename, "a");
  if (f == NULL) return 1;
  fprintf (f, "%s\n", cmd);
  fclose (f);
  return 0;
}
