/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Macintosh-specific stuff */

#include <CursorCtl.h>
#include <Files.h>
#include <stdio.h>
#include <stdlib.h>
#include <Strings.h>
#include <TextUtils.h>
#include <Timer.h>
#include <Types.h>

/* The user interface defaults to MPW tool.  The standalone application
   replaces these functions, as well as [main], [InitCursorCtl],
   [RotateCursor], [atexit]
   (see rotatecursor.c).
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

  pb.ioCompletion = NULL;
  pb.ioNamePtr = c2pstr (dir);
  pb.ioVRefNum = 0;
  pb.ioWDProcID = 'Caml';
  pb.ioWDDirID = 0;
  result = PBOpenWDSync (&pb);
  p2cstr ((unsigned char *) dir);
  if (result != noErr) return -1;
  result = SetVol (NULL, pb.ioVRefNum);
  if (result != noErr) return -1;
  if (prevdir != 0){
    prevdir = 0;
	pb.ioVRefNum = prevdir;
	PBCloseWDSync (&pb);
  }
  return 0;
}

static char *getfullpathid (short wd, long id);

static void cat_cp_str (char **cstr, StringPtr pstr)
{
  int l2 = (unsigned char) pstr [0];
  int l1 = strlen (*cstr);
  int i;

  *cstr = realloc (*cstr, l1 + l2);
  if (*cstr == NULL) return;
  for (i = 0; i < l2; i++){
    (*cstr)[l1 + i] = pstr [i + 1];
  }
  (*cstr)[l1 + l2] = '\0';
}

static char *getfullpathpb (CInfoPBPtr pb)
{
  char *result;

  if (pb->hFileInfo.ioFlParID == fsRtParID){
	result = malloc (1);
	if (result == NULL) return NULL;
	result [0] = '\0';
  }else{
    result = getfullpathid (pb->hFileInfo.ioVRefNum, pb->hFileInfo.ioFlParID);
	if (result == NULL) return NULL;
  }
  cat_cp_str (&result, pb->hFileInfo.ioNamePtr);
  if (pb->hFileInfo.ioFlAttrib & (1<<4)) cat_cp_str (&result, "\p:");
  return result;
}

static char *getfullpathcwd (void)
{
  CInfoPBRec pb;
  Str255 pname;

  pname [0] = 1;
  pname [1] = ':';

  pb.hFileInfo.ioCompletion = NULL;
  pb.hFileInfo.ioVRefNum = 0;
  pb.hFileInfo.ioNamePtr = pname;
  pb.hFileInfo.ioFRefNum = 0;
  pb.hFileInfo.ioFVersNum = 0;
  pb.hFileInfo.ioFDirIndex = 0;
  pb.hFileInfo.ioDirID = 0;
  if (PBGetCatInfoSync (&pb) != noErr) return NULL;
  pb.hFileInfo.ioFDirIndex = -1;
  if (PBGetCatInfoSync (&pb) != noErr) return NULL;
  return getfullpathpb (&pb);
}

static char *getfullpathid (short wd, long id)
{
  CInfoPBRec pb;
  Str255 name;

  pb.hFileInfo.ioCompletion = NULL;
  pb.hFileInfo.ioNamePtr = name;
  pb.hFileInfo.ioVRefNum = wd;
  pb.hFileInfo.ioFRefNum = 0;
  pb.hFileInfo.ioFVersNum = 0;
  pb.hFileInfo.ioFDirIndex = -1;
  pb.hFileInfo.ioDirID = id;
  if (PBGetCatInfoSync (&pb) != noErr) return NULL;
  return getfullpathpb (&pb);
}

char *getcwd (char *buf, long size)
{
  char *path = getfullpathcwd ();

  if (path == NULL) return NULL;
  if (strlen (path) >= size){
    free (path);
    return NULL;
  }
  if (buf == NULL){
    return path;
  }
  strcpy (buf, path);
  free (path);
  return buf;
}

int system (char const *cmd)
{
  char *filename;
  FILE *f;
  
  filename = getenv ("ocamlcommands");
  if (filename == NULL) return 1;
  f = fopen (filename, "a");
  if (f == NULL) return 1;
  fprintf (f, "%s\n", cmd);
  fclose (f);
  return 0;
}
