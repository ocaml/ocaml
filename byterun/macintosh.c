/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* MacOS-specific stuff */

#include <stdio.h>
#include <stdlib.h>

#include <AppleEvents.h>
#include <CursorCtl.h>
#include <Errors.h>
#include <Files.h>
#include <IntEnv.h>
#include <MacTypes.h>
#include <QuickDraw.h>
#include <TextUtils.h>

#include "misc.h"
#include "rotatecursor.h"

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

char *getcwd (char *buf, size_t size)
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

pascal Boolean system_idleproc (const EventRecord *event, long *sleepTime,
                                RgnHandle *mouseRgn)
{
  static RgnHandle myregion = NULL;
  EventRecord evt;
  
  if (myregion == NULL){
    myregion = NewRgn ();
    SetRectRgn (myregion, -32000, -32000, 32000, 32000);
  }

  /* XXX standalone appli: process event */
  *mouseRgn = myregion;
  *sleepTime = 3;
  if (EventAvail (keyDownMask, &evt)
      && (evt.modifiers & cmdKey)
      && ((evt.message & charCodeMask) == '.')){
    return true;
  }else{
    return false;
  }
}

void quote (char *buf, long buflen)
{
  long i, j;

  j = 2;
  for (i = 0; buf[i] != '\0'; i++){
    if (buf[i] == '\'') j += 3;
    ++ j;
  }
  if (j >= buflen) return;

  buf[j--] = '\0';
  buf[j--] = '\'';
  while (i > 0){
    -- i;
    buf[j--] = buf[i];
    if (buf[i] == '\''){
      buf[j--] = '\'';
      buf[j--] = '\266';
      buf[j--] = '\'';
    }
  }
  buf[j] = '\'';                   Assert (j == 0);
}

int system (char const *cmd)
{
  char *fmt = "directory %s; %s";
  char *cmdline;
  char *buf;
  #define buf_size 66000

  static AEIdleUPP myIdleProcUPP = NULL;
  AEAddressDesc serveraddr;
  AppleEvent myevent, reply;
  OSType toolserver_sig = 'MPSX';
  DescType ret_type;
  OSErr err = noErr;
  long event_status = 0, ret_size;
  int result;

  /* once only */
  if (myIdleProcUPP == NULL) myIdleProcUPP = NewAEIdleProc (system_idleproc);

  SetCursor (*GetCursor (watchCursor));
  
  buf = malloc (buf_size);
  if (buf == NULL) goto failed_malloc_buf;

  /* Create the command line */
  getcwd (buf, buf_size);
  quote (buf, buf_size);
  cmdline = malloc (strlen (fmt) + strlen (cmd) + strlen (buf) + 1);
  if (cmdline == NULL) goto failed_malloc_cmdline;
  sprintf (cmdline, fmt, buf, cmd);
  
  /* Send the event and get the reply */
  err = AECreateDesc (typeApplSignature, &toolserver_sig,
                      sizeof (toolserver_sig), &serveraddr);
  if (err != noErr) goto failed_AECreateDesc;
  err = AECreateAppleEvent ('misc', 'dosc', &serveraddr, kAutoGenerateReturnID,
                            kAnyTransactionID, &myevent);
  if (err != noErr) goto failed_AECreateAppleEvent;
  err = AEPutParamPtr (&myevent, '----', 'TEXT', cmdline, strlen (cmdline));
  if (err != noErr) goto failed_AEPutParamPtr;
  err = AESend (&myevent, &reply, kAEWaitReply + kAENeverInteract,
                kAENormalPriority, kNoTimeOut, myIdleProcUPP, NULL);
  if (err != noErr) goto failed_AESend;
  err = AEGetParamPtr (&reply, 'errn', typeLongInteger, &ret_type,
                       &event_status, sizeof (event_status), &ret_size);
  if (err != noErr || event_status != noErr) goto failed_script;
  err = AEGetParamPtr (&reply, 'stat', typeLongInteger, &ret_type,
                       &event_status, sizeof (event_status), &ret_size);
  if (err != noErr || event_status != noErr) goto failed_script;

  /* forward stdout and stderr */
  err = AEGetParamPtr (&reply, 'diag', typeChar, &ret_type,
                       buf, buf_size, &ret_size);
  if (err == noErr) write (2, buf, ret_size);
  err = AEGetParamPtr (&reply, '----', typeChar, &ret_type,
                       buf, buf_size, &ret_size);
  if (err == noErr) write (1, buf, ret_size);
  
  AEDisposeDesc (&reply);
  AEDisposeDesc (&myevent);
  AEDisposeDesc (&serveraddr);
  free (cmdline);
  free (buf);
  RotateCursor (32);
  return 0;

  failed_script:
    AEDisposeDesc (&reply);
  failed_AESend:
  failed_AEPutParamPtr:
    AEDisposeDesc (&myevent);
  failed_AECreateAppleEvent:
    AEDisposeDesc (&serveraddr);
  failed_AECreateDesc:
    free (cmdline);
  failed_malloc_cmdline:
    free (buf);
  failed_malloc_buf:
    if (err != noErr) result = err;
    else if (event_status != 0) result = event_status;
    else result = 1;
    if (result == 0 || result == -1) result = 1;
    RotateCursor (32);
    return result;
}

/* We don't need search_exe_in_path on MacOS 9 because there
   are no #! scripts */

char *search_exe_in_path (char * name)
{
  return name;
}


/* O'Caml's use use of dynamic linking is Unix-specific, these are functions
   from dynlink.c without the dynamic linking stuff.
*/

#include "misc.h"
#include "mlvalues.h"
#include "prims.h"

struct ext_table shared_libs_path;
struct ext_table prim_table;

static c_primitive lookup_primitive(char * name)
{
  int i;
  void * res;

  for (i = 0; names_of_builtin_cprim[i] != NULL; i++) {
    if (strcmp(name, names_of_builtin_cprim[i]) == 0)
      return builtin_cprim[i];
  }
  return NULL;
}

void build_primitive_table(char * lib_path,
                           char * libs,
                           char * req_prims)
{
  char * p;

  caml_ext_table_init(&prim_table, 0x180);
  for (p = req_prims; *p != 0; p += strlen(p) + 1) {
    c_primitive prim = lookup_primitive(p);
    if (prim == NULL)
      caml_fatal_error_arg("Fatal error: unknown C primitive `%s'\n", p);
    caml_ext_table_add(&prim_table, (void *) prim);
  }
}

value dynlink_open_lib (value filename)
{
  return Val_unit;
}

value dynlink_close_lib(value handle)
{
  return Val_unit;
}

value dynlink_lookup_symbol(value handle, value symbolname)
{
  return Val_unit;
}

value dynlink_add_primitive(value handle)
{
  invalid_argument("dynlink_add_primitive");
  return Val_unit; /* not reached */
}

value dynlink_get_current_libs(value unit)
{
  return Atom (0);
}
