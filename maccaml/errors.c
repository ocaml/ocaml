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

static int exiting = 0;

void assert_failure (char *condition, char *file, int line)
{
  Str255 buf;

  if (exiting) ExitToShell ();
  exiting = 1;
  NumToString ((long) line, buf);
  ParamText (c2pstr (condition), c2pstr (file), buf, NULL);
  InitCursor ();
  modalkeys = kKeysOK;
  StopAlert (kAlertBug, myModalFilterUPP);
  ExitApplication ();
}

void XXX (void)
{
  InitCursor ();
  modalkeys = kKeysOK;
  StopAlert (kAlertNotYet, myModalFilterUPP);
}

void ErrorAlert (short msg1, Str255 bufmsg2, short msg3, OSErr err)
{
  Str255 bufmsg1, bufmsg3, bufmsg4;
  short msg;

  switch (err){
  case noErr:
  case userCanceledErr: return;

  case mFulErr:
  case memFullErr:
  case cTempMemErr:
  case cNoMemErr:
  case updPixMemErr: msg = kMemFull; break;
  case dskFulErr:
  case afpDiskFull: msg = kDiskFull; break;
  case dirFulErr: msg = kDirFull; break;
  case tmfoErr:
  case afpTooManyFilesOpen: msg = kTooManyFiles; break;
  case fnfErr: msg = kFileNotFound; break;
  case wPrErr: msg = kWriteProtect; break;
  case fLckdErr:
  case afpObjectLocked: msg = kFileLocked; break;
  case vLckdErr:
  case afpVolLocked: msg = kVolLocked; break;
  case fBsyErr:
  case afpFileBusy: msg = kFileBusy; break;
  case opWrErr: msg = kFileOpen; break;
  case volOffLinErr: msg = kVolOffLine; break;
  case permErr:
  case afpAccessDenied: msg = kPermDenied; break;
  case wrPermErr: msg = kWritePermDenied; break;
  case dirNFErr: msg = kDirNotFound; break;
  case volGoneErr:
  case afpSessClosed: msg = kDisconnected; break;
  case ioErr: msg = kIOError; break;

  default: msg = 0; break;
  }

  GetIndString (bufmsg1, kMiscStrings, msg1);
  GetIndString (bufmsg3, kMiscStrings, msg3);

  if (msg != 0){
    GetIndString (bufmsg4, kErrorStrings, msg);
    ParamText (bufmsg1, bufmsg2, bufmsg3, bufmsg4);
  }else{
    NumToString (err, bufmsg4);
    ParamText (bufmsg1, bufmsg2, bufmsg3, bufmsg4);
  }
  InitCursor ();
  modalkeys = kKeysOK;
  StopAlert (msg ? kAlertErrorMsg : kAlertErrorNum, myModalFilterUPP);
}

void ErrorAlertCantOpen (Str255 filename, OSErr err)
{
  ErrorAlert (kCannotOpenIdx, filename, kCloseQuoteIdx, err);
}

void ErrorAlertGeneric (OSErr err)
{
  ErrorAlert (kEmptyIdx, "\p", kEmptyIdx, err);
}

OSErr InitialiseErrors (void)
{
/* XXX CouldAlert n'existe plus ??
  CouldAlert (kAlertErrorMsg);
  CouldAlert (kAlertErrorNum);
  CouldAlert (kAlertBug);
*/
  return noErr;
}
