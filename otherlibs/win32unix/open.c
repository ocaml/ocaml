/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include <fcntl.h>

static int open_access_flags[8] = {
  GENERIC_READ, GENERIC_WRITE, GENERIC_READ|GENERIC_WRITE, 0, 0, 0, 0, 0,
};

static int open_create_flags[8] = {
  0, 0, 0, 0, 0, O_CREAT, O_TRUNC, O_EXCL
};

CAMLprim value unix_open(value path, value flags, value perm)
{
  int fileaccess, createflags, fileattrib, filecreate;
  SECURITY_ATTRIBUTES attr;
  HANDLE h;

  fileaccess = convert_flag_list(flags, open_access_flags);

  createflags = convert_flag_list(flags, open_create_flags);
  if ((createflags & (O_CREAT | O_EXCL)) == (O_CREAT | O_EXCL))
    filecreate = CREATE_NEW;
  else if ((createflags & (O_CREAT | O_TRUNC)) == (O_CREAT | O_TRUNC))
    filecreate = CREATE_ALWAYS;
  else if (createflags & O_TRUNC)
    filecreate = TRUNCATE_EXISTING;
  else if (createflags & O_CREAT)
    filecreate = OPEN_ALWAYS;
  else
    filecreate = OPEN_EXISTING;

  if ((createflags & O_CREAT) && (Int_val(perm) & 0200) == 0)
    fileattrib = FILE_ATTRIBUTE_READONLY;
  else
    fileattrib = FILE_ATTRIBUTE_NORMAL;

  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  h = CreateFile(String_val(path), fileaccess,
                 FILE_SHARE_READ | FILE_SHARE_WRITE, &attr,
                 filecreate, fileattrib, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("open", path);
  }
  return win_alloc_handle(h);
}
