/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <errno.h>
#include <mlvalues.h>
#include <memory.h>
#include <alloc.h>
#include "unixsupport.h"
#include "cst2constr.h"
#define _INTEGRAL_MAX_BITS 64
#include <sys/types.h>
#include <sys/stat.h>

#ifndef S_IFLNK
#define S_IFLNK 0
#endif
#ifndef S_IFIFO
#define S_IFIFO 0
#endif
#ifndef S_IFSOCK
#define S_IFSOCK 0
#endif
#ifndef S_IFBLK
#define S_IFBLK 0
#endif

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

static value stat_aux(int use_64, struct _stati64 *buf)
{
  value v;
  value atime = Val_unit, mtime = Val_unit, ctime = Val_unit;

  Begin_roots3(atime,mtime,ctime)
    atime = copy_double((double) buf->st_atime);
    mtime = copy_double((double) buf->st_mtime);
    ctime = copy_double((double) buf->st_ctime);
    v = alloc_small(12, 0);
    Field (v, 0) = Val_int (buf->st_dev);
    Field (v, 1) = Val_int (buf->st_ino);
    Field (v, 2) = cst_to_constr(buf->st_mode & S_IFMT, file_kind_table,
                                 sizeof(file_kind_table) / sizeof(int), 0);
    Field (v, 3) = Val_int(buf->st_mode & 07777);
    Field (v, 4) = Val_int (buf->st_nlink);
    Field (v, 5) = Val_int (buf->st_uid);
    Field (v, 6) = Val_int (buf->st_gid);
    Field (v, 7) = Val_int (buf->st_rdev);
    Field (v, 8) = 
      use_64 ? copy_int64(buf->st_size) : Val_int (buf->st_size);
    Field (v, 9) = atime;
    Field (v, 10) = mtime;
    Field (v, 11) = ctime;
  End_roots();
  return v;
}

CAMLprim value unix_stat(value path)
{
  int ret;
  struct _stati64 buf;

  ret = _stati64(String_val(path), &buf);
  if (ret == -1) uerror("stat", path);
  if (buf.st_size > Max_long) {
    win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
    uerror("stat", path);
  }
  return stat_aux(0, &buf);
}

CAMLprim value unix_stat_64(value path)
{
  int ret;
  struct _stati64 buf;
  ret = _stati64(String_val(path), &buf);
  if (ret == -1) uerror("stat", path);
  return stat_aux(1, &buf);
}

