/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include "cst2constr.h"
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

static value stat_aux(struct stat *buf)
{
  value v;

  v = alloc_tuple(12);
  Field (v, 0) = Val_int (buf->st_dev);
  Field (v, 1) = Val_int (buf->st_ino);
  Field (v, 2) = cst_to_constr(buf->st_mode & S_IFMT, file_kind_table,
                               sizeof(file_kind_table) / sizeof(int), 0);
  Field (v, 3) = Val_int(buf->st_mode & 07777);
  Field (v, 4) = Val_int (buf->st_nlink);
  Field (v, 5) = Val_int (buf->st_uid);
  Field (v, 6) = Val_int (buf->st_gid);
  Field (v, 7) = Val_int (buf->st_rdev);
  Field (v, 8) = Val_int (buf->st_size);
  Field (v, 9) = Val_int (buf->st_atime);
  Field (v, 10) = Val_int (buf->st_mtime);
  Field (v, 11) = Val_int (buf->st_ctime);
  return v;
}

value unix_stat(value path)             /* ML */
{
  int ret;
  struct stat buf;
  ret = stat(String_val(path), &buf);
  if (ret == -1) uerror("stat", path);
  return stat_aux(&buf);
}

value unix_lstat(value path)             /* ML */
{
  int ret;
  struct stat buf;
#ifdef HAS_SYMLINK
  ret = lstat(String_val(path), &buf);
#else
  ret = stat(String_val(path), &buf);
#endif
  if (ret == -1) uerror("lstat", path);
  return stat_aux(&buf);
}

value unix_fstat(value fd)             /* ML */
{
  int ret;
  struct stat buf;
  ret = fstat(Int_val(fd), &buf);
  if (ret == -1) uerror("fstat", Nothing);
  return stat_aux(&buf);
}
