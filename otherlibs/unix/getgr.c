/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <fail.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"
#include <stdio.h>
#include <grp.h>

static value alloc_group_entry(entry)
     struct group * entry;
{
  value res;
  Push_roots(s, 3);

  s[0] = copy_string(entry->gr_name);
  s[1] = copy_string(entry->gr_passwd);
  s[2] = copy_string_array(entry->gr_mem);
  res = alloc_tuple(4);
  Field(res,0) = s[0];
  Field(res,1) = s[1];
  Field(res,2) = Val_int(entry->gr_gid);
  Field(res,3) = s[2];
  Pop_roots();
  return res;
}

value unix_getgrnam(name)        /* ML */
     value name;
{
  struct group * entry;
  entry = getgrnam(String_val(name));
  if (entry == NULL) raise_not_found();
  return alloc_group_entry(entry);
}

value unix_getgrgid(gid)         /* ML */
     value gid;
{
  struct group * entry;
  entry = getgrgid(Int_val(gid));
  if (entry == NULL) raise_not_found();
  return alloc_group_entry(entry);
}
