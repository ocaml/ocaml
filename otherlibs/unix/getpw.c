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
#include <memory.h>
#include <fail.h>
#include "unixsupport.h"
#include <pwd.h>

static value alloc_passwd_entry(entry)
     struct passwd * entry;
{
  value res;
  Push_roots(s, 5);

  s[0] = copy_string(entry->pw_name);
  s[1] = copy_string(entry->pw_passwd);
  s[2] = copy_string(entry->pw_gecos);
  s[3] = copy_string(entry->pw_dir);
  s[4] = copy_string(entry->pw_shell);
  res = alloc_tuple(7);
  Field(res,0) = s[0];
  Field(res,1) = s[1];
  Field(res,2) = Val_int(entry->pw_uid);
  Field(res,3) = Val_int(entry->pw_gid);
  Field(res,4) = s[2];
  Field(res,5) = s[3];
  Field(res,6) = s[4];
  Pop_roots();
  return res;
}

value unix_getpwnam(name)        /* ML */
     value name;
{
  struct passwd * entry;
  entry = getpwnam(String_val(name));
  if (entry == (struct passwd *) NULL) raise_not_found();
  return alloc_passwd_entry(entry);
}

value unix_getpwuid(uid)         /* ML */
     value uid;
{
  struct passwd * entry;
  entry = getpwuid(Int_val(uid));
  if (entry == (struct passwd *) NULL) raise_not_found();
  return alloc_passwd_entry(entry);
}
