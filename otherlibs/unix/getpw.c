/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include "unixsupport.h"
#include <pwd.h>

static value alloc_passwd_entry(struct passwd *entry)
{
  value res;
  value name = Val_unit, passwd = Val_unit, gecos = Val_unit;
  value dir = Val_unit, shell = Val_unit;

  Begin_roots5 (name, passwd, gecos, dir, shell);
    name = copy_string(entry->pw_name);
    passwd = copy_string(entry->pw_passwd);
    gecos = copy_string(entry->pw_gecos);
    dir = copy_string(entry->pw_dir);
    shell = copy_string(entry->pw_shell);
    res = alloc_small(7, 0);
    Field(res,0) = name;
    Field(res,1) = passwd;
    Field(res,2) = Val_int(entry->pw_uid);
    Field(res,3) = Val_int(entry->pw_gid);
    Field(res,4) = gecos;
    Field(res,5) = dir;
    Field(res,6) = shell;
  End_roots();
  return res;
}

value unix_getpwnam(value name)        /* ML */
{
  struct passwd * entry;
  entry = getpwnam(String_val(name));
  if (entry == (struct passwd *) NULL) raise_not_found();
  return alloc_passwd_entry(entry);
}

value unix_getpwuid(value uid)         /* ML */
{
  struct passwd * entry;
  entry = getpwuid(Int_val(uid));
  if (entry == (struct passwd *) NULL) raise_not_found();
  return alloc_passwd_entry(entry);
}
