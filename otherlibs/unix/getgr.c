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
#include <fail.h>
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"
#include <stdio.h>
#include <grp.h>

static value alloc_group_entry(entry)
     struct group * entry;
{
  value res;
  value name = Val_unit, pass = Val_unit, mem = Val_unit;

  Begin_roots3 (name, pass, mem);
    name = copy_string(entry->gr_name);
    pass = copy_string(entry->gr_passwd);
    mem = copy_string_array(entry->gr_mem);
    res = alloc_tuple(4);
    Field(res,0) = name;
    Field(res,1) = pass;
    Field(res,2) = Val_int(entry->gr_gid);
    Field(res,3) = mem;
  End_roots();
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
