/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Pascal Cuoq, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <memory.h>
#include <errno.h>
#include <alloc.h>
#include "unixsupport.h"

value win_findfirst(name)             /* ML */
     value name;
{
  int h;
  value v;
  struct _finddata_t fileinfo;
  value valname = Val_unit;

  Begin_root (valname);
    h = _findfirst(String_val(name),&fileinfo);
    if (h == -1) {
      if (errno == ENOENT)
        raise_end_of_file();
      else
        uerror("opendir", Nothing);
    }
    valname = copy_string(fileinfo.name);
    v = alloc_tuple(2);
    Field(v,0) = valname;
    Field(v,1) = Val_int(h);
  End_roots();
  return v;
}

value win_findnext(valh)        /* ML */
     value valh;
{
  int retcode;
  struct _finddata_t fileinfo;

  retcode = _findnext(Int_val(valh), &fileinfo);
  if (retcode != 0) raise_end_of_file();
  return copy_string(fileinfo.name);
}

value win_findclose(valh)       /* ML */
     value valh;
{
  if (_findclose(Int_val(valh)) != 0) uerror("closedir", Nothing);
  return Val_unit;
}

