/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Francois Rouaix, projet Cristal, INRIA Rocquencourt      */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <string.h>
#include <fcntl.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <callback.h>

#ifdef DBM_USES_GDBM_NDBM
#include <gdbm-ndbm.h>
#else
#include <ndbm.h>
#endif

/* Quite close to sys_open_flags, but we need RDWR */
static int dbm_open_flags[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_CREAT
};

static void raise_dbm (char *errmsg) Noreturn;

static void raise_dbm(char *errmsg)
{
  static value * dbm_exn = NULL;
  if (dbm_exn == NULL)
    dbm_exn = caml_named_value("dbmerror");
  raise_with_string(*dbm_exn, errmsg);
}

#define DBM_val(v) *((DBM **) &Field(v, 0))

static value alloc_dbm(DBM * db)
{
  value res = alloc_small(1, Abstract_tag);
  DBM_val(res) = db;
  return res;
}

static DBM * extract_dbm(value vdb)
{
  if (DBM_val(vdb) == NULL) raise_dbm("DBM has been closed");
  return DBM_val(vdb);
}

/* Dbm.open : string -> Sys.open_flag list -> int -> t */
value caml_dbm_open(value vfile, value vflags, value vmode) /* ML */
{
  char *file = String_val(vfile);
  int flags = convert_flag_list(vflags, dbm_open_flags);
  int mode = Int_val(vmode);
  DBM *db = dbm_open(file,flags,mode);

  if (db == NULL) 
    raise_dbm("Can't open file");
  else
    return (alloc_dbm(db));
}

/* Dbm.close: t -> unit */
value caml_dbm_close(value vdb)       /* ML */
{
  dbm_close(extract_dbm(vdb));
  DBM_val(vdb) = NULL;
  return Val_unit;
}

/* Dbm.fetch: t -> string -> string */
value caml_dbm_fetch(value vdb, value vkey)  /* ML */
{
  datum key,answer;
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);
  answer = dbm_fetch(extract_dbm(vdb), key);
  if (answer.dptr) {
    value res = alloc_string(answer.dsize);
    memmove (String_val (res), answer.dptr, answer.dsize);
    return res;
  }
  else raise_not_found();
}

value caml_dbm_insert(value vdb, value vkey, value vcontent) /* ML */
{
  datum key, content;
  
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);
  content.dptr = String_val(vcontent);
  content.dsize = string_length(vcontent);

  switch(dbm_store(extract_dbm(vdb), key, content, DBM_INSERT)) {
  case 0:
    return Val_unit;
  case 1:                       /* DBM_INSERT and already existing */
    raise_dbm("Entry already exists");
  default:
    raise_dbm("dbm_store failed");
  }
}

value caml_dbm_replace(value vdb, value vkey, value vcontent) /* ML */
{
  datum key, content;
  
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);
  content.dptr = String_val(vcontent);
  content.dsize = string_length(vcontent);

  switch(dbm_store(extract_dbm(vdb), key, content, DBM_REPLACE)) {
  case 0:
    return Val_unit;
  default:
    raise_dbm("dbm_store failed");
  }
}

value caml_dbm_delete(value vdb, value vkey)         /* ML */
{
  datum key;
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);

  if (dbm_delete(extract_dbm(vdb), key) < 0)
    raise_dbm("dbm_delete");
  else return Val_unit;
}

value caml_dbm_firstkey(value vdb)            /* ML */
{
  datum key = dbm_firstkey(extract_dbm(vdb));

  if (key.dptr) {
    value res = alloc_string(key.dsize);
    memmove (String_val (res), key.dptr, key.dsize);
    return res;
  }
  else raise_not_found();
}

value caml_dbm_nextkey(value vdb)             /* ML */
{
  datum key = dbm_nextkey(extract_dbm(vdb));

  if (key.dptr) {
    value res = alloc_string(key.dsize);
    memmove (String_val (res), key.dptr, key.dsize);
    return res;
  }
  else raise_not_found();
}
