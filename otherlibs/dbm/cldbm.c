/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Francois Rouaix, projet Cristal, INRIA Rocquencourt      */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <string.h>
#include <fcntl.h>
#include <ndbm.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <str.h>
#include <callback.h>

/* Quite close to sys_open_flags, but we need RDWR */
static int dbm_open_flags[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_CREAT
};

static void raise_dbm P((char *errmsg)) Noreturn;

static void raise_dbm(errmsg)
     char *errmsg;
{
  static value * dbm_exn = NULL;
  if (dbm_exn == NULL)
    dbm_exn = caml_named_value("dbmerror");
  raise_with_string(*dbm_exn, errmsg);
}

/* Dbm.open : string -> Sys.open_flag list -> int -> t */
value caml_dbm_open(vfile, vflags, vmode) /* ML */
     value vfile;
     value vflags;
     value vmode;
{
  char *file = String_val(vfile);
  int flags = convert_flag_list(vflags, dbm_open_flags);
  int mode = Int_val(vmode);
  DBM *db = dbm_open(file,flags,mode);

  if (db == NULL) 
    raise_dbm("Can't open file");
  else
    return ((value)db);
}

/* Dbm.close: t -> unit */
value caml_dbm_close(vdb)       /* ML */
     value vdb;
{
  dbm_close((DBM *)vdb);
  return Val_unit;
}

/* Dbm.fetch: t -> string -> string */
value caml_dbm_fetch(vdb,vkey)  /* ML */
     value vdb;
     value vkey;
{
  datum key,answer;
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);
  answer = dbm_fetch((DBM *)vdb, key);
  if (answer.dptr) {
    value res = alloc_string(answer.dsize);
    bcopy(answer.dptr,String_val(res),answer.dsize);
    return res;
  }
  else raise_not_found();
}

value caml_dbm_insert(vdb,vkey,vcontent) /* ML */
     value vdb,vkey,vcontent;
{
  datum key, content;
  
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);
  content.dptr = String_val(vcontent);
  content.dsize = string_length(vcontent);

  switch(dbm_store((DBM *)vdb, key, content, DBM_INSERT)) {
  case 0:
    return Val_unit;
  case 1:                       /* DBM_INSERT and already existing */
    raise_dbm("Entry already exists");
  default:
    raise_dbm("dbm_store failed");
  }
}


value caml_dbm_replace(vdb,vkey,vcontent) /* ML */
     value vdb,vkey,vcontent;
{
  datum key, content;
  
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);
  content.dptr = String_val(vcontent);
  content.dsize = string_length(vcontent);

  switch(dbm_store((DBM *)vdb, key, content, DBM_REPLACE)) {
  case 0:
    return Val_unit;
  default:
    raise_dbm("dbm_store failed");
  }
}

value caml_dbm_delete(vdb,vkey)         /* ML */
     value vdb, vkey;
{
  datum key;
  key.dptr = String_val(vkey);
  key.dsize = string_length(vkey);

  if (dbm_delete((DBM *)vdb, key) < 0)
    raise_dbm("dbm_delete");
  else return Val_unit;
}

value caml_dbm_firstkey(vdb)            /* ML */
  value vdb;
{
  datum key = dbm_firstkey((DBM *)vdb);

  if (key.dptr) {
    value res = alloc_string(key.dsize);
    bcopy(key.dptr,String_val(res),key.dsize);
    return res;
  }
  else raise_not_found();
}

value caml_dbm_nextkey(vdb)             /* ML */
  value vdb;
{
  datum key = dbm_nextkey((DBM *)vdb);

  if (key.dptr) {
    value res = alloc_string(key.dsize);
    bcopy(key.dptr,String_val(res),key.dsize);
    return res;
  }
  else raise_not_found();
}
