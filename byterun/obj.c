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

/* Operations on objects */

#include "alloc.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"

value static_alloc(value size)        /* ML */
{
  return (value) stat_alloc((asize_t) Long_val(size));
}

value static_free(value blk)          /* ML */
{
  stat_free((void *) blk);
  return Val_unit;
}

value static_resize(value blk, value new_size) /* ML */
{
  return (value) stat_resize((char *) blk, (asize_t) Long_val(new_size));
}

value obj_is_block(value arg)             /* ML */
{
  return Val_bool(Is_block(arg));
}

value obj_tag(value arg)                 /* ML */
{
  return Val_int(Tag_val(arg));
}

value obj_block(value tag, value size) /* ML */
{
  value res;
  mlsize_t sz, i;
  tag_t tg;

  sz = Long_val(size);
  tg = Long_val(tag);
  if (sz == 0) return Atom(tg);
  res = alloc(sz, tg);
  for (i = 0; i < sz; i++)
    Field(res, i) = Val_long(0);

  return res;
}

value obj_dup(value arg) /* ML */
{
  value res;
  mlsize_t sz, i;
  tag_t tg;

  sz = Wosize_val(arg);
  if (sz == 0) return arg;

  Begin_root(arg);
  tg = Tag_val(arg);
  res = alloc(sz, tg);
  for (i = 0; i < sz; i++)
    Field(res, i) = Field(arg, i);
  End_roots();

  return res;
}
