/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Handling of finalised values. */

#include "callback.h"
#include "fail.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"

struct final {
  value fun;
  value val;
};

static struct final *final_table = NULL;
static unsigned long old = 0, young = 0, active = 0, size = 0;
/* [0..old) : finalisable set
   [old..young) : recent set
   [young..active) : free space
   [active..size) : finalising set
*/

/* Find white finalisable values, darken them, and put them in the
   finalising set.
   The recent set is empty.
*/
void final_update (void)
{
  unsigned long i;
  unsigned long oldactive = active;
  
  Assert (young == old);
  Assert (young <= active);
  for (i = 0; i < old; i++){
  again:
    Assert (Is_block (final_table[i].val));
    Assert (Is_in_heap (final_table[i].val));
    if (Is_white_val (final_table[i].val)){
      struct final f;

      if (Tag_val (final_table[i].val) == Forward_tag){
        value fv = Forward_val (final_table[i].val);
        if (Is_block (fv) && (Is_young (fv) || Is_in_heap (fv))
            && (Tag_val (fv) == Forward_tag || Tag_val (fv) == Lazy_tag
                || Tag_val (fv) == Double_tag)){
          /* Do not short-circuit the pointer. */
        }else{
          final_table[i].val = fv;
          if (Is_block (final_table[i].val) && Is_in_heap (final_table[i].val)){
            goto again;
          }
        }
      }
      f = final_table[i];
      final_table[i] = final_table[--old];
      final_table[--active] = f;
      -- i;
    }
  }
  young = old;
  for (i = active; i < oldactive; i++) darken (final_table[i].val, NULL);
}

/* Call the finalisation functions for the finalising set.
   Note that this function must be reentrant.
*/
void final_do_calls (void)
{
  struct final f;
  
  Assert (active <= size);
  if (active < size){
    caml_gc_message (0x80, "Calling finalisation functions.\n", 0);
    while (active < size){
      f = final_table[active++];
      callback (f.fun, f.val);
    }
    caml_gc_message (0x80, "Done calling finalisation functions.\n", 0);
  }
}

/* Call a scanning_action [f] on [x]. */
#define Call_action(f,x) (*(f)) ((x), &(x))

/* Call [*f] on the closures of the finalisable set and
   the closures and values of the finalising set.
   The recent set is empty.
   This is called by the major GC and the compactor through [darken_all_roots].
*/
void final_do_strong_roots (scanning_action f)
{
  unsigned long i;

  Assert (old == young);
  Assert (young <= active);
  Assert (active <= size);
  for (i = 0; i < old; i++) Call_action (f, final_table[i].fun);
  for (i = active; i < size; i++){
    Call_action (f, final_table[i].fun);
    Call_action (f, final_table[i].val);
  }
}

/* Call [*f] on the values of the finalisable set.
   The recent set is empty.
   This is called directly by the compactor.
*/
void final_do_weak_roots (scanning_action f)
{
  unsigned long i;

  Assert (old == young);
  for (i = 0; i < old; i++) Call_action (f, final_table[i].val);
}

/* Call [*f] on the closures and values of the recent set.
   This is called by the minor GC through [oldify_local_roots].
*/
void final_do_young_roots (scanning_action f)
{
  unsigned long i;
  
  Assert (old <= young);
  for (i = old; i < young; i++){
    Call_action (f, final_table[i].fun);
    Call_action (f, final_table[i].val);
  }
}

/* Empty the recent set into the finalisable set.
   This is called at the end of each minor collection.
   The minor heap must be empty when this is called.
*/
void final_empty_young (void)
{
  old = young;
}

/* Put (f,v) in the recent set. */
CAMLprim value final_register (value f, value v)
{
  if (!(Is_block (v) && (Is_in_heap (v) || Is_young (v)))){
    invalid_argument ("Gc.finalise");
  }

  Assert (old <= young);
  Assert (young <= active);
  Assert (active <= size);
  
  if (young >= active){
    if (final_table == NULL){
      unsigned long new_size = 30;
      final_table = stat_alloc (new_size * sizeof (struct final));
      Assert (old == 0);
      Assert (young == 0);
      active = size = new_size;
    }else{
      unsigned long new_size = size * 2;
      unsigned long i;
      final_table = stat_resize (final_table, new_size * sizeof (struct final));
      for (i = size-1; i >= active; i--){
        final_table[i + new_size - size] = final_table[i];
      }
      active += new_size - size;
      size = new_size;
    }
  }
  Assert (young < active);
  final_table[young].fun = f;
  if (Tag_val (v) == Infix_tag) v -= Infix_offset_val (v);
  final_table[young].val = v;
  ++ young;

  return Val_unit;
}
