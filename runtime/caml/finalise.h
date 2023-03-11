/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_FINALISE_H
#define CAML_FINALISE_H

#ifdef CAML_INTERNALS

#include "roots.h"
#include "domain.h"

struct final {
  value fun;
  value val;
  int offset;
};

struct finalisable {
  struct final *table;
  uintnat old;
  uintnat young;
  uintnat size;
};
/* [0..old) : finalisable set, the values are in the major heap
   [old..young) : recent set, the values could be in the minor heap
   [young..size) : free space

   The element of the finalisable set are moved to the finalising set
   below when the value are unreachable (for the first or last time).

*/

struct final_todo {
  struct final_todo *next;
  int size;
  struct final item[1];  /* variable size */
};

/*
  todo_head: head of the list of finalisation functions that can be run.
  todo_tail: tail of the list of finalisation functions that can be run.

  It is the finalising set.
*/

struct caml_final_info {
  struct finalisable first;
  uintnat updated_first;
  struct finalisable last;
  uintnat updated_last;
  struct final_todo *todo_head;
  struct final_todo *todo_tail;
  uintnat running_finalisation_function;
  struct caml_final_info* next; /* used for orphaned finalisers.
                                   See major_gc.c */
};

void caml_final_merge_finalisable (struct finalisable *source,
                                   struct finalisable *target);
int caml_final_update_first (caml_domain_state* d);
int caml_final_update_last (caml_domain_state* d);
value caml_final_do_calls_exn (void);
void caml_final_do_roots (
  scanning_action f, scanning_action_flags fflags, void* fdata,
  caml_domain_state* domain, int do_val);
void caml_final_do_young_roots (
  scanning_action f, scanning_action_flags fflags, void* fdata,
  caml_domain_state* d, int do_last_val);
void caml_final_empty_young (caml_domain_state* d);
void caml_final_update_last_minor (caml_domain_state* d);
struct caml_final_info* caml_alloc_final_info(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_FINALISE_H */
