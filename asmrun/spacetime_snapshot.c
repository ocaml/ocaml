/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Mark Shinwell and Leo White, Jane Street Europe             */
/*                                                                        */
/*   Copyright 2013--2016, Jane Street Group, LLC                         */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <sys/resource.h>

#include "caml/alloc.h"
#include "caml/backtrace_prim.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "spacetime.h"
#include "stack.h"

#ifdef WITH_SPACETIME

/* The following structures must match the type definitions in the
   [Spacetime] module. */

typedef struct {
  /* (GC header here.) */
  value minor_words;
  value promoted_words;
  value major_words;
  value minor_collections;
  value major_collections;
  value heap_words;
  value heap_chunks;
  value compactions;
  value top_heap_words;
} gc_stats;

typedef struct {
  value profinfo;
  value num_blocks;
  value num_words_including_headers;
} snapshot_entry;

typedef struct {
  /* (GC header here.) */
  snapshot_entry entries[0];
} snapshot_entries;

typedef struct {
  /* (GC header here.) */
  value time;  /* Cf. [Sys.time]. */
  value gc_stats;
  value entries;
  value num_blocks_in_minor_heap;
  value num_blocks_in_major_heap;
  value num_blocks_in_minor_heap_with_profinfo;
  value num_blocks_in_major_heap_with_profinfo;
} snapshot;

typedef struct {
  uintnat num_blocks;
  uintnat num_words_including_headers;
} raw_snapshot_entry;

static value allocate_outside_heap_with_tag(mlsize_t size_in_bytes, tag_t tag)
{
  /* CR mshinwell: this function should live somewhere else */
  header_t* block;

  assert(size_in_bytes % sizeof(value) == 0);
  block = caml_stat_alloc(sizeof(header_t) + size_in_bytes);
  *block = Make_header(size_in_bytes / sizeof(value), tag, Caml_black);
  return (value) &block[1];
}

static value allocate_outside_heap(mlsize_t size_in_bytes)
{
  assert(size_in_bytes > 0);
  return allocate_outside_heap_with_tag(size_in_bytes, 0);
}

static value take_gc_stats(void)
{
  value v_stats;
  gc_stats* stats;

  v_stats = allocate_outside_heap(sizeof(gc_stats));
  stats = (gc_stats*) v_stats;

  stats->minor_words = Val_long(caml_stat_minor_words);
  stats->promoted_words = Val_long(caml_stat_promoted_words);
  stats->major_words =
    Val_long(((uintnat) caml_stat_major_words)
             + ((uintnat) caml_allocated_words));
  stats->minor_collections = Val_long(caml_stat_minor_collections);
  stats->major_collections = Val_long(caml_stat_major_collections);
  stats->heap_words = Val_long(caml_stat_heap_wsz / sizeof(value));
  stats->heap_chunks = Val_long(caml_stat_heap_chunks);
  stats->compactions = Val_long(caml_stat_compactions);
  stats->top_heap_words = Val_long(caml_stat_top_heap_wsz / sizeof(value));

  return v_stats;
}

CAMLprim value caml_spacetime_take_heap_snapshot(void)
{
  value v_snapshot;
  snapshot* heap_snapshot;
  value v_entries;
  snapshot_entries* entries;
  char* chunk;
  value gc_stats;
  uintnat index;
  uintnat target_index;
  value v_time;
  double time;
  uintnat profinfo;
  uintnat num_distinct_profinfos;
  value* ptr;
  /* Fixed size buffer to avoid needing a hash table: */
  static raw_snapshot_entry* raw_entries = NULL;
  uintnat num_blocks_in_minor_heap = 0;
  uintnat num_blocks_in_minor_heap_with_profinfo = 0;
  uintnat num_blocks_in_major_heap = 0;
  uintnat num_blocks_in_major_heap_with_profinfo = 0;

  time = caml_sys_time_unboxed(Val_unit);
  gc_stats = take_gc_stats();

  if (raw_entries == NULL) {
    size_t size = (PROFINFO_MASK + 1) * sizeof(raw_snapshot_entry);
    raw_entries = caml_stat_alloc(size);
    memset(raw_entries, '\0', size);
  }

  num_distinct_profinfos = 0;

  /* Scan the minor heap. */
  /* CR mshinwell: this is wrong, it should start from the roots */
  assert(((uintnat) caml_young_ptr) % sizeof(value) == 0);
  ptr = (value*) caml_young_ptr;
  assert(ptr >= (value*) caml_young_start);
  while (ptr < (value*) caml_young_end) {
    header_t hd;
    value value_in_minor_heap;

    ptr++;
    value_in_minor_heap = (value) ptr;
    assert(Is_young(value_in_minor_heap));
    assert(Is_block(value_in_minor_heap));

    hd = Hd_val(value_in_minor_heap);

    /* We do not expect the value to be promoted, since this function
       should not be called during a minor collection. */
    assert(hd != 0);

    profinfo = Profinfo_hd(hd);

    num_blocks_in_minor_heap++;
    if (profinfo <= PROFINFO_MASK) {
      num_blocks_in_minor_heap_with_profinfo++;
      assert (raw_entries[profinfo].num_blocks >= 0);
      if (raw_entries[profinfo].num_blocks == 0) {
        num_distinct_profinfos++;
      }
      raw_entries[profinfo].num_blocks++;
      raw_entries[profinfo].num_words_including_headers +=
        Whsize_val(value_in_minor_heap);
    }

    ptr += Wosize_val(value_in_minor_heap);
  }

  /* Scan the major heap. */
  chunk = caml_heap_start;
  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      switch (Color_hd(hd)) {
        case Caml_blue:
          break;

        default:
          profinfo = Profinfo_hd(hd);
          num_blocks_in_major_heap++;
          if (profinfo <= PROFINFO_MASK) {
            num_blocks_in_major_heap_with_profinfo++;
            assert (raw_entries[profinfo].num_blocks >= 0);
            if (raw_entries[profinfo].num_blocks == 0) {
              num_distinct_profinfos++;
            }
            raw_entries[profinfo].num_blocks++;
            raw_entries[profinfo].num_words_including_headers +=
              Whsize_hd(hd);
          }
          break;
      }
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  if (num_distinct_profinfos > 0) {
    v_entries = allocate_outside_heap(
      num_distinct_profinfos*sizeof(snapshot_entry));
    entries = (snapshot_entries*) v_entries;
    target_index = 0;
    for (index = 0; index <= PROFINFO_MASK; index++) {
      assert(raw_entries[index].num_blocks >= 0);
      if (raw_entries[index].num_blocks > 0) {
        assert(target_index < num_distinct_profinfos);
        entries->entries[target_index].profinfo = Val_long(index);
        entries->entries[target_index].num_blocks
          = Val_long(raw_entries[index].num_blocks);
        entries->entries[target_index].num_words_including_headers
          = Val_long(raw_entries[index].num_words_including_headers);
        target_index++;
      }
    }
  } else {
    v_entries = Atom(0);
  }

  assert(sizeof(double) == sizeof(value));
  v_time = allocate_outside_heap_with_tag(sizeof(double), Double_tag);
  Double_field(v_time, 0) = time;

  v_snapshot = allocate_outside_heap(sizeof(snapshot));
  heap_snapshot = (snapshot*) v_snapshot;

  heap_snapshot->time = v_time;
  heap_snapshot->gc_stats = gc_stats;
  heap_snapshot->entries = v_entries;
  heap_snapshot->num_blocks_in_minor_heap =
    Val_long(num_blocks_in_minor_heap);
  heap_snapshot->num_blocks_in_major_heap =
    Val_long(num_blocks_in_major_heap);
  heap_snapshot->num_blocks_in_minor_heap_with_profinfo
    = Val_long(num_blocks_in_minor_heap_with_profinfo);
  heap_snapshot->num_blocks_in_major_heap_with_profinfo
    = Val_long(num_blocks_in_major_heap_with_profinfo);

  return v_snapshot;
}

CAMLprim value caml_spacetime_free_heap_snapshot(value v_snapshot)
{
  snapshot* heap_snapshot = (snapshot*) v_snapshot;
  caml_stat_free(Hp_val(heap_snapshot->time));
  caml_stat_free(Hp_val(heap_snapshot->gc_stats));
  if (Wosize_val(heap_snapshot->entries) > 0) {
    caml_stat_free(Hp_val(heap_snapshot->entries));
  }
  caml_stat_free(Hp_val(v_snapshot));
  return Val_unit;
}

CAMLprim value caml_spacetime_marshal_heap_snapshot
      (value v_channel, value v_snapshot)
{
  caml_extern_allow_out_of_heap = 1;
  caml_output_value(v_channel, v_snapshot, Val_long(0));
  caml_extern_allow_out_of_heap = 0;

  return Val_unit;
}

CAMLprim value
caml_spacetime_num_frame_descriptors(value unit)
{
  assert(unit == Val_unit);

  if (caml_frame_descriptors == NULL) {
    caml_init_frame_descriptors();
  }

  return Val_long(caml_frame_descriptors_mask + 1);
}

CAMLprim value
caml_spacetime_get_frame_descriptor(value v_index)
{
  uintnat index;
  value v_result;
  frame_descr* descr;

  assert(!Is_block(v_index));
  index = Long_val(v_index);
  if (index > caml_frame_descriptors_mask) {
    caml_failwith("caml_spacetime_get_frametable: bad index");
  }

  if (caml_frame_descriptors == NULL) {
    caml_init_frame_descriptors();
  }
  
  descr = caml_frame_descriptors[index];

  if (descr == NULL) {
    return Val_long(0 /* None */);
  }

  v_result = caml_alloc_small(1, 1 /* Some */);
  Field(v_result, 0) = caml_val_raw_backtrace_slot((backtrace_slot) descr);

  return v_result;
}

CAMLprim value
caml_spacetime_return_address_of_frame_descriptor(value v_descr)
{
  frame_descr* descr;

  descr = (frame_descr*) caml_raw_backtrace_slot_val(v_descr);
  assert(descr != NULL);

  return caml_copy_int64(descr->retaddr);
}

extern struct caml_custom_operations caml_int64_ops;  /* ints.c */

static value
allocate_int64_outside_heap(uint64_t i)
{
  value v;

  v = allocate_outside_heap_with_tag(2, Custom_tag);
  Field(v, 0) = (value) &caml_int64_ops;
  Int64_val(v) = i;

  return v;
}

CAMLprim value
caml_spacetime_shape_table(value v_unit)
{
  /* Flatten the hierarchy of shape tables into a single associative list
     mapping from function symbols to node layouts.  The node layouts are
     themselves lists.

     This function reverses the order of the lists giving the layout of each
     node; however, spacetime_profiling.ml ensures they are emitted in
     reverse order, so at the end of it all they're not reversed. */

  value list = Val_long(0);  /* the empty list */
  shape_table* table = caml_spacetime_shape_tables;

  while (table != NULL) {
    uint64_t** table_for_one_unit = table->table;
    while (*table_for_one_unit != (uint64_t) 0) {
      uint64_t* table_for_one_function = *table_for_one_unit;
      while (*table_for_one_function != (uint64_t) 0) {
        value new_list_element, pair, function_address, layout;

        function_address =
          allocate_int64_outside_heap(*table_for_one_function);

        layout = Val_long(0);  /* the empty list */
        while (*table_for_one_function != (uint64_t) 0) {
          int tag;
          int has_argument;
          value part_of_shape;
          value new_part_list_element;

          /* CR mshinwell: share with emit.mlp */
          switch (*table_for_one_function) {
            case 1:  /* caml_call_gc */
              tag = 0;
              has_argument = 0;
              break;

            case 2:  /* bounds check failure */
              tag = 1;
              has_argument = 0;
              break;

            case 3:  /* direct call to given location */
              tag = 0;
              has_argument = 1;
              break;

            case 4:  /* indirect call to given location */
              tag = 1;
              has_argument = 1;
              break;

            case 5:  /* allocation at given location */
              tag = 2;
              has_argument = 1;
              break;

            default:
              assert(0);
          }

          table_for_one_function++;

          if (!has_argument) {
            part_of_shape = Val_long(tag);
          }
          else {
            value location;

            location = allocate_int64_outside_heap(*table_for_one_function);
            table_for_one_function++;

            part_of_shape = allocate_outside_heap_with_tag(1, tag);
            Field(part_of_shape, 0) = location;
          }

          new_part_list_element =
            allocate_outside_heap_with_tag(2, 0 /* (::) */);
          Field(new_part_list_element, 0) = part_of_shape;
          Field(new_part_list_element, 1) = layout;
          layout = new_part_list_element;
        }

        pair = allocate_outside_heap_with_tag(2, 0);
        Field(pair, 0) = function_address;
        Field(pair, 1) = layout;

        new_list_element = allocate_outside_heap_with_tag(2, 0 /* (::) */);
        Field(new_list_element, 0) = pair;
        Field(new_list_element, 1) = list;
        list = new_list_element;

        Assert(*table_for_one_function == 0);
        table_for_one_function++;
      }
      table_for_one_unit++;
    }
    table = table->next;
  }

  return list;
}

#else

static void spacetime_disabled()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value caml_spacetime_take_heap_snapshot()
{
  spacetime_disabled();
}

CAMLprim value caml_spacetime_marshal_heap_snapshot()
{
  spacetime_disabled();
}

CAMLprim value caml_spacetime_free_heap_snapshot()
{
  spacetime_disabled();
}

CAMLprim value caml_spacetime_num_frame_descriptors ()
{
  spacetime_disabled();
}

CAMLprim value caml_spacetime_get_frame_descriptor ()
{
  spacetime_disabled();
}

CAMLprim value caml_spacetime_return_address_of_frame_descriptor ()
{
  spacetime_disabled();
}

#endif
