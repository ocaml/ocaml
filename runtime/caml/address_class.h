/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Classification of addresses for GC and runtime purposes. */

/* The current runtime supports two different configurations that
   correspond to two different value models, depending on whether
   "naked pointers", that do not point to a well-formed OCaml block,
   are allowed (considered valid values).

   In "classic mode", naked pointers are allowed, and the
   implementation uses a page table. A valid value is then either:
   - a tagged integer (Is_long or !Is_block from mlvalues.h)
   - a pointer to the minor heap (Is_young)
   - a pointer to the major heap (Is_in_heap)
   - a pointer to a constant block statically-allocated by OCaml code
     or the OCaml runtime (Is_in_static_data)
   - a "foreign" pointer, which is none of the above; the destination
     of those pointers may be a well-formed OCaml blocks, but it may
     also be a naked pointer.

   The macros and functions below give access to a global page table
   to classify addresses to be able to implement Is_in_heap,
   In_static_data (or their disjunction Is_in_value_area) and thus
   detect values which may be naked pointers. The runtime
   conservatively assumes that all foreign pointers may be naked
   pointers, and uses the page table to not dereference/follow them.

   In "no naked pointers" mode (when NO_NAKED_POINTERS is defined),
   naked pointers are illegal, so pointers that are values can always
   be assumed to point to well-formed blocks.

   To support an implementation without a global page table, runtime
   code should not rely on Is_in_heap and Is_in_static_data. This
   corresponds to a simpler model where a valid value is either:
   - a tagged integer (Is_long)
   - a pointer to the minor heap (Is_young)
   - a pointer to a well-formed block outside the minor heap
     (it may be in the major heap, or static, or a foreign pointer,
      without a check to distinguish the various cases).

   (To create a well-formed block outside the heap that the GC will
   not scan, one can use the Caml_out_of_heap_header from mlvalues.h.)
*/

#ifndef CAML_ADDRESS_CLASS_H
#define CAML_ADDRESS_CLASS_H

#include "config.h"
#include "misc.h"
#include "mlvalues.h"

/* Use the following macros to test an address for the different classes
   it might belong to. */

#define Is_young(val) \
  (CAMLassert (Is_block (val)), \
   (char *)(val) < (char *)Caml_state_field(young_end) && \
   (char *)(val) > (char *)Caml_state_field(young_start))

#define Is_in_heap(a) (Classify_addr(a) & In_heap)

#ifdef NO_NAKED_POINTERS

#define Is_in_heap_or_young(a) 1
#define Is_in_value_area(a) 1

#else

#define Is_in_heap_or_young(a) (Classify_addr(a) & (In_heap | In_young))

#define Is_in_value_area(a)                                     \
  (Classify_addr(a) & (In_heap | In_young | In_static_data))

#define Is_in_static_data(a) (Classify_addr(a) & In_static_data)

#endif

/***********************************************************************/
/* The rest of this file is private and may change without notice. */

#define Not_in_heap 0
#define In_heap 1
#define In_young 2
#define In_static_data 4

#ifdef ARCH_SIXTYFOUR

/* 64 bits: Represent page table as a sparse hash table */
int caml_page_table_lookup(void * addr);
#define Classify_addr(a) (caml_page_table_lookup((void *)(a)))

#else

/* 32 bits: Represent page table as a 2-level array */
#define Pagetable2_log 11
#define Pagetable2_size (1 << Pagetable2_log)
#define Pagetable1_log (Page_log + Pagetable2_log)
#define Pagetable1_size (1 << (32 - Pagetable1_log))
CAMLextern unsigned char * caml_page_table[Pagetable1_size];

#define Pagetable_index1(a) (((uintnat)(a)) >> Pagetable1_log)
#define Pagetable_index2(a) \
  ((((uintnat)(a)) >> Page_log) & (Pagetable2_size - 1))
#define Classify_addr(a) \
  caml_page_table[Pagetable_index1(a)][Pagetable_index2(a)]

#endif

int caml_page_table_add(int kind, void * start, void * end);
int caml_page_table_remove(int kind, void * start, void * end);
int caml_page_table_initialize(mlsize_t bytesize);

#endif /* CAML_ADDRESS_CLASS_H */
