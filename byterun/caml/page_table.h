/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_PAGETABLE_H
#define CAML_PAGETABLE_H

#define Not_in_heap 0
#define In_heap 1
#define In_young 2
#define In_static_data 4
#define In_code_area 8

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

#define Is_in_value_area(a) \
  (Classify_addr(a) & (In_heap | In_young | In_static_data))
#define Is_in_heap(a) (Classify_addr(a) & In_heap)
#define Is_in_heap_or_young(a) (Classify_addr(a) & (In_heap | In_young))

int caml_page_table_add(int kind, void * start, void * end);
int caml_page_table_remove(int kind, void * start, void * end);
int caml_page_table_initialize(mlsize_t bytesize);

#endif
