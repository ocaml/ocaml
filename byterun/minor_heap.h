#ifndef CAML_MINOR_HEAP_H
#define CAML_MINOR_HEAP_H

#include "mlvalues.h"

void caml_init_minor_heaps();

void caml_init_young_ptrs();
asize_t caml_norm_minor_heap_size (intnat);
void caml_allocate_minor_heap(asize_t);
void caml_free_minor_heap ();

struct domain;
struct domain* caml_owner_of_young_block(value);

#endif /* CAML_MINOR_HEAP_H */
