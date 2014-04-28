#ifndef CAML_MINOR_HEAP_H
#define CAML_MINOR_HEAP_H

#include "mlvalues.h"

CAMLextern __thread char *caml_young_end, *caml_young_start;

void caml_init_minor_heaps();

void caml_init_young_ptrs();
asize_t caml_norm_minor_heap_size (intnat);
void caml_allocate_minor_heap(asize_t);
void caml_free_minor_heap ();



/* All values which are not blocks in the current domain's minor heap
   differ from caml_young_start in at least one of the bits set in
   Young_val_bitmask */
#define Young_val_bitmask \
  ((uintnat)1 | ~(((uintnat)1 << Minor_heap_align_bits) - (uintnat)1))

/* All values which are not blocks in any domain's minor heap differ
   from caml_young_start in at least one of the bits set in
   Minor_val_bitmask */
#define Minor_val_bitmask \
  ((uintnat)1 | ~(((uintnat)1 << (Minor_heap_align_bits + Minor_heap_sel_bits)) - (uintnat)1))


/* Is_young(val) is true iff val is a block in the current domain's minor heap.
   Since the minor heap is allocated in one aligned block, this can be tested
   via bitmasking. */
#define Is_young(val) \
  ((((uintnat)(val) ^ (uintnat)caml_young_start) & Young_val_bitmask) == 0)

/* Is_minor(val) is true iff val is a block in any domain's minor heap. */
#define Is_minor(val) \
  ((((uintnat)(val) ^ (uintnat)caml_young_start) & Minor_val_bitmask) == 0)

/* Is_foreign(val) is true iff val is a block in another domain's minor heap.
   Since all minor heaps lie in one aligned blick, this can be tested via
   more bitmasking. */
#define Is_foreign(val) \
  (((((uintnat)(val) ^ (uintnat)caml_young_start) - (1 << Minor_heap_align_bits)) & \
    Minor_val_bitmask) == 0)

#endif /* CAML_MINOR_HEAP_H */
