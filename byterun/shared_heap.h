#ifndef CAML_SHARED_HEAP_H
#define CAML_SHARED_HEAP_H

value* caml_shared_try_alloc(mlsize_t wosize, tag_t tag, int is_pinned);


struct caml_heap_state;
extern __thread struct caml_heap_state* caml_shared_heap;
value* caml_shared_try_alloc_remote(struct caml_heap_state*, mlsize_t wosize, tag_t tag, int is_pinned);


void caml_init_shared_heap();

struct domain* caml_owner_of_shared_block(value v);

void caml_shared_unpin(value v);

int caml_mark_object(value);

int caml_sweep(int);


/* must be called during STW */
void caml_cycle_heap_stw(void);

/* must be called on each domain 
   (after caml_cycle_heap_stw) */
void caml_cycle_heap(void);

#endif /* CAML_SHARED_HEAP_H */
