#ifndef CAML_SHARED_HEAP_H
#define CAML_SHARED_HEAP_H

struct caml_heap_state;


struct caml_heap_state* caml_init_shared_heap();

value* caml_shared_try_alloc(struct caml_heap_state*, mlsize_t wosize, tag_t tag, int is_pinned);

uintnat caml_heap_size(struct caml_heap_state*);

struct domain* caml_owner_of_shared_block(value v);

void caml_shared_unpin(value v);

int caml_mark_object(value);

intnat caml_sweep(struct caml_heap_state*, intnat);


/* must be called during STW */
void caml_cycle_heap_stw(void);

/* must be called on each domain
   (after caml_cycle_heap_stw) */
void caml_cycle_heap(struct caml_heap_state*);

#ifdef DEBUG

/* [is_garbage(v)] returns true if [v] is a garbage value */
int is_garbage (value);

#endif

#endif /* CAML_SHARED_HEAP_H */
