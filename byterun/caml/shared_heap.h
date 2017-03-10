#ifndef CAML_SHARED_HEAP_H
#define CAML_SHARED_HEAP_H

#include "config.h"
#include "roots.h"
#include "domain.h"
#include "misc.h"

struct caml_heap_state;
struct pool;

struct caml_heap_state* caml_init_shared_heap();

value* caml_shared_try_alloc(struct caml_heap_state*, mlsize_t wosize, tag_t tag, int is_pinned);

void caml_sample_heap_stats(struct caml_heap_state*, struct heap_stats*);

uintnat caml_heap_size(struct caml_heap_state*);

struct pool* caml_pool_of_shared_block(value v);
struct domain* caml_owner_of_shared_block(value v);

void caml_shared_unpin(value v);

int caml_mark_object(value);

void caml_redarken_pool(struct pool*, scanning_action);

intnat caml_sweep(struct caml_heap_state*, intnat);


/* must be called during STW */
void caml_cycle_heap_stw(void);

/* must be called on each domain
   (after caml_cycle_heap_stw) */
void caml_cycle_heap(struct caml_heap_state*);

#ifdef CAML_VERIFY_HEAP
/* must only be called while all domains are paused */
void caml_verify_root(value, value*);
void caml_verify_heap(void);
#endif

#ifdef DEBUG
/* [is_garbage(v)] returns true if [v] is a garbage value */
int is_garbage (value);

#endif

#endif /* CAML_SHARED_HEAP_H */
