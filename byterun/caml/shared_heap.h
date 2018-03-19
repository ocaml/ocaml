#ifndef CAML_SHARED_HEAP_H
#define CAML_SHARED_HEAP_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "roots.h"
#include "domain.h"
#include "misc.h"

struct caml_heap_state;
struct pool;

struct caml_heap_state* caml_init_shared_heap();
void caml_teardown_shared_heap(struct caml_heap_state* heap);

value* caml_shared_try_alloc(struct caml_heap_state*, mlsize_t wosize, tag_t tag, int is_pinned);

void caml_sample_heap_stats(struct caml_heap_state*, struct heap_stats*);

uintnat caml_heap_size(struct caml_heap_state*);

struct pool* caml_pool_of_shared_block(value v);
struct domain* caml_owner_of_shared_block(value v);

void caml_shared_unpin(value v);

/* always readable by all threads
   written only by a single thread during STW periods */
typedef uintnat status;
struct global_heap_state {
  status MARKED, UNMARKED, GARBAGE;
};
extern struct global_heap_state global;

/* CR mshinwell: ensure this matches [Emitaux] */
enum {NOT_MARKABLE = 3 << 8};

static inline int Has_status_hd(header_t hd, status s) {
  return (hd & (3 << 8)) == s;
}

static inline header_t With_status_hd(header_t hd, status s) {
  return (hd & ~(3 << 8)) | s;
}

static inline int is_garbage(value v) {
  return Has_status_hd(Hd_val(v), global.GARBAGE);
}


void caml_redarken_pool(struct pool*, scanning_action, void*);

intnat caml_sweep(struct caml_heap_state*, intnat);


/* must be called during STW */
void caml_cycle_heap_stw(void);

/* must be called on each domain
   (after caml_cycle_heap_stw) */
void caml_cycle_heap(struct caml_heap_state*);

/* Heap invariant verification (for debugging) */

/* caml_verify_begin must only be called while all domains are paused */
struct heap_verify_state* caml_verify_begin();
void caml_verify_root(void*, value, value*);
void caml_verify_heap(struct heap_verify_state*); /* deallocates arg */

#ifdef DEBUG
/* [is_garbage(v)] returns true if [v] is a garbage value */
int is_garbage (value);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_SHARED_HEAP_H */
