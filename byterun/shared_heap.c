#include "plat_threads.h"
#include "mlvalues.h"
#include "gc.h"
#include "fail.h"
#include "memory.h"
#include "sizeclasses.h"

typedef unsigned int sizeclass;
typedef uintnat status;

/* always readable by all threads
   written only by a single thread during STW periods */
struct global_heap_state {
  status MARKED, UNMARKED, GARBAGE, NOT_MARKABLE;
};
struct global_heap_state global = {0 << 8, 1 << 8, 2 << 8, 3 << 8};

/* readable and writable only using explicit atomic operations
   from plat_threads.h */
struct {
  shared_stack unswept_regions;
  shared_stack swept_regions;
} global_atomic = { SHARED_STACK_INIT, SHARED_STACK_INIT };

static int Has_status_hd(header_t hd, status s) {
  return (hd & (3 << 8)) == s;
}

static header_t With_status_hd(header_t hd, status s) {
  return (hd & ~(3 << 8)) | s;
}


typedef struct region {
  shared_stack_node node;

  struct region* next_pool;
  sizeclass sz;
  value* next_obj;
  /* FIXME alignment */


  mlsize_t region_size;
} region;


/* readable and writable only by the current thread */
struct local_state {
  region* free_pools;
  region* swept_pools[NUM_SIZECLASSES];
  region* unswept_pools[NUM_SIZECLASSES];

  sizeclass next_to_sweep;
  int fully_swept_global;
};

__thread struct local_state* local;


void caml_init_shared_heap() {
  int i;
  local = caml_stat_alloc(sizeof(struct local_state));
  local->free_pools = 0;
  for (i = 0; i<NUM_SIZECLASSES; i++) {
    local->swept_pools[i] = 0;
    local->unswept_pools[i] = 0;
  }
  local->next_to_sweep = 0;
  local->fully_swept_global = 0;
}

region* region_alloc(mlsize_t sz) {
  region* r = malloc(sizeof(region) + sz);
  r->region_size = sz;

  shared_stack_push(&global_atomic.swept_regions, &r->node);
  return r;
}

void* region_start(region* r) {
  return (void*)(r + 1);
}

void* region_end(region* r) {
  return ((char*)region_start(r)) + r->region_size;
}

void pool_new(sizeclass sz) {
  region* a;
  if (local->free_pools) {
    a = local->free_pools;
    local->free_pools = a->next_pool;
  } else {
    a = region_alloc(Bsize_wsize(POOL_WSIZE));
    if (!a) return;
  }
  a->sz = sz;
  a->next_obj = 0;
  mlsize_t wh = wsize_sizeclass[sz];

  value* p = region_start(a);
  value* end = region_end(a);

  while (p + wh <= end) {
    p[0] = (value)Make_header(0, 0, global.NOT_MARKABLE);
    p[1] = (value)a->next_obj;
    a->next_obj = p;
    p += wh;
  }
  
  a->next_pool = local->swept_pools[sz];
  local->swept_pools[sz] = a;
}

void pool_sweep(sizeclass sz) {
  region* a = local->unswept_pools[sz];
  if (!a) return;
  local->unswept_pools[sz] = a->next_pool;
  value* p = (void*)(a + 1);
  value* end = (void*)((uintnat*)a + POOL_WSIZE);
  mlsize_t wh = wsize_sizeclass[sz];
  int all_free = 1;
  
  while (p + wh <= end) {
    header_t hd = (header_t)*p;
    if (Has_status_hd(hd, global.GARBAGE)) {
      p[0] = (value)Make_header(0, 0, global.NOT_MARKABLE);
      p[1] = (value)a->next_obj;
      a->next_obj = p;
    } else if (Has_status_hd(hd, global.NOT_MARKABLE)) {
      /* already on freelist, ignore */
    } else {
      all_free = 0;
    }
    p += wh;
  }

  /* FIXME: consider returning some of these to central pool */
  if (all_free) {
    a->next_pool = local->free_pools;
    local->free_pools = a;
  } else {
    a->next_pool = local->swept_pools[sz];
    local->swept_pools[sz] = a;
  }
}

void* pool_alloc(sizeclass sz) {
  region* a = local->swept_pools[sz];
  if (!a) { pool_sweep(sz); a = local->swept_pools[sz]; }
  if (!a) { pool_new(sz); a = local->swept_pools[sz]; }
  if (!a) { return 0; }
  
  value* p = a->next_obj;
  value* next = (value*)p[1];
  if (next) {
    a->next_obj = next;
  } else {
    local->swept_pools[sz] = a->next_pool;
  }
  return p;
}


value* caml_shared_try_alloc(mlsize_t wosize, tag_t tag) {
  mlsize_t whsize = Whsize_wosize(wosize);
  value* p;
  Assert (wosize > 0);
  if (whsize <= SIZECLASS_MAX) {
    p = pool_alloc(sizeclass_wsize[whsize]);
  } else {
    p = region_start(region_alloc(Bsize_wsize(whsize)));
  }
  if (!p) return 0;
  Hd_hp (p) = Make_header(wosize, tag, global.UNMARKED);
#ifdef DEBUG
  {
    int i;
    for (i = 0; i < wosize; i++) {
      Field(Val_hp(p), i) = Debug_free_major;
    }
  }
#endif
  return p;
}

int caml_sweep(int work) {
  /* Sweep local pools */
  while (work > 0 && local->next_to_sweep < NUM_SIZECLASSES) {
    if (local->unswept_pools[local->next_to_sweep]) {
      pool_sweep(local->next_to_sweep);
      work--;
    } else {
      local->next_to_sweep++;
    }
  }

  /* Sweep global regions */
  while (work > 0 && !local->fully_swept_global) {
    region* r = shared_stack_pop(&global_atomic.unswept_regions);
    if (r) {
      /* sweep the region somehow */
      shared_stack_push(&global_atomic.swept_regions, &r->node);
    } else {
      local->fully_swept_global = 1;
    }
  }
  return work;
}




int caml_mark_object(value p) {
  Assert (Is_block(p));
  header_t h = Hd_val(p);
  /* An object should have one of these statuses:
       - UNMARKED:     this object has not yet been traced
       - MARKED:       this object has already been traced or is being traced
       - NOT_MARKABLE: this object should be ignored by the GC */
  Assert (!Has_status_hd(h, global.GARBAGE));
  if (Has_status_hd(h, global.UNMARKED)) {
    Hd_val(p) = With_status_hd(h, global.MARKED);
    return 1;
  } else {
    return 0;
  }
}

header_t atoms[256];

CAMLexport value caml_atom(tag_t tag) {
  return Val_hp(&atoms[tag]);
}

void caml_init_major_heap (asize_t size) {
  int i;
  for (i=0; i<256; i++) atoms[i] = Make_header(0, i, global.NOT_MARKABLE);
}

void caml_cycle_heap() {
  struct global_heap_state oldg = global;
  struct global_heap_state newg;
  newg.UNMARKED     = oldg.MARKED;
  newg.GARBAGE      = oldg.UNMARKED;
  newg.MARKED       = oldg.GARBAGE; /* should be empty because garbage was swept */
  newg.NOT_MARKABLE = oldg.NOT_MARKABLE;
  global = newg;
}
