#include <stdlib.h>
#include <string.h>
#include "platform.h"
#include "mlvalues.h"
#include "gc.h"
#include "fail.h"
#include "memory.h"
#include "sizeclasses.h"
#include "addrmap.h"
#include "roots.h"
#include "globroots.h"

typedef unsigned int sizeclass;
typedef uintnat status;

/* always readable by all threads
   written only by a single thread during STW periods */
struct global_heap_state {
  status MARKED, UNMARKED, GARBAGE;
};
struct global_heap_state global = {0 << 8, 1 << 8, 2 << 8};
enum {NOT_MARKABLE = 3 << 8};


static int Has_status_hd(header_t hd, status s) {
  return (hd & (3 << 8)) == s;
}

static header_t With_status_hd(header_t hd, status s) {
  return (hd & ~(3 << 8)) | s;
}


typedef struct pool {
  struct pool* next;
  value* next_obj;
  struct domain* owner;
} pool;

typedef struct large_alloc {
  struct domain* owner;
  struct large_alloc* next;
} large_alloc;


#define ALIGN_SIZEOF_VALUE(n) (((n + sizeof(value) - 1) / sizeof(value)) * sizeof(value))

/* sizeof(pool) rounded up to sizeof(value) */
#define POOL_HEADER_SZ ALIGN_SIZEOF_VALUE(sizeof(pool))
#define LARGE_ALLOC_HEADER_SZ ALIGN_SIZEOF_VALUE(sizeof(large_alloc))

struct {
  caml_plat_mutex lock;
  pool* free;
} pool_freelist;


/* readable and writable only by the current thread */
struct caml_heap_state {
  pool* free_pools;
  int num_free_pools;

  pool* avail_pools[NUM_SIZECLASSES];
  pool* full_pools[NUM_SIZECLASSES];
  pool* unswept_avail_pools[NUM_SIZECLASSES];
  pool* unswept_full_pools[NUM_SIZECLASSES];

  large_alloc* swept_large;
  large_alloc* unswept_large;

  sizeclass next_to_sweep;

  struct domain* owner;
};

__thread struct caml_heap_state* caml_shared_heap;

void caml_init_shared_heap(int is_main) {
  int i;
  if (caml_domain_is_main(caml_domain_self())) {
    caml_plat_mutex_init(&pool_freelist.lock);
  }

  Assert(NOT_MARKABLE == Promotedhd_hd(0));

  caml_shared_heap = caml_stat_alloc(sizeof(struct caml_heap_state));
  caml_shared_heap->free_pools = 0;
  caml_shared_heap->num_free_pools = 0;
  for (i = 0; i<NUM_SIZECLASSES; i++) {
    caml_shared_heap->avail_pools[i] = caml_shared_heap->full_pools[i] =
      caml_shared_heap->unswept_avail_pools[i] = caml_shared_heap->unswept_full_pools[i] = 0;
  }
  caml_shared_heap->next_to_sweep = 0;
  caml_shared_heap->swept_large = 0;
  caml_shared_heap->unswept_large = 0;
  caml_shared_heap->owner = caml_domain_self();
}


/* Allocating and deallocating pools from the global freelist.
   Up to MAX_LOCAL_FREE_POOLS are cached locally */

#define POOLS_PER_ALLOCATION 16
#define MAX_LOCAL_FREE_POOLS 5
static pool* pool_acquire(struct caml_heap_state* local) {
  pool* r;

  if (local->num_free_pools > 0) {
    r = local->free_pools;
    local->free_pools = r->next;
    local->num_free_pools--;
  } else {
    caml_plat_lock(&pool_freelist.lock);
    if (!pool_freelist.free) {
      void* mem = caml_mem_map(Bsize_wsize(POOL_WSIZE) * POOLS_PER_ALLOCATION, 
                               Bsize_wsize(POOL_WSIZE), 0 /* allocate */);
      int i;
      if (mem) {
        pool_freelist.free = mem;
        for (i=1; i<POOLS_PER_ALLOCATION; i++) {
          r = (pool*)(((uintnat)mem) + ((uintnat)i) * Bsize_wsize(POOL_WSIZE));
          r->next = pool_freelist.free;
          pool_freelist.free = r;
        }
      }
    }
    r = pool_freelist.free;
    if (r)
      pool_freelist.free = r->next;
    caml_plat_unlock(&pool_freelist.lock);
  }
  return r;
}

static void pool_release(struct caml_heap_state* local, pool* pool) {
  if (local->num_free_pools < MAX_LOCAL_FREE_POOLS) {
    local->num_free_pools++;
    pool->next = local->free_pools;
    local->free_pools = pool;
  } else {
    caml_plat_lock(&pool_freelist.lock);
    pool->next = pool_freelist.free;
    pool_freelist.free = pool;
    caml_plat_unlock(&pool_freelist.lock);
  }
}


/* Allocating an object from a pool */

static int pool_sweep(struct caml_heap_state* local, pool**, sizeclass sz);
static pool* pool_find(struct caml_heap_state* local, sizeclass sz) {
  pool* r;

  /* Hopefully we have a pool we can use directly */
  r = local->avail_pools[sz];
  if (r) return r;

  /* Otherwise, try to sweep until we find one */
  while (!local->avail_pools[sz] && 
         pool_sweep(local, &local->unswept_avail_pools[sz], sz));
  while (!local->avail_pools[sz] &&
         pool_sweep(local, &local->unswept_full_pools[sz], sz));
  r = local->avail_pools[sz];
  if (r) return r;

  /* Failing that, we need to allocate a new pool */
  if (local->free_pools) {
    r = local->free_pools;
    local->free_pools = r->next;
  } else {
    r = pool_acquire(local);
    if (!r) return 0; /* if we can't allocate, give up */
  }

  /* Having allocated a new pool, set it up for size sz */
  local->avail_pools[sz] = r;
  r->next = 0;
  r->owner = local->owner;
  mlsize_t wh = wsize_sizeclass[sz];
  value* p = (value*)((char*)r + POOL_HEADER_SZ);
  value* end = (value*)((char*)r + Bsize_wsize(POOL_WSIZE));

  while (p + wh <= end) {
    p[0] = 0; /* zero header indicates free object */
    p[1] = (value)r->next_obj;
    r->next_obj = p;
    p += wh;
  }

  return r;
}

static void* pool_allocate(struct caml_heap_state* local, sizeclass sz) {
  pool* r = pool_find(local, sz);

  if (!r) return 0;

  value* p = r->next_obj;
  value* next = (value*)p[1];
  r->next_obj = next;
  Assert(p[0] == 0);
  if (!next) {
    local->avail_pools[sz] = r->next;
    r->next = local->full_pools[sz];
    local->full_pools[sz] = r;
  }
  return p;
}

static void* large_allocate(struct caml_heap_state* local, mlsize_t sz) {
  large_alloc* a = malloc(sz + LARGE_ALLOC_HEADER_SZ);
  if (!a) caml_raise_out_of_memory();
  a->owner = local->owner;
  a->next = local->swept_large;
  local->swept_large = a;
  return (char*)a + LARGE_ALLOC_HEADER_SZ;
}

value* caml_shared_try_alloc_remote(struct caml_heap_state* local, mlsize_t wosize, tag_t tag, int pinned) {
  mlsize_t whsize = Whsize_wosize(wosize);
  value* p;
  Assert (wosize > 0);
  Assert (tag != Infix_tag);
  if (whsize <= SIZECLASS_MAX) {
    p = pool_allocate(local, sizeclass_wsize[whsize]);
  } else {
    p = large_allocate(local, Bsize_wsize(whsize));
  }
  if (!p) return 0;
  Hd_hp (p) = Make_header(wosize, tag, pinned ? NOT_MARKABLE : global.UNMARKED);
#ifdef DEBUG
  {
    int i;
    for (i = 0; i < wosize; i++) {
      Op_val(Val_hp(p))[i] = Debug_free_major;
    }
  }
#endif
  return p;
}

value* caml_shared_try_alloc(mlsize_t wosize, tag_t tag, int pinned) {
  return caml_shared_try_alloc_remote(caml_shared_heap, wosize, tag, pinned);
}

struct domain* caml_owner_of_shared_block(value v) {
  Assert (Is_block(v) && !Is_minor(v));
  mlsize_t whsize = Whsize_wosize(Wosize_val(v));
  Assert (whsize > 0); /* not an atom */
  if (whsize <= SIZECLASS_MAX) {
    /* FIXME: ORD: if we see the object, we must see the owner */
    pool* p = (pool*)((uintnat)v &~(POOL_WSIZE * sizeof(value) - 1));
    return p->owner;
  } else {
    large_alloc* a = (large_alloc*)((char*)v - LARGE_ALLOC_HEADER_SZ);
    return a->owner;
  }
}

void caml_shared_unpin(value v) {
  Assert (Is_block(v) && !Is_minor(v));
  Assert (caml_owner_of_shared_block(v) == caml_domain_self());
  Assert (Has_status_hd(Hd_val(v), NOT_MARKABLE));
  Hd_val(v) = With_status_hd(Hd_val(v), global.UNMARKED);
}

/* Sweeping */

static int pool_sweep(struct caml_heap_state* local, pool** plist, sizeclass sz) {
  pool* a = *plist;
  if (!a) return 0;
  *plist = a->next;

  value* p = (value*)((char*)a + POOL_HEADER_SZ);
  value* end = (value*)a + POOL_WSIZE;
  mlsize_t wh = wsize_sizeclass[sz];
  int all_free = 1, all_used = 1;
  
  while (p + wh <= end) {
    header_t hd = (header_t)*p;
    if (hd == 0) {
      /* already on freelist */
      all_used = 0;
    } else if (Has_status_hd(hd, global.GARBAGE)) {
      /* add to freelist */
      p[0] = 0;
      p[1] = (value)a->next_obj;
      Assert(Is_block((value)p));
      a->next_obj = p;
      all_used = 0;
    } else {
      /* still live */
      all_free = 0;
    }
    p += wh;
  }

  if (all_free) {
    pool_release(local, a);
  } else {
    pool** list = all_used ? &local->full_pools[sz] : &local->avail_pools[sz];
    a->next = *list;
    *list = a;
  }

  return 1;
}

static void large_alloc_sweep(struct caml_heap_state* local) {
  large_alloc* a = local->unswept_large;
  if (a) {
    local->unswept_large = a->next;
    header_t hd = *(header_t*)((char*)a + LARGE_ALLOC_HEADER_SZ);
    if (Has_status_hd(hd, global.GARBAGE)) {
      free(a);
    } else {
      a->next = local->swept_large;
      local->swept_large = a;
    }
  }
}

int caml_sweep(int work) {
  /* Sweep local pools */
  struct caml_heap_state* local = caml_shared_heap;
  while (work > 0 && local->next_to_sweep < NUM_SIZECLASSES) {
    sizeclass sz = local->next_to_sweep;
    int sweep_work = 
      pool_sweep(local, &local->unswept_avail_pools[sz], sz) +
      pool_sweep(local, &local->unswept_full_pools[sz], sz);
    if (sweep_work) {
      work -= sweep_work;
    } else {
      local->next_to_sweep++;
    }
  }

  /* Sweep global pools */
  while (work > 0 && local->unswept_large) {
    large_alloc_sweep(local);
    work--;
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
  Assert (h && !Has_status_hd(h, global.GARBAGE));
  if (Has_status_hd(h, global.UNMARKED)) {
    Hd_val(p) = With_status_hd(h, global.MARKED);
    return 1;
  } else {
    return 0;
  }
}

const header_t atoms[256] = {
#define A(i) Make_header(0, i, NOT_MARKABLE)
A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),
A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),
A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),
A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),
A(41),A(42),A(43),A(44),A(45),A(46),A(47),A(48),A(49),A(50),
A(51),A(52),A(53),A(54),A(55),A(56),A(57),A(58),A(59),A(60),
A(61),A(62),A(63),A(64),A(65),A(66),A(67),A(68),A(69),A(70),
A(71),A(72),A(73),A(74),A(75),A(76),A(77),A(78),A(79),A(80),
A(81),A(82),A(83),A(84),A(85),A(86),A(87),A(88),A(89),A(90),
A(91),A(92),A(93),A(94),A(95),A(96),A(97),A(98),A(99),A(100),
A(101),A(102),A(103),A(104),A(105),A(106),A(107),A(108),A(109),
A(110),A(111),A(112),A(113),A(114),A(115),A(116),A(117),A(118),
A(119),A(120),A(121),A(122),A(123),A(124),A(125),A(126),A(127),
A(128),A(129),A(130),A(131),A(132),A(133),A(134),A(135),A(136),
A(137),A(138),A(139),A(140),A(141),A(142),A(143),A(144),A(145),
A(146),A(147),A(148),A(149),A(150),A(151),A(152),A(153),A(154),
A(155),A(156),A(157),A(158),A(159),A(160),A(161),A(162),A(163),
A(164),A(165),A(166),A(167),A(168),A(169),A(170),A(171),A(172),
A(173),A(174),A(175),A(176),A(177),A(178),A(179),A(180),A(181),
A(182),A(183),A(184),A(185),A(186),A(187),A(188),A(189),A(190),
A(191),A(192),A(193),A(194),A(195),A(196),A(197),A(198),A(199),
A(200),A(201),A(202),A(203),A(204),A(205),A(206),A(207),A(208),
A(209),A(210),A(211),A(212),A(213),A(214),A(215),A(216),A(217),
A(218),A(219),A(220),A(221),A(222),A(223),A(224),A(225),A(226),
A(227),A(228),A(229),A(230),A(231),A(232),A(233),A(234),A(235),
A(236),A(237),A(238),A(239),A(240),A(241),A(242),A(243),A(244),
A(245),A(246),A(247),A(248),A(249),A(250),A(251),A(252),A(253),
A(254),A(255)
#undef A
};

CAMLexport value caml_atom(tag_t tag) {
  return Val_hp(&atoms[tag]);
}

void caml_init_major_heap (asize_t size) {
}


static __thread value* verify_stack;
static __thread int verify_stack_len;
static __thread int verify_sp;
static __thread intnat verify_objs = 0;
static __thread struct addrmap verify_seen = ADDRMAP_INIT;

static void verify_push(value v, value* p) {
  if (verify_sp == verify_stack_len) {
    verify_stack_len = verify_stack_len * 2 + 100;
    verify_stack = caml_stat_resize(verify_stack,
         sizeof(value*) * verify_stack_len);
  } 
  verify_stack[verify_sp++] = v;
}

static void verify_object(value v) {
  if (!Is_block(v)) return;

  if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
    Assert(Tag_val(v) == Closure_tag);
  }

  intnat* entry = caml_addrmap_insert_pos(&verify_seen, v);
  if (*entry != ADDRMAP_NOT_PRESENT) return;
  *entry = 1;

  if (Has_status_hd(Hd_val(v), NOT_MARKABLE)) return;
  verify_objs++;

  if (!Is_minor(v)) {
    Assert(Has_status_hd(Hd_val(v), global.MARKED));
  }
  if (Tag_val(v) < No_scan_tag) {
    int i;
    for (i = 0; i < Wosize_val(v); i++) {
      value f = Op_val(v)[i];
      if (Is_minor(v) && Is_minor(f)) {
        Assert(caml_owner_of_young_block(v) ==
               caml_owner_of_young_block(f));
      }
      if (Is_block(f)) verify_push(f, 0);
    }
  }
}

static void verify_heap() {
  struct caml_sampled_roots roots;

  caml_sample_local_roots(&roots);
  caml_do_local_roots(&verify_push, &roots);
  caml_scan_global_roots(&verify_push);
  while (verify_sp) verify_object(verify_stack[--verify_sp]);
  caml_gc_log("Verify: %lu objs", verify_objs);

  caml_addrmap_clear(&verify_seen);
  verify_objs = 0;
  caml_stat_free(verify_stack);
  verify_stack = 0;
  verify_stack_len = 0;
  verify_sp = 0;
}


static void verify_pool(pool* a, sizeclass sz) {
  value* v;
  for (v = a->next_obj; v; v = (value*)v[1]) {
    Assert(*v == 0);
  }
  
  value* p = (value*)((char*)a + POOL_HEADER_SZ);
  value* end = (value*)a + POOL_WSIZE;
  mlsize_t wh = wsize_sizeclass[sz];
  
  while (p + wh <= end) {
    header_t hd = (header_t)*p;
    Assert(hd == 0 || Has_status_hd(hd, global.MARKED) || Has_status_hd(hd, global.UNMARKED));
    p += wh;
  }
}

static void verify_freelists () {
  int i;
  struct caml_heap_state* local = caml_shared_heap;
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    /* sweeping should be done by this point */
    Assert(local->unswept_avail_pools[i] == 0 &&
           local->unswept_full_pools[i] == 0);
    pool* p;
    for (p = local->avail_pools[i]; p; p = p->next)
      verify_pool(p, i);
    for (p = local->full_pools[i]; p; p = p->next) {
      Assert(p->next_obj == 0);
      verify_pool(p, i);
    }
  }
}

void caml_cycle_heap_stw() {
  struct global_heap_state oldg = global;
  struct global_heap_state newg;
  verify_heap();
  verify_freelists();
  newg.UNMARKED     = oldg.MARKED;
  newg.GARBAGE      = oldg.UNMARKED;
  newg.MARKED       = oldg.GARBAGE; /* should be empty because garbage was swept */
  global = newg;
}

void caml_cycle_heap() {
  int i;
  struct caml_heap_state* local = caml_shared_heap;
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    local->unswept_avail_pools[i] = local->avail_pools[i];
    local->avail_pools[i] = 0;
    local->unswept_full_pools[i] = local->full_pools[i];
    local->full_pools[i] = 0;
  }
  local->next_to_sweep = 0;
}



#define STAT_ALLOC_MAGIC 0x314159
CAMLexport void * caml_stat_alloc (asize_t sz)
{
  void* result = malloc (sizeof(value) + sz);
  if (result == NULL)
    caml_raise_out_of_memory();
  Hd_hp(result) = Make_header(STAT_ALLOC_MAGIC, Abstract_tag, NOT_MARKABLE);
#ifdef DEBUG
  memset ((void*)Val_hp(result), Debug_uninit_stat, sz);
#endif
  return (void*)Val_hp(result);
}

CAMLexport char * caml_stat_alloc_string(value str)
{
  mlsize_t sz = caml_string_length(str) + 1;
  char * p = caml_stat_alloc(sz);
  memcpy(p, String_val(str), sz);
  return p;
}

CAMLexport void caml_stat_free (void * p)
{
  if (p == NULL) return;
  Assert(Wosize_val((value)p) == STAT_ALLOC_MAGIC);
  Assert(Tag_val((value)p) == Abstract_tag);
  free (Hp_val((value)p));
}

CAMLexport void * caml_stat_resize (void * p, asize_t sz)
{
  void * result;

  if (p == NULL)
    return caml_stat_alloc(sz);

  result = realloc (Hp_val((value)p), sizeof(value) + sz);

  if (result == NULL) {
    caml_stat_free(p);
    caml_raise_out_of_memory ();
  }

  return (void*)Val_hp(result);
}
