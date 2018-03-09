#define CAML_INTERNALS

#include <stdlib.h>
#include <string.h>
#include "caml/platform.h"
#include "caml/mlvalues.h"
#include "caml/gc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/sizeclasses.h"
#include "caml/addrmap.h"
#include "caml/roots.h"
#include "caml/globroots.h"
#include "caml/shared_heap.h"
#include "caml/params.h"
#include "caml/fiber.h" /* for verification */

typedef unsigned int sizeclass;
struct global_heap_state global = {0 << 8, 1 << 8, 2 << 8};

typedef struct pool {
  struct pool* next;
  value* next_obj;
  struct domain* owner;
  sizeclass sz;
} pool;
CAML_STATIC_ASSERT(sizeof(pool) == Bsize_wsize(POOL_HEADER_WSIZE));
#define POOL_HEADER_SZ sizeof(pool)

typedef struct large_alloc {
  struct domain* owner;
  struct large_alloc* next;
} large_alloc;
CAML_STATIC_ASSERT(sizeof(large_alloc) % sizeof(value) == 0);
#define LARGE_ALLOC_HEADER_SZ sizeof(large_alloc)

struct {
  caml_plat_mutex lock;
  pool* free;

  /* these only contain swept memory of terminated domains*/
  struct heap_stats stats;
  pool* global_avail_pools[NUM_SIZECLASSES];
  pool* global_full_pools[NUM_SIZECLASSES];
  large_alloc* global_large;
} pool_freelist = {
  CAML_PLAT_MUTEX_INITIALIZER,
  NULL,
  { 0, },
  { 0, },
  NULL
};


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

  struct heap_stats stats;
};

struct caml_heap_state* caml_init_shared_heap() {
  int i;
  struct caml_heap_state* heap;

  heap = caml_stat_alloc(sizeof(struct caml_heap_state));
  heap->free_pools = 0;
  heap->num_free_pools = 0;
  for (i = 0; i<NUM_SIZECLASSES; i++) {
    heap->avail_pools[i] = heap->full_pools[i] =
      heap->unswept_avail_pools[i] = heap->unswept_full_pools[i] = 0;
  }
  heap->next_to_sweep = 0;
  heap->swept_large = 0;
  heap->unswept_large = 0;
  heap->owner = caml_domain_self();
  memset(&heap->stats, 0, sizeof(heap->stats));
  return heap;
}

static int move_all_pools(pool** src, pool** dst, struct domain* new_owner) {
  int count = 0;
  while (*src) {
    pool* p = *src;
    *src = p->next;
    p->owner = new_owner;
    p->next = *dst;
    *dst = p;
    count++;
  }
  return count;
}

void caml_teardown_shared_heap(struct caml_heap_state* heap) {
  int i;
  int released = 0, released_large = 0;
  caml_plat_lock(&pool_freelist.lock);
  heap->num_free_pools -=
    move_all_pools(&heap->free_pools, &pool_freelist.free, NULL);
  Assert(heap->num_free_pools == 0);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    released +=
      move_all_pools(&heap->avail_pools[i], &pool_freelist.global_avail_pools[i], NULL);
    released +=
      move_all_pools(&heap->full_pools[i], &pool_freelist.global_full_pools[i], NULL);
    /* should be swept by now */
    Assert(!heap->unswept_avail_pools[i]);
    Assert(!heap->unswept_full_pools[i]);
  }
  Assert(!heap->unswept_large);
  while (heap->swept_large) {
    large_alloc* a = heap->swept_large;
    heap->swept_large = a->next;
    a->next = pool_freelist.global_large;
    pool_freelist.global_large = a;
    released_large++;
  }
  caml_accum_heap_stats(&pool_freelist.stats, &heap->stats);
  caml_plat_unlock(&pool_freelist.lock);
  caml_stat_free(heap);
  caml_gc_log("Shutdown shared heap. Released %d active pools, %d large",
              released, released_large);
}

void caml_sample_heap_stats(struct caml_heap_state* local, struct heap_stats* h)
{
  *h = local->stats;
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
          r->owner = 0;
          pool_freelist.free = r;
        }
      }
    }
    r = pool_freelist.free;
    if (r)
      pool_freelist.free = r->next;
    caml_plat_unlock(&pool_freelist.lock);
  }
  if (r) Assert (r->owner == 0);
  return r;
}

static void pool_release(struct caml_heap_state* local, pool* pool, sizeclass sz) {
  pool->owner = 0;
  Assert(pool->sz == sz);
  local->stats.pool_words -= POOL_WSIZE;
  local->stats.pool_frag_words -= POOL_HEADER_WSIZE + wastage_sizeclass[sz];
  if (local->num_free_pools < MAX_LOCAL_FREE_POOLS) {
    /* FIXME: in the local cache, other domains can't get it */
    local->num_free_pools++;
    pool->next = local->free_pools;
    local->free_pools = pool;
  } else {
    /* FIXME: give free pools back to the OS */
    caml_plat_lock(&pool_freelist.lock);
    pool->next = pool_freelist.free;
    pool_freelist.free = pool;
    caml_plat_unlock(&pool_freelist.lock);
  }
}

/* Allocating an object from a pool */

static intnat pool_sweep(struct caml_heap_state* local, pool**, sizeclass sz);
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
  r = pool_acquire(local);
  if (!r) return 0; /* if we can't allocate, give up */

  local->stats.pool_words += POOL_WSIZE;
  if (local->stats.pool_words > local->stats.pool_max_words)
    local->stats.pool_max_words = local->stats.pool_words;
  local->stats.pool_frag_words += POOL_HEADER_WSIZE + wastage_sizeclass[sz];

  /* Having allocated a new pool, set it up for size sz */
  local->avail_pools[sz] = r;
  r->next = 0;
  r->owner = local->owner;
  r->next_obj = 0;
  r->sz = sz;
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

  Assert(r->next_obj == 0 || *r->next_obj == 0);
  return p;
}

static void* large_allocate(struct caml_heap_state* local, mlsize_t sz) {
  large_alloc* a = malloc(sz + LARGE_ALLOC_HEADER_SZ);
  if (!a) caml_raise_out_of_memory();
  local->stats.large_words += Wsize_bsize(sz + LARGE_ALLOC_HEADER_SZ);
  if (local->stats.large_words > local->stats.large_max_words)
    local->stats.large_max_words = local->stats.large_words;
  local->stats.large_blocks++;
  a->owner = local->owner;
  a->next = local->swept_large;
  local->swept_large = a;
  return (char*)a + LARGE_ALLOC_HEADER_SZ;
}

value* caml_shared_try_alloc(struct caml_heap_state* local, mlsize_t wosize,
                             tag_t tag, int pinned)
{
  mlsize_t whsize = Whsize_wosize(wosize);
  value* p;
  uintnat colour;

  Assert (wosize > 0);
  Assert (tag != Infix_tag);
  if (whsize <= SIZECLASS_MAX) {
    sizeclass sz = sizeclass_wsize[whsize];
    Assert(wsize_sizeclass[sz] >= whsize);
    p = pool_allocate(local, sz);
    if (!p) return 0;
    struct heap_stats* s = &local->stats;
    s->pool_live_blocks++;
    s->pool_live_words += whsize;
    s->pool_frag_words += wsize_sizeclass[sz] - whsize;
  } else {
    p = large_allocate(local, Bsize_wsize(whsize));
    if (!p) return 0;
  }
  colour = pinned ? NOT_MARKABLE : global.MARKED;
  Hd_hp (p) = Make_header(wosize, tag, colour);
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

struct pool* caml_pool_of_shared_block(value v)
{
  Assert (Is_block(v) && !Is_minor(v));
  mlsize_t whsize = Whsize_wosize(Wosize_val(v));
  if (whsize > 0 && whsize <= SIZECLASS_MAX) {
    return (pool*)((uintnat)v &~(POOL_WSIZE * sizeof(value) - 1));
  } else {
    return 0;
  }
}

/* Sweeping */

static intnat pool_sweep(struct caml_heap_state* local, pool** plist, sizeclass sz) {
  pool* a = *plist;
  if (!a) return 0;
  *plist = a->next;

  value* p = (value*)((char*)a + POOL_HEADER_SZ);
  value* end = (value*)a + POOL_WSIZE;
  mlsize_t wh = wsize_sizeclass[sz];
  int all_free = 1, all_used = 1;
  struct heap_stats* s = &local->stats;

  while (p + wh <= end) {
    header_t hd = (header_t)*p;
    if (hd == 0) {
      /* already on freelist */
      all_used = 0;
    } else if (Has_status_hd(hd, global.GARBAGE)) {
      Assert(Whsize_hd(hd) <= wh);
      /* add to freelist */
      p[0] = 0;
      p[1] = (value)a->next_obj;
      Assert(Is_block((value)p));
      a->next_obj = p;
      all_used = 0;
      /* update stats */
      s->pool_live_blocks--;
      s->pool_live_words -= Whsize_hd(hd);
      s->pool_frag_words -= (wh - Whsize_hd(hd));
    } else {
      /* still live */
      all_free = 0;
    }
    p += wh;
  }

  if (all_free) {
    pool_release(local, a, sz);
  } else {
    pool** list = all_used ? &local->full_pools[sz] : &local->avail_pools[sz];
    a->next = *list;
    *list = a;
  }

  return POOL_WSIZE;
}

static intnat large_alloc_sweep(struct caml_heap_state* local) {
  large_alloc* a = local->unswept_large;
  if (!a) return 0;
  local->unswept_large = a->next;
  header_t hd = *(header_t*)((char*)a + LARGE_ALLOC_HEADER_SZ);
  if (Has_status_hd(hd, global.GARBAGE)) {
    local->stats.large_words -=
      Whsize_hd(hd) + Wsize_bsize(LARGE_ALLOC_HEADER_SZ);
    local->stats.large_blocks--;
    free(a);
  } else {
    a->next = local->swept_large;
    local->swept_large = a;
  }
  return Whsize_hd(hd);
}

static void verify_swept(struct caml_heap_state*);

intnat caml_sweep(struct caml_heap_state* local, intnat work) {
  /* Sweep local pools */
  while (work > 0 && local->next_to_sweep < NUM_SIZECLASSES) {
    sizeclass sz = local->next_to_sweep;
    intnat sweep_work =
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
    work -= large_alloc_sweep(local);
  }

  if (caml_params->verify_heap && work > 0) {
    /* sweeping is complete, check everything worked */
    verify_swept(local);
  }
  return work;
}

uintnat caml_heap_size(struct caml_heap_state* local) {
  return Bsize_wsize(local->stats.pool_words + local->stats.large_words);
}

void caml_redarken_pool(struct pool* r, scanning_action f, void* fdata) {
  mlsize_t wh = wsize_sizeclass[r->sz];
  value* p = (value*)((char*)r + POOL_HEADER_SZ);
  value* end = (value*)((char*)r + Bsize_wsize(POOL_WSIZE));

  while (p + wh <= end) {
    header_t hd = p[0];
    if (hd != 0 && Has_status_hd(hd, global.MARKED)) {
      f(fdata, Val_hp(p), 0);
    }
    p += wh;
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


/* Verify heap invariants.

   Verification happens just after the heap is cycled during STW, so
   everything should be unmarked. If something reachable marked after
   cycling the heap, it means that garbage was reachable beforehand.
*/
struct heap_verify_state {
  value* stack;
  int stack_len;
  int sp;
  intnat objs;
  struct addrmap seen;
};

struct heap_verify_state* caml_verify_begin()
{
  struct heap_verify_state init = {0, 0, 0, 0, ADDRMAP_INIT};
  struct heap_verify_state* st = caml_stat_alloc(sizeof init);
  *st = init;
  return st;
}

void verify_push(void* st_v, value v, value* p)
{
  struct heap_verify_state* st = st_v;
  if (!Is_block(v)) return;

  if (st->sp == st->stack_len) {
    st->stack_len = st->stack_len * 2 + 100;
    st->stack = caml_stat_resize(st->stack,
         sizeof(value*) * st->stack_len);
  }
  st->stack[st->sp++] = v;
}

void caml_verify_root(void* state, value v, value* p)
{
  Assert (!Is_foreign(v));
  verify_push(state, v, p);
}

static void verify_object(struct heap_verify_state* st, value v) {
  if (!Is_block(v)) return;

  Assert (Hd_val(v));
  if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
    Assert(Tag_val(v) == Closure_tag);
  }

  intnat* entry = caml_addrmap_insert_pos(&st->seen, v);
  if (*entry != ADDRMAP_NOT_PRESENT) return;
  *entry = 1;

  if (Has_status_hd(Hd_val(v), NOT_MARKABLE)) return;
  st->objs++;

  // caml_gc_log ("verify_object: v=0x%lx hd=0x%lx tag=%u", v, Hd_val(v), Tag_val(v));
  if (!Is_minor(v)) {
    Assert(Has_status_hd(Hd_val(v), global.UNMARKED));
  }

  if (Tag_val(v) == Stack_tag) {
    caml_scan_stack(verify_push, st, v);
  } else if (Tag_val(v) < No_scan_tag) {
    int i;
    for (i = 0; i < Wosize_val(v); i++) {
      value f = Op_val(v)[i];
      if (Is_minor(v) && Is_minor(f)) {
        Assert(caml_owner_of_young_block(v) ==
               caml_owner_of_young_block(f));
      }
      if (Is_block(f)) verify_push(st, f, 0);
    }
  }
}

void caml_verify_heap(struct heap_verify_state* st) {
  caml_save_stack_gc();
  while (st->sp) verify_object(st, st->stack[--st->sp]);

  caml_addrmap_clear(&st->seen);
  caml_stat_free(st->stack);
  caml_stat_free(st);
  caml_restore_stack_gc();
}


struct mem_stats {
  /* unit is words */
  uintnat alloced;
  uintnat live;
  uintnat free;
  uintnat overhead;

  uintnat live_blocks;
};

static void verify_pool(pool* a, sizeclass sz, struct mem_stats* s) {
  value* v;
  for (v = a->next_obj; v; v = (value*)v[1]) {
    Assert(*v == 0);
  }

  value* p = (value*)((char*)a + POOL_HEADER_SZ);
  value* end = (value*)a + POOL_WSIZE;
  mlsize_t wh = wsize_sizeclass[sz];
  s->overhead += Wsize_bsize(POOL_HEADER_SZ);

  while (p + wh <= end) {
    header_t hd = (header_t)*p;
    Assert(hd == 0 || !Has_status_hd(hd, global.GARBAGE));
    if (hd) {
      s->live += Whsize_hd(hd);
      s->overhead += wh - Whsize_hd(hd);
      s->live_blocks++;
    } else {
      s->free += wh;
    }
    p += wh;
  }
  Assert(end - p == wastage_sizeclass[sz]);
  s->overhead += end - p;
  s->alloced += POOL_WSIZE;
}

static void verify_large(large_alloc* a, struct mem_stats* s) {
  for (; a; a = a->next) {
    header_t hd = *(header_t*)((char*)a + LARGE_ALLOC_HEADER_SZ);
    Assert (!Has_status_hd(hd, global.GARBAGE));
    s->alloced += Wsize_bsize(LARGE_ALLOC_HEADER_SZ) + Whsize_hd(hd);
    s->overhead += Wsize_bsize(LARGE_ALLOC_HEADER_SZ);
    s->live_blocks++;
  }
}

static void verify_swept (struct caml_heap_state* local) {
  int i;
  struct mem_stats pool_stats = {}, large_stats = {};
  /* sweeping should be done by this point */
  Assert(local->next_to_sweep == NUM_SIZECLASSES);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    Assert(local->unswept_avail_pools[i] == 0 &&
           local->unswept_full_pools[i] == 0);
    pool* p;
    for (p = local->avail_pools[i]; p; p = p->next)
      verify_pool(p, i, &pool_stats);
    for (p = local->full_pools[i]; p; p = p->next) {
      Assert(p->next_obj == 0);
      verify_pool(p, i, &pool_stats);
    }
  }
  caml_gc_log("Pooled memory: %lu alloced, %lu free, %lu fragmentation",
              pool_stats.alloced, pool_stats.free, pool_stats.overhead);

  verify_large(local->swept_large, &large_stats);
  Assert(local->unswept_large == 0);
  caml_gc_log("Large memory: %lu alloced, %lu free, %lu fragmentation",
              large_stats.alloced, large_stats.free, large_stats.overhead);

  /* Check stats are being computed correctly */
  Assert(local->stats.pool_words == pool_stats.alloced);
  Assert(local->stats.pool_live_words == pool_stats.live);
  Assert(local->stats.pool_live_blocks == pool_stats.live_blocks);
  Assert(local->stats.pool_frag_words == pool_stats.overhead);
  Assert(local->stats.pool_words -
         (local->stats.pool_live_words + local->stats.pool_frag_words)
         == pool_stats.free);
  Assert(local->stats.large_words == large_stats.alloced);
  Assert(local->stats.large_blocks == large_stats.live_blocks);
}

void caml_cycle_heap_stw() {
  struct global_heap_state oldg = global;
  struct global_heap_state newg;
  newg.UNMARKED     = oldg.MARKED;
  newg.GARBAGE      = oldg.UNMARKED;
  newg.MARKED       = oldg.GARBAGE; /* should be empty because garbage was swept */
  global = newg;
}

void caml_cycle_heap(struct caml_heap_state* local) {
  int i, received_p = 0, received_l = 0;
  caml_gc_log("Cycling heap [%02d]", local->owner->state->id);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    local->unswept_avail_pools[i] = local->avail_pools[i];
    local->avail_pools[i] = 0;
    local->unswept_full_pools[i] = local->full_pools[i];
    local->full_pools[i] = 0;
  }
  local->unswept_large = local->swept_large;
  local->swept_large = 0;

  caml_plat_lock(&pool_freelist.lock);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    received_p += move_all_pools(&pool_freelist.global_avail_pools[i],
                                 &local->unswept_avail_pools[i],
                                 local->owner);
    received_p += move_all_pools(&pool_freelist.global_full_pools[i],
                                 &local->unswept_full_pools[i],
                                 local->owner);
  }
  while (pool_freelist.global_large) {
    large_alloc* a = pool_freelist.global_large;
    pool_freelist.global_large = a->next;
    a->owner = local->owner;
    a->next = local->unswept_large;
    local->unswept_large = a;
    received_l++;
  }
  if (received_p || received_l) {
    caml_accum_heap_stats(&local->stats, &pool_freelist.stats);
    memset(&pool_freelist.stats, 0, sizeof(pool_freelist.stats));
  }
  caml_plat_unlock(&pool_freelist.lock);
  if (received_p || received_l)
    caml_gc_log("Received %d new pools, %d new large allocs", received_p, received_l);

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
