/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2015 Indian Institute of Technology, Madras                */
/*   Copyright 2015 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/
#define CAML_INTERNALS

#include <stdlib.h>
#include <string.h>

#include "caml/addrmap.h"
#include "caml/custom.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/fiber.h" /* for verification */
#include "caml/gc.h"
#include "caml/globroots.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/roots.h"
#include "caml/shared_heap.h"
#include "caml/sizeclasses.h"
#include "caml/startup_aux.h"

typedef unsigned int sizeclass;
struct global_heap_state caml_global_heap_state = {0 << 8, 1 << 8, 2 << 8};

typedef struct pool {
  struct pool* next;
  value* next_obj;
  caml_domain_state* owner;
  sizeclass sz;
} pool;
CAML_STATIC_ASSERT(sizeof(pool) == Bsize_wsize(POOL_HEADER_WSIZE));
#define POOL_HEADER_SZ sizeof(pool)

typedef struct large_alloc {
  caml_domain_state* owner;
  struct large_alloc* next;
} large_alloc;
CAML_STATIC_ASSERT(sizeof(large_alloc) % sizeof(value) == 0);
#define LARGE_ALLOC_HEADER_SZ sizeof(large_alloc)

static struct {
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
  { 0, },
  NULL
};

/* readable and writable only by the current thread */
struct caml_heap_state {
  pool* avail_pools[NUM_SIZECLASSES];
  pool* full_pools[NUM_SIZECLASSES];
  pool* unswept_avail_pools[NUM_SIZECLASSES];
  pool* unswept_full_pools[NUM_SIZECLASSES];

  large_alloc* swept_large;
  large_alloc* unswept_large;

  sizeclass next_to_sweep;

  caml_domain_state* owner;

  struct heap_stats stats;
};


/* You need to hold the [pool_freelist] lock to call these functions. */
static void orphan_heap_stats_with_lock(struct caml_heap_state *);
static void adopt_pool_stats_with_lock(struct caml_heap_state *,
                                       pool *, sizeclass);

struct caml_heap_state* caml_init_shared_heap (void) {
  int i;
  struct caml_heap_state* heap;

  heap = caml_stat_alloc_noexc(sizeof(struct caml_heap_state));
  if(heap != NULL) {
    for (i = 0; i<NUM_SIZECLASSES; i++) {
      heap->avail_pools[i] = heap->full_pools[i] =
        heap->unswept_avail_pools[i] = heap->unswept_full_pools[i] = 0;
    }
    heap->next_to_sweep = 0;
    heap->swept_large = NULL;
    heap->unswept_large = NULL;
    heap->owner = Caml_state;
    memset(&heap->stats, 0, sizeof(heap->stats));
  }
  return heap;
}

static int move_all_pools(pool** src, pool** dst, caml_domain_state* new_owner){
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
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    released +=
      move_all_pools(&heap->avail_pools[i],
                     &pool_freelist.global_avail_pools[i], NULL);

    released +=
      move_all_pools(&heap->full_pools[i],
                     &pool_freelist.global_full_pools[i], NULL);

    /* should be swept by now */
    CAMLassert(!heap->unswept_avail_pools[i]);
    CAMLassert(!heap->unswept_full_pools[i]);
  }
  CAMLassert(!heap->unswept_large);
  while (heap->swept_large) {
    large_alloc* a = heap->swept_large;
    heap->swept_large = a->next;
    a->next = pool_freelist.global_large;
    pool_freelist.global_large = a;
    released_large++;
  }
  orphan_heap_stats_with_lock(heap);
  caml_plat_unlock(&pool_freelist.lock);
  caml_stat_free(heap);
  caml_gc_log("Shutdown shared heap. Released %d active pools, %d large",
              released, released_large);
}


/* Allocating and deallocating pools from the global freelist. */

#define POOLS_PER_ALLOCATION 16
static pool* pool_acquire(struct caml_heap_state* local) {
  pool* r;

  caml_plat_lock(&pool_freelist.lock);
  if (!pool_freelist.free) {
    void* mem = caml_mem_map(Bsize_wsize(POOL_WSIZE) * POOLS_PER_ALLOCATION,
                              Bsize_wsize(POOL_WSIZE), 0 /* allocate */);
    int i;
    if (mem) {
      CAMLassert(pool_freelist.free == NULL);
      for (i=0; i<POOLS_PER_ALLOCATION; i++) {
        r = (pool*)(((uintnat)mem) + ((uintnat)i) * Bsize_wsize(POOL_WSIZE));
        r->next = pool_freelist.free;
        r->owner = NULL;
        pool_freelist.free = r;
      }
    }
  }
  r = pool_freelist.free;
  if (r)
    pool_freelist.free = r->next;
  caml_plat_unlock(&pool_freelist.lock);

  if (r) CAMLassert (r->owner == NULL);
  return r;
}

static void pool_release(struct caml_heap_state* local,
                         pool* pool,
                         sizeclass sz) {
  pool->owner = NULL;
  CAMLassert(pool->sz == sz);
  local->stats.pool_words -= POOL_WSIZE;
  local->stats.pool_frag_words -= POOL_HEADER_WSIZE + wastage_sizeclass[sz];
  /* TODO: give free pools back to the OS. Issue #698 */
  caml_plat_lock(&pool_freelist.lock);
  pool->next = pool_freelist.free;
  pool_freelist.free = pool;
  caml_plat_unlock(&pool_freelist.lock);
}

static void calc_pool_stats(pool* a, sizeclass sz, struct heap_stats* s) {
  value* p = (value*)((char*)a + POOL_HEADER_SZ);
  value* end = (value*)a + POOL_WSIZE;
  mlsize_t wh = wsize_sizeclass[sz];
  s->pool_frag_words += Wsize_bsize(POOL_HEADER_SZ);

  while (p + wh <= end) {
    header_t hd = (header_t)atomic_load_relaxed((atomic_uintnat*)p);
    if (hd) {
      s->pool_live_words += Whsize_hd(hd);
      s->pool_frag_words += wh - Whsize_hd(hd);
      s->pool_live_blocks++;
    }

    p += wh;
  }
  CAMLassert(end - p == wastage_sizeclass[sz]);
  s->pool_frag_words += end - p;
  s->pool_words += POOL_WSIZE;
}

/* Initialize a pool and its object freelist */
Caml_inline void pool_initialize(pool* r,
                                 sizeclass sz,
                                 caml_domain_state* owner)
{
  mlsize_t wh = wsize_sizeclass[sz];
  value* p = (value*)((char*)r + POOL_HEADER_SZ);
  value* end = (value*)((char*)r + Bsize_wsize(POOL_WSIZE));

  r->next = 0;
  r->owner = owner;
  r->next_obj = 0;
  r->sz = sz;

  p[0] = 0;
  p[1] = 0;
  p += wh;

  while (p + wh <= end) {
    p[0] = 0; /* zero header indicates free object */
    p[1] = (value)(p - wh);
    p += wh;
  }
  r->next_obj = p - wh;
}

/* Allocating an object from a pool */
static intnat pool_sweep(struct caml_heap_state* local,
                         pool**,
                         sizeclass sz ,
                         int release_to_global_pool);

/* Adopt pool from the pool_freelist avail and full pools
   to satisfy an alloction */
static pool* pool_global_adopt(struct caml_heap_state* local, sizeclass sz)
{
  pool* r = NULL;
  int adopted_pool = 0;

  /* probably no available pools out there to be had */
  if( !pool_freelist.global_avail_pools[sz] &&
      !pool_freelist.global_full_pools[sz] )
    return NULL;

  /* Haven't managed to find a pool locally, try the global ones */
  caml_plat_lock(&pool_freelist.lock);
  if( pool_freelist.global_avail_pools[sz] ) {
    r = pool_freelist.global_avail_pools[sz];

    if( r ) {
      pool_freelist.global_avail_pools[sz] = r->next;
      r->next = 0;
      local->avail_pools[sz] = r;
      adopt_pool_stats_with_lock(local, r, sz);

      #ifdef DEBUG
      {
        value* next_obj = r->next_obj;
        while( next_obj ) {
          CAMLassert(next_obj[0] == 0);
          next_obj = (value*)next_obj[1];
        }
      }
      #endif

    }
  }

  /* There were no global avail pools, so let's adopt one of the full ones and
     try our luck sweeping it later on */
  if( !r ) {
    r = pool_freelist.global_full_pools[sz];

    if( r ) {
      pool_freelist.global_full_pools[sz] = r->next;
      r->next = local->full_pools[sz];
      local->full_pools[sz] = r;
      adopt_pool_stats_with_lock(local, r, sz);

      adopted_pool = 1;
      r = 0; // this pool is full
    }
  }

  caml_plat_unlock(&pool_freelist.lock);

  if( !r && adopted_pool ) {
    local->owner->major_work_todo -=
      pool_sweep(local, &local->full_pools[sz], sz, 0);
    r = local->avail_pools[sz];
  }
  return r;
}

/* Allocating an object from a pool */
static pool* pool_find(struct caml_heap_state* local, sizeclass sz) {
  pool* r;

  /* Hopefully we have a pool we can use directly */
  r = local->avail_pools[sz];
  if (r) return r;

  /* Otherwise, try to sweep until we find one */
  while (!local->avail_pools[sz] && local->unswept_avail_pools[sz]) {
    local->owner->major_work_todo -=
      pool_sweep(local, &local->unswept_avail_pools[sz], sz, 0);
  }

  r = local->avail_pools[sz];
  if (r) return r;

  /* Haven't managed to find a pool locally, try the global ones */
  r = pool_global_adopt(local, sz);
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
  pool_initialize(r, sz, local->owner);

  return r;
}

static void* pool_allocate(struct caml_heap_state* local, sizeclass sz) {
  value* p;
  value* next;
  pool* r = pool_find(local, sz);

  if (!r) return 0;

  p = r->next_obj;
  next = (value*)p[1];
  r->next_obj = next;
  CAMLassert(p[0] == 0);
  if (!next) {
    local->avail_pools[sz] = r->next;
    r->next = local->full_pools[sz];
    local->full_pools[sz] = r;
  }

  CAMLassert(r->next_obj == 0 || *r->next_obj == 0);
  return p;
}

static void* large_allocate(struct caml_heap_state* local, mlsize_t sz) {
  large_alloc* a = malloc(sz + LARGE_ALLOC_HEADER_SZ);
  if (!a) return NULL;
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

  CAMLassert (wosize > 0);
  CAMLassert (tag != Infix_tag);

  CAML_EV_ALLOC(wosize);

  if (whsize <= SIZECLASS_MAX) {
    struct heap_stats* s;
    sizeclass sz = sizeclass_wsize[whsize];
    CAMLassert(wsize_sizeclass[sz] >= whsize);
    p = pool_allocate(local, sz);
    if (!p) return 0;
    s = &local->stats;
    s->pool_live_blocks++;
    s->pool_live_words += whsize;
    s->pool_frag_words += wsize_sizeclass[sz] - whsize;
  } else {
    p = large_allocate(local, Bsize_wsize(whsize));
    if (!p) return 0;
  }
  colour = pinned ? NOT_MARKABLE : caml_global_heap_state.MARKED;
  Hd_hp (p) = Make_header(wosize, tag, colour);
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

struct pool* caml_pool_of_shared_block(value v)
{
  mlsize_t whsize;
  CAMLassert (Is_block(v) && !Is_young(v));
  whsize = Whsize_wosize(Wosize_val(v));
  if (whsize > 0 && whsize <= SIZECLASS_MAX) {
    return (pool*)((uintnat)v &~(POOL_WSIZE * sizeof(value) - 1));
  } else {
    return 0;
  }
}

/* Sweeping */

static intnat pool_sweep(struct caml_heap_state* local, pool** plist,
                         sizeclass sz, int release_to_global_pool) {
  intnat work = 0;
  pool* a = *plist;
  if (!a) return 0;
  *plist = a->next;

  {
    value* p = (value*)((char*)a + POOL_HEADER_SZ);
    value* end = (value*)a + POOL_WSIZE;
    mlsize_t wh = wsize_sizeclass[sz];
    int all_used = 1;
    struct heap_stats* s = &local->stats;

    while (p + wh <= end) {
      header_t hd = (header_t)atomic_load_relaxed((atomic_uintnat*)p);
      if (hd == 0) {
        /* already on freelist */
        all_used = 0;
      } else if (Has_status_hd(hd, caml_global_heap_state.GARBAGE)) {
        CAMLassert(Whsize_hd(hd) <= wh);
        if (Tag_hd (hd) == Custom_tag) {
          void (*final_fun)(value) = Custom_ops_val(Val_hp(p))->finalize;
          if (final_fun != NULL) final_fun(Val_hp(p));
        }
        /* add to freelist */
        atomic_store_relaxed((atomic_uintnat*)p, 0);
        p[1] = (value)a->next_obj;
        CAMLassert(Is_block((value)p));
#ifdef DEBUG
        {
          int i;
          mlsize_t wo = Wosize_whsize(wh);
          for (i = 2; i < wo; i++) {
            Field(Val_hp(p), i) = Debug_free_major;
          }
        }
#endif
        a->next_obj = p;
        all_used = 0;
        /* update stats */
        s->pool_live_blocks--;
        s->pool_live_words -= Whsize_hd(hd);
        local->owner->swept_words += Whsize_hd(hd);
        s->pool_frag_words -= (wh - Whsize_hd(hd));
      } else {
        /* still live, the pool can't be released to the global freelist */
        release_to_global_pool = 0;
      }
      p += wh;
      work += wh;
    }

    if (release_to_global_pool) {
      pool_release(local, a, sz);
    } else {
      pool** list = all_used ? &local->full_pools[sz] : &local->avail_pools[sz];
      a->next = *list;
      *list = a;
    }
  }

  return work;
}

static intnat large_alloc_sweep(struct caml_heap_state* local) {
  value* p;
  header_t hd;
  large_alloc* a = local->unswept_large;
  if (!a) return 0;
  local->unswept_large = a->next;

  p = (value*)((char*)a + LARGE_ALLOC_HEADER_SZ);
  hd = (header_t)*p;
  if (Has_status_hd(hd, caml_global_heap_state.GARBAGE)) {
    if (Tag_hd (hd) == Custom_tag) {
      void (*final_fun)(value) = Custom_ops_val(Val_hp(p))->finalize;
      if (final_fun != NULL) final_fun(Val_hp(p));
    }

    local->stats.large_words -=
      Whsize_hd(hd) + Wsize_bsize(LARGE_ALLOC_HEADER_SZ);
    local->owner->swept_words +=
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
    intnat full_sweep_work = 0;
    intnat avail_sweep_work =
      pool_sweep(local, &local->unswept_avail_pools[sz], sz, 1);
    work -= avail_sweep_work;

    if (work > 0) {
      full_sweep_work = pool_sweep(local,
                                   &local->unswept_full_pools[sz],
                                   sz, 1);

      work -= full_sweep_work;
    }

    if(full_sweep_work+avail_sweep_work == 0) {
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

uintnat caml_top_heap_words(struct caml_heap_state* local) {
  /* FIXME: summing two maximums computed at different points in time
     returns an incorrect result. */
  return local->stats.pool_max_words + local->stats.large_max_words;
}


uintnat caml_heap_blocks(struct caml_heap_state* local) {
  return local->stats.pool_live_blocks + local->stats.large_blocks;
}

void caml_redarken_pool(struct pool* r, scanning_action f, void* fdata) {
  mlsize_t wh = wsize_sizeclass[r->sz];
  value* p = (value*)((char*)r + POOL_HEADER_SZ);
  value* end = (value*)((char*)r + Bsize_wsize(POOL_WSIZE));

  while (p + wh <= end) {
    header_t hd = p[0];
    if (hd != 0 && Has_status_hd(hd, caml_global_heap_state.MARKED)) {
      f(fdata, Val_hp(p), 0);
    }
    p += wh;
  }
}


/* Heap and freelist stats */

/* Move the given heap stats to the orphan pools.
   You need to hold the [pool_freelist] lock. */
static void orphan_heap_stats_with_lock(struct caml_heap_state *heap) {
  caml_accum_heap_stats(&pool_freelist.stats, &heap->stats);
  memset(&heap->stats, 0, sizeof(heap->stats));
}

/* The stats for an adopted pool are moved from the free pool stats to
   the heap stats of the adopting domain.
   You need to hold the [pool_freelist] lock. */
static void adopt_pool_stats_with_lock(
  struct caml_heap_state* adopter, pool *r, sizeclass sz)
{
    struct heap_stats pool_stats = { 0, };

    calc_pool_stats(r, sz, &pool_stats);
    caml_accum_heap_stats(&adopter->stats, &pool_stats);
    caml_remove_heap_stats(&pool_freelist.stats, &pool_stats);
}

/* Move the stats of all orphan pools into the given heap.
   You need to hold the [pool_freelist] lock. */
static void adopt_all_pool_stats_with_lock(struct caml_heap_state *adopter) {
  caml_accum_heap_stats(&adopter->stats, &pool_freelist.stats);
  memset(&pool_freelist.stats, 0, sizeof(pool_freelist.stats));
}

void caml_collect_heap_stats_sample(
  struct caml_heap_state* local,
  struct heap_stats* sample)
{
  *sample = local->stats;
}

/* Add the orphan pool stats to a stats accumulator. */
void caml_accum_orphan_heap_stats(struct heap_stats* acc)
{
  caml_plat_lock(&pool_freelist.lock);
  caml_accum_heap_stats(acc, &pool_freelist.stats);
  caml_plat_unlock(&pool_freelist.lock);
}


/* Atoms */
static const header_t atoms[256] = {
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

struct heap_verify_state* caml_verify_begin (void)
{
  struct heap_verify_state init = {0, 0, 0, 0, ADDRMAP_INIT};
  struct heap_verify_state* st = caml_stat_alloc(sizeof init);
  *st = init;
  return st;
}

static void verify_push (void* st_v, value v, value* p)
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
  verify_push(state, v, p);
}

static scanning_action_flags verify_scanning_flags = 0;

static void verify_object(struct heap_verify_state* st, value v) {
  intnat* entry;
  if (!Is_block(v)) return;

  CAMLassert (!Is_young(v));
  CAMLassert (Hd_val(v));

  if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
    CAMLassert(Tag_val(v) == Closure_tag);
  }

  entry = caml_addrmap_insert_pos(&st->seen, v);
  if (*entry != ADDRMAP_NOT_PRESENT) return;
  *entry = 1;

  if (Has_status_hd(Hd_val(v), NOT_MARKABLE)) return;
  st->objs++;

  CAMLassert(Has_status_hd(Hd_val(v), caml_global_heap_state.UNMARKED));

  if (Tag_val(v) == Cont_tag) {
    struct stack_info* stk = Ptr_val(Field(v, 0));
    if (stk != NULL)
      caml_scan_stack(verify_push, verify_scanning_flags, st, stk, 0);
  } else if (Tag_val(v) < No_scan_tag) {
    int i = 0;
    if (Tag_val(v) == Closure_tag) {
      i = Start_env_closinfo(Closinfo_val(v));
    }
    for (; i < Wosize_val(v); i++) {
      value f = Field(v, i);
      if (Is_block(f)) verify_push(st, f, Op_val(v)+i);
    }
  }
}

void caml_verify_heap(caml_domain_state *domain) {
  struct heap_verify_state* st = caml_verify_begin();
  caml_do_roots (&caml_verify_root, verify_scanning_flags, st, domain, 1);
  caml_scan_global_roots(&caml_verify_root, st);

  while (st->sp) verify_object(st, st->stack[--st->sp]);

  caml_addrmap_clear(&st->seen);
  caml_stat_free(st->stack);
  caml_stat_free(st);
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
    CAMLassert(*v == 0);
  }

  {
    value* p = (value*)((char*)a + POOL_HEADER_SZ);
    value* end = (value*)a + POOL_WSIZE;
    mlsize_t wh = wsize_sizeclass[sz];
    s->overhead += Wsize_bsize(POOL_HEADER_SZ);

    while (p + wh <= end) {
      header_t hd = (header_t)*p;
      CAMLassert(hd == 0 || !Has_status_hd(hd, caml_global_heap_state.GARBAGE));
      if (hd) {
        s->live += Whsize_hd(hd);
        s->overhead += wh - Whsize_hd(hd);
        s->live_blocks++;
      } else {
        s->free += wh;
      }
      p += wh;
    }
    CAMLassert(end - p == wastage_sizeclass[sz]);
    s->overhead += end - p;
    s->alloced += POOL_WSIZE;
  }
}

static void verify_large(large_alloc* a, struct mem_stats* s) {
  for (; a; a = a->next) {
    header_t hd = *(header_t*)((char*)a + LARGE_ALLOC_HEADER_SZ);
    CAMLassert (!Has_status_hd(hd, caml_global_heap_state.GARBAGE));
    s->alloced += Wsize_bsize(LARGE_ALLOC_HEADER_SZ) + Whsize_hd(hd);
    s->overhead += Wsize_bsize(LARGE_ALLOC_HEADER_SZ);
    s->live_blocks++;
  }
}

static void verify_swept (struct caml_heap_state* local) {
  int i;
  struct mem_stats pool_stats = {}, large_stats = {};

  /* sweeping should be done by this point */
  CAMLassert(local->next_to_sweep == NUM_SIZECLASSES);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    pool* p;
    CAMLassert(local->unswept_avail_pools[i] == NULL &&
               local->unswept_full_pools[i] == NULL);
    for (p = local->avail_pools[i]; p; p = p->next)
      verify_pool(p, i, &pool_stats);
    for (p = local->full_pools[i]; p; p = p->next) {
      CAMLassert(p->next_obj == NULL);
      verify_pool(p, i, &pool_stats);
    }
  }
  caml_gc_log("Pooled memory: %" ARCH_INTNAT_PRINTF_FORMAT
                "u alloced, %" ARCH_INTNAT_PRINTF_FORMAT
                "u free, %" ARCH_INTNAT_PRINTF_FORMAT
                "u fragmentation",
              pool_stats.alloced, pool_stats.free, pool_stats.overhead);

  verify_large(local->swept_large, &large_stats);
  CAMLassert(local->unswept_large == NULL);
  caml_gc_log("Large memory: %" ARCH_INTNAT_PRINTF_FORMAT
                "u alloced, %" ARCH_INTNAT_PRINTF_FORMAT
                "u free, %" ARCH_INTNAT_PRINTF_FORMAT
                "u fragmentation",
              large_stats.alloced, large_stats.free, large_stats.overhead);

  /* Check stats are being computed correctly */
  CAMLassert(local->stats.pool_words == pool_stats.alloced);
  CAMLassert(local->stats.pool_live_words == pool_stats.live);
  CAMLassert(local->stats.pool_live_blocks == pool_stats.live_blocks);
  CAMLassert(local->stats.pool_frag_words == pool_stats.overhead);
  CAMLassert(local->stats.pool_words -
         (local->stats.pool_live_words + local->stats.pool_frag_words)
         == pool_stats.free);
  CAMLassert(local->stats.large_words == large_stats.alloced);
  CAMLassert(local->stats.large_blocks == large_stats.live_blocks);
}

void caml_cycle_heap_stw (void) {
  struct global_heap_state oldg = caml_global_heap_state;
  struct global_heap_state newg;
  newg.UNMARKED     = oldg.MARKED;
  newg.GARBAGE      = oldg.UNMARKED;
  newg.MARKED       = oldg.GARBAGE; /* should be empty because
                                        garbage was swept */
  caml_global_heap_state = newg;
}

void caml_cycle_heap(struct caml_heap_state* local) {
  int i, received_p = 0, received_l = 0;

  caml_gc_log("Cycling heap [%02d]", local->owner->id);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    CAMLassert(local->unswept_avail_pools[i] == NULL);
    local->unswept_avail_pools[i] = local->avail_pools[i];
    local->avail_pools[i] = NULL;
    CAMLassert(local->unswept_full_pools[i] == NULL);
    local->unswept_full_pools[i] = local->full_pools[i];
    local->full_pools[i] = NULL;
  }
  CAMLassert(local->unswept_large == NULL);
  local->unswept_large = local->swept_large;
  local->swept_large = NULL;

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
    adopt_all_pool_stats_with_lock(local);
  }
  caml_plat_unlock(&pool_freelist.lock);
  if (received_p || received_l)
    caml_gc_log("Received %d new pools, %d new large allocs",
                received_p, received_l);

  local->next_to_sweep = 0;
}
