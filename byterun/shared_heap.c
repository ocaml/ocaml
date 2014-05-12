#include <stdlib.h>
#include <string.h>
#include "plat_threads.h"
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
    p[0] = (value)Make_header(0, 0, NOT_MARKABLE);
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
      p[0] = (value)Make_header(0, 0, NOT_MARKABLE);
      p[1] = (value)a->next_obj;
      a->next_obj = p;
    } else if (Has_status_hd(hd, NOT_MARKABLE)) {
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
      Op_val(Val_hp(p))[i] = Debug_free_major;
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


static __thread struct addrmap objs = ADDRMAP_INIT;
static __thread uintnat nobjs = 0;

static void verify_object(value v) {
  int i;
  intnat* entry;
  if (!Is_block(v)) return;

  if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
    Assert(Tag_val(v) == Closure_tag);
  }
  
  entry = caml_addrmap_insert_pos(&objs, v);
  if (*entry != ADDRMAP_NOT_PRESENT) return;
  *entry = 0;

  if (Has_status_hd(Hd_val(v), NOT_MARKABLE)) return;

  nobjs++;

  Assert(!Is_minor(v));

  if (!Is_minor(v)) {
    Assert(Has_status_hd(Hd_val(v), global.MARKED));
  }
  if (Tag_val(v) < No_scan_tag) {
    for (i = 0; i < Wosize_val(v); i++) {
      verify_object(Field(v, i));
    }
  }
}

static void verify_root(value v, value* p) {
  if (!v) return;
  verify_object(v);
}

static void verify_heap() {
  struct caml_sampled_roots roots;
  caml_addrmap_clear(&objs);
  nobjs = 0;
  caml_sample_local_roots(&roots);
  caml_do_local_roots(&verify_root, &roots);
  caml_scan_global_roots(&verify_root);
  caml_gc_log("Verify: %lu objs", nobjs);
  caml_addrmap_clear(&objs);
}

void caml_cycle_heap() {
  struct global_heap_state oldg = global;
  struct global_heap_state newg;
  verify_heap();
  newg.UNMARKED     = oldg.MARKED;
  newg.GARBAGE      = oldg.UNMARKED;
  newg.MARKED       = oldg.GARBAGE; /* should be empty because garbage was swept */
  global = newg;
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
