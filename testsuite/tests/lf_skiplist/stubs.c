#define CAML_INTERNALS

#include "caml/lf_skiplist.h"
#include "caml/memory.h"
#include <assert.h>
#define FMT ARCH_INTNAT_PRINTF_FORMAT

CAMLprim value test_skiplist_serial(value val) {
  CAMLparam0();

  struct lf_skiplist list;

  caml_lf_skiplist_init(&list);

  for (int c = 1; c < 10000; c++) {
    assert(caml_lf_skiplist_insert(&list, c, c));
  }

  for (int c = 1; c < 10000; c++) {
    uintnat j = 0;
    caml_lf_skiplist_find(&list, c, &j);
    assert(j == c);
  }

  for (int c = 1; c < 10000; c++) {
    assert(caml_lf_skiplist_remove(&list, c));
  }

  for (int c = 1; c < 10000; c++) {
    uintnat j = 0;
    assert(caml_lf_skiplist_insert(&list, c, c));
    caml_lf_skiplist_find(&list, c, &j);
    assert(j == c);
    assert(caml_lf_skiplist_remove(&list, c));
  }

  CAMLreturn(Val_unit);
}

static struct lf_skiplist the_list;

CAMLprim value init_skiplist(value val) {
  CAMLparam0();

  caml_lf_skiplist_init(&the_list);

  CAMLreturn(Val_unit);
}

CAMLprim value cardinal_skiplist(value val) {
  CAMLparam0();
  uintnat r = 0;
  FOREACH_LF_SKIPLIST_ELEMENT(p,&the_list,r++);
  CAMLreturn(Val_long(r));
}

static int get_len(struct  lf_skipcell *p, struct lf_skipcell *end) {
  int len = 0 ;
  for ( ; p != end; p = atomic_load(&p->garbage_next)) len++ ;
  return len ;
}


static uintnat count_marks(struct lf_skiplist *sk) {
  uintnat r = 0;
  struct lf_skipcell *p = sk->head;
  uintptr_t succ;

  while (p) {
    for (int k = p->top_level; k >= 0; k--) {
      succ =
        (uintptr_t)atomic_load_explicit(&p->forward[k],memory_order_relaxed);
      if (LF_SK_IS_MARKED(succ)) r++ ;
    }
    p = LF_SK_UNMARK(succ);
  }
  return r;
}

CAMLprim value clean_skiplist(value val) {
  CAMLparam1(val);
  intnat v = Long_val(val) ;

  assert (count_marks(&the_list) == 0) ;
  {
    int len = get_len(atomic_load(&the_list.garbage_head),the_list.head) ;
    if (v >= 0) {
      if (len != v) {
        fprintf(stderr,"len=%d, and v=%" FMT "d differ, space leak detected\n",
                        len,v);
      }
    }
  }
  caml_lf_skiplist_free_garbage(&the_list);
  assert(get_len(atomic_load(&the_list.garbage_head),the_list.head) == 0) ;
  CAMLreturn(Val_unit);
}

CAMLprim value hammer_skiplist(value domain_id_val) {
  CAMLparam1(domain_id_val);

  uintnat domain_id = Long_val(domain_id_val);

  for (int i = 0; i < 100; i++) {
    for (int c = 10000 * domain_id + 1; c < 10000 * (domain_id + 1); c++) {
      assert(caml_lf_skiplist_insert(&the_list, c, c));
    }

    for (int c = 10000 * domain_id + 1; c < 10000 * (domain_id + 1); c++) {
      uintnat j = 0;
      caml_lf_skiplist_find(&the_list, c, &j);
      assert(j == c);
    }

    for (int c = 10000 * domain_id + 1; c < 10000 * (domain_id + 1); c++) {
      assert(caml_lf_skiplist_remove(&the_list, c));
    }

    for (int c = 10000 * domain_id + 1; c < 10000 * (domain_id + 1); c++) {
      uintnat j = 0;
      assert(caml_lf_skiplist_insert(&the_list, c, c));
      caml_lf_skiplist_find(&the_list, c, &j);
      assert(j == c);
      assert(caml_lf_skiplist_remove(&the_list, c));
    }
  }

  CAMLreturn(Val_unit);
}

inline static uintnat calc_value(uintnat id) { return id; }
inline static uintnat calc_key(uintnat id,uintnat turn) { return 1024*id+turn+1; }
inline static uintnat calc_right(uintnat id,uintnat turn,uintnat ndoms) { return (id+turn) % ndoms; }

CAMLprim value insert_skiplist(value turn_val,value ndoms_val,value domain_id_val) {
  CAMLparam3(turn_val,ndoms_val,domain_id_val);
  uintnat domain_id = Long_val(domain_id_val);
  uintnat ndoms = Long_val(ndoms_val);
  uintnat turn = Long_val(turn_val);
  uintnat k = calc_key(domain_id,turn) ;
  uintnat v =  calc_value(domain_id) ;
  //  fprintf(stderr,"I: %" FMT "u -> %" FMT "u\n",k,v);
  int r = caml_lf_skiplist_insert(&the_list, k, v) ;
  assert(r);
  CAMLreturn(Val_unit);
}

CAMLprim value find_skiplist(value turn_val,value ndoms_val,value domain_id_val) {
  CAMLparam3(turn_val,ndoms_val,domain_id_val);
  uintnat domain_id = Long_val(domain_id_val);
  uintnat ndoms = Long_val(ndoms_val);
  uintnat turn = Long_val(turn_val);
  uintnat right = calc_right(domain_id,turn,ndoms) ; // neighbour on the right
  uintnat k = calc_key(right,turn);
  uintnat w = 0 ;
  int r = caml_lf_skiplist_find(&the_list, k, &w);
  if (r) {
    assert(w == calc_value(right));
    assert(caml_lf_skiplist_remove(&the_list, k));
    assert(!caml_lf_skiplist_remove(&the_list, k));
    //    fprintf(stderr,"R: %lu -> %lu\n",k,w);
  }
  CAMLreturn(Val_bool(r));
}
