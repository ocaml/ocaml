#define CAML_INTERNALS

#include "caml/lf_skiplist.h"
#include "caml/memory.h"
#include <assert.h>

CAMLextern value test_skiplist_serial(value val) {
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

CAMLextern value init_skiplist(value val) {
  CAMLparam0();

  caml_lf_skiplist_init(&the_list);

  CAMLreturn(Val_unit);
}

CAMLextern value hammer_skiplist(value domain_id_val) {
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