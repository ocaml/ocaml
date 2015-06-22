#ifndef CAML_DOMAIN_STATE
#define CAML_DOMAIN_STATE

#include <stddef.h>

struct caml_domain_state {
#define DOMAIN_STATE(idx, type, name) type name;
#include "domain_state.tbl"
#undef DOMAIN_STATE
};

CAMLextern __thread struct caml_domain_state* caml_domain_state;

/* Statically assert that each field of domain_state is one word long and at the right index */

#define CAML_STATIC_ASSERT_3(b, l) \
  typedef char static_assertion_failure_line_##l[(b) ? 1 : -1]
#define CAML_STATIC_ASSERT_2(b, l) CAML_STATIC_ASSERT_3(b, l)
#define CAML_STATIC_ASSERT(b) CAML_STATIC_ASSERT_2(b, __LINE__)


#define DOMAIN_STATE(idx, type, name) \
  CAML_STATIC_ASSERT(sizeof(caml_domain_state->name) == sizeof(void*)); \
  CAML_STATIC_ASSERT(offsetof(struct caml_domain_state, name) == idx * sizeof(void*));
#include "domain_state.tbl"
#undef DOMAIN_STATE


#endif
