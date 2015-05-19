#ifndef CAML_DOMAIN_STATE
#define CAML_DOMAIN_STATE

CAMLextern __thread uintnat* caml_tls_area;


enum {
#define DOMAIN_STATE(name, num) Caml_##name = num,
#include "domain_state.tbl"
#undef DOMAIN_STATE
};

#define Caml_domain_state(id) (caml_tls_area[id])

#endif
