type t =
#define DOMAIN_STATE(type, name) | Domain_##name
#include "domain_state.tbl"
#undef DOMAIN_STATE

val idx_of_field : t -> int
