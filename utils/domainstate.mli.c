val minor_heap_sel_bits : int
val minor_heap_align_bits : int

type t =
#define DOMAIN_STATE(n, v) | Domain_##n
#include "domain_state.tbl"
#undef DOMAIN_STATE

val idx_of_field : t -> int
