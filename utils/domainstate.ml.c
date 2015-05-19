(* must be kept in sync with byterun/config.h
   (FIXME: should be done using CPP or similar) *)
let minor_heap_sel_bits = 8
let minor_heap_align_bits = 24

type t =
#define DOMAIN_STATE(n, v) | Domain_##n
#include "domain_state.tbl"
#undef DOMAIN_STATE

let idx_of_field = function
#define DOMAIN_STATE(n, v) | Domain_##n -> v
#include "domain_state.tbl"
#undef DOMAIN_STATE
