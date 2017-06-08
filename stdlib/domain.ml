type t
external spawn : (unit -> unit) -> t = "caml_domain_spawn"

external self : unit -> int = "caml_ml_domain_id"

external join : t -> unit = "caml_ml_domain_join"

module BVar = struct
  type 'a t
  external create : 'a -> 'a t = "caml_bvar_create"
  external take : 'a t -> 'a = "caml_bvar_take"
  external put : 'a t -> 'a -> unit = "caml_bvar_put"
  external is_empty : 'a t -> bool = "caml_bvar_is_empty"
end
