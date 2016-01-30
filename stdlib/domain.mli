val spawn : (unit -> unit) -> unit

val self : unit -> int

module BVar : sig
  (* A bvar is a reference, either empty or full,
     supporting atomic "take" and "put" operations.

     These are biased towards one domain. So, take
     and put operations by the same domain will be
     fast, while contended operations will be very
     slow. *)
  type 'a t

  (* Create an (initially full) bvar *)
  external create : 'a -> 'a t = "caml_bvar_create"

  (* Remove the value from a bvar, leaving it empty.
     Raises Not_found if the bvar is already empty.
     If several domains concurrently attempt to take,
     exactly one of them will succeed *)
  external take : 'a t -> 'a = "caml_bvar_take"

  (* Put a value into an empty bvar, leaving it full.
     Raises Invalid_argument if the bvar is already full.
     If several domains concurrently attempt to put,
     exactly one of them will succeed *)
  external put : 'a t -> 'a -> unit = "caml_bvar_put"

  (* Check whether a bvar is empty. Being nonempty/empty is
     no guarantee that a take/put will succeed: an intervening
     take/put may occur *)
  external is_empty : 'a t -> bool = "caml_bvar_is_empty"
end
