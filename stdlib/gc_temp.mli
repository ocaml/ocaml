(* additional GC functions *)
(* These are in a separate file because we want to do our testing
   on code that doesn't like changes to the Gc module's interface.
   Ultimately, these new functions will be in Gc.
*)

external get_minor_free : unit -> int = "caml_get_minor_free" "noalloc"
(** Return the current size of the free space inside the minor heap. *)

external get_bucket : int -> int = "caml_get_major_bucket" "noalloc"
(** [get_bucket n] returns the current size of the [n]-th future bucket
    of the GC smoothing system. The unit is one millionth of a full GC.
    Raise [Invalid_argument] if [n] is negative, return 0 if n is larger
    than the smoothing window.
*)

external get_credit : unit -> int = "caml_get_major_credit" "noalloc"
(** [get_bucket n] returns the current size of the "work done in advance"
    counter of the GC smoothing system. The unit is one millionth of a
    full GC.
*)
