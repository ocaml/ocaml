(* additional GC functions *)
(* These are in a separate file because we want to do our testing
   on code that doesn't like changes to the Gc module's interface.
   Ultimately, these new functions will be in Gc.
*)

external get_minor_free : unit -> int = "caml_get_minor_free";;
