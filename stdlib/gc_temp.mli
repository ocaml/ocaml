external huge_fallback_count : unit -> int = "caml_gc_huge_fallback_count"
(** Return the number of times we tried to map huge pages and had to fall
    back to small pages. *)

external runtime_variant : unit -> string = "caml_runtime_variant"
(** Return the name of the runtime variant the program is running on.
    This is normally the argument given to [-runtime-variant] at compile
    time, but for byte-code it can be changed after compilation.
   @since 4.03.0
*) (* FIXME This function belongs in [Sys] rather than [Gc]. *)

external runtime_parameters : unit -> string = "caml_runtime_parameters"
(** Return the value of the runtime parameters, in the same format
    as the contents of the OCAMLRUNPARAM environment variable.
   @since 4.03.0
*) (* FIXME move to [Sys] *)
