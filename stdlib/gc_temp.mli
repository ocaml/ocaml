external huge_fallback_count : unit -> int = "caml_gc_huge_fallback_count"
(** Return the number of times we tried to map huge pages and had to fall
    back to small pages. *)
