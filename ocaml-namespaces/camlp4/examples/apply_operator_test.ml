let ( & ) = ();; (* To force it to be inlined. If not it's not well typed. *)

fun f g h x -> f& g& h x
