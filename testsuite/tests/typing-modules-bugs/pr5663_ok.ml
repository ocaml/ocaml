(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module F (M : sig
    type 'a t
    type 'a u = string
    val f : unit -> _ u t
  end) = struct
    let t = M.f ()
  end
