(* TEST
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)
module Foo = struct
  type 'a t

  let create : ('a -> unit) -> 'a t =
    fun  _ -> assert false
end

module type Bar = sig
  type s = A

  val created : s Foo.t
end

let baz : (module Bar) =
  (module (struct
    type s = A

    let created = Foo.create (fun _ -> ())
  end))
