(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "0"
** check-ocamlc.byte-output
*)

module Make_sure_val : sig
  val x : int
end = struct
  let x = 3

  open struct
    let x = 'c'
  end
end

type t = A

open struct
  type t = B
end

type ext = ..

module Make_sure_ec : sig
  type ext += C of int
end = struct
  type ext += C of int

  open struct
    type ext += D of char
  end
end


module M = struct type t = int end

open struct
  module M = struct type u = char end
end

module type S = sig type t = int end

open struct
  module type S = sig type u = char end
end

class c = object method x = 3 end

open struct
  class c = object method y = 'c' end
end

class type ct = object method x : int end

open struct
  class type ct = object method y : int end
end
