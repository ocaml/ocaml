(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "2"
** check-ocamlc.byte-output
*)

module type T = sig
  type t
  val x : t
  val f : t -> unit
end

module Int = struct
  type t = int
  let x = 42
  let f = print_int
end

module String = struct
  type t = string
  let x = "Forty Two"
  let f = print_endline
end

let r = ref (module Int : T)

module F (X : sig end) = struct
  open struct
    include (val !r)
  end
  type s = t
  let x : s = x
  let f : s -> unit = f
end

module M = struct end

module N = F(M)

let () =
  r := (module String : T)

module O = F(M)

let () =
  O.f N.x
