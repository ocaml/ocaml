(* TEST
flags = " -w a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module type Priv = sig
  type t = private int
end

module Make (Unit:sig end): Priv = struct type t = int end

module A = Make (struct end)

module type Priv' = sig
  type t = private [> `A]
end

module Make' (Unit:sig end): Priv' = struct type t = [`A] end

module A' = Make' (struct end)
