(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module type T = sig
  type t
  val x : t
  val show : t -> string
end

module Int = struct
  type t = int
  let x = 0
  let show x = Int.to_string x
end

module String = struct
  type t = string
  let x = "Hello"
  let show x = x
end

module type S = sig
  module Choice : T
  val r : Choice.t list ref ref
end

module Force (X : functor () -> S) = struct end

let () =
  let switch = ref true in
  let module Choose () = struct
    module Choice =
      (val if !switch then (module Int : T)
        else (module String : T))
    let r = ref (ref [])
  end in
  let module M = Choose () in
  let () = switch := false in
  let module N = Choose () in
  let () = N.r := !M.r in
  let module Ignore = Force(Choose) in
  let module M' = (M : S) in
  let () = (!M'.r) := [M'.Choice.x] in
  let module N' = (N : S) in
  List.iter (fun x -> print_string (N'.Choice.show x)) !(!N'.r)
