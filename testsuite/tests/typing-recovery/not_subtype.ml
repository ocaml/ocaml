(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)


(* The expansion is a bit disappointing and less precise than when
   recovery mode is not active. *)

type u0 = int * int -> unit
type u = u0 -> unit
type v0 = float * float -> unit
type v = v0 -> unit
let f = (g: u -> unit :> v -> unit)

class a = object
  method foo x = x + 1
end

class b = object
  method foo x = x - 1
  method bar x = x + 1
end

let f x = (x: b :> a)

let g x =
  let g x = (x: a :> b) in
  (g x) # foo + 1

let t : string = 10
