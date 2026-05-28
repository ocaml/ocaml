(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

class type t = object
  method foo : int -> unit
  method bar : int
end

let f (x : t) : unit * int * string * string =
  let a = x # foo 10 in
  let b = x # bar in
  let c = x # baz and y = x # foobar in
  (a, b, c, y)

(* Should work *)
let f (x : #t) : unit * int * string * string =
  let a = x # foo 10 in
  let b = x # bar in
  let c = x # baz and y = x # foobar in
  (a, b, c, y)

let _ =
  let a = object
    method foo = 10
  end in
  let b = (a # bar) + (a # foo) in
  b + 12

let t : int = true
