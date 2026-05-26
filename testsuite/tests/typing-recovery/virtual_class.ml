(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

class virtual t = object
  method virtual foo : string -> string
end

class virtual k = object
  method virtual foo : string -> string
  method virtual bar : unit
end

class r = object
  inherit k
  method foo x = x
  method bar = ()
end


let f = new t
let g = new k

let r =
  let a = new t in
  let b = new r in
  (b # foo) (a # foo "Hello World")

let x = new r

let t: bool = 10
