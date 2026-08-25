(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)


let (f : x:int -> int) = fun y -> y

let t =
  let (f : int -> int) = fun ~y -> y in
  f 10

module R = struct
class t =
  object
    method foo (f : int -> int) = f ~x:10
  end
end

let t : string = 10
