(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let poly (id: 'a. 'a -> 'a) =
  let a = id 3
  and b = id 3.0
  and c = id "three"
  in (a, b, c)

let _ok1 = poly (fun x -> x)
let _less_general_1 = poly (fun x -> x + 1)

let _ = poly (let r = ref None in fun x -> r := Some x; x)

class ['a] id1 = object
  method virtual id : 'b. 'b -> 'a
  method id x = x
end
