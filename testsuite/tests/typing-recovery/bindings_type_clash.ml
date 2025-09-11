(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let apply x f = f x
let pair x y = x, y

module A = struct

  let (let+) = (fun x f -> not x)
  let (and+) = pair

  let f =
    let+ x = 1
    and+ y = 2
    and+ z = 3 in
    x + y + z
end

module B = struct

  let (let+) = apply
  let (and+) = fun x y -> x + 1, y

  let f =
    let+ x = 1
    and+ y = 2
    and+ z = 3 in
    x + y + z
end
