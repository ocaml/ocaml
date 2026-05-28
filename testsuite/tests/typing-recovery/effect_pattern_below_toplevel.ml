(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type _ eff += A: 'a -> int eff

let handle_1 a =
  match a () with
  | _ -> "foo"
  | Some (effect A, _) -> "bar"

let handle_2 a =
  match a () with
  | _ -> "baz"
  | [effect A, _] -> "foobar"

let t =
  let a = handle_1 (fun _ -> None)
  and b = handle_2 (fun _ -> [])
  in a ^ b


let k : int = t + t
