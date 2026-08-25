(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f g =
  match (g ()) with
  | Some x | exception Failure x when String.equal x "foo" ->
      "succeed"
  | _ -> "failure"

let h g x =
  let a = f g in
  let b = match x with
    | Some "foo" -> "foo"
    | None -> "erf"
    | Some _ | exception Not_found when a = "foo" -> ""
  in a ^ b

let t = (h (fun ()-> Some "foo") None) ^ (f (fun () -> None))
let u = t ^ t

let k = u + 10
