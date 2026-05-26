(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let x =
  let a : string = 10 in
  let () = if (fun x -> x) then () in
  a ^ "foo"

module R = struct
  let x =
    let a : string = 10 in
    let () = if (fun x -> x) then () in
    a ^ "foo"
end

let () = if (fun x -> x) then () else ()

let f () =
  String.capitalize_ascii (x ^ R.x)

let t : bool = 0
