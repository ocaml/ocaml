(* TEST

flags = "-w A"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

let _ = Some 0 = Some 1

let _ = (Some 0 = Some 1) [@warning "-67"]

let _ =
  let [@warning "-67"] _ = Some 0 = Some 1 in
  ()

let [@warning "-67"] _ = Some 0 = Some 1
and _ = Some 0 = Some 1
and _ = Some 0 = Some 1 [@@warning "-67"]

let () =
  if Some 0 = Some 1 then ();
  (if Some 0 = Some 1 then ()) [@warning "-67"];
  if Some 0 = Some 1 then ();

[@@@warning "-67"]

let _ = Some 0 = Some 1
