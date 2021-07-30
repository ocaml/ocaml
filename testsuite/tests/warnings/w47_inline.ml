(* TEST

flags = "-w +A-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

let a = (fun x -> x) [@inline] (* accepted *)
let b = (fun x -> x) [@inline never] (* accepted *)
let c = (fun x -> x) [@inline always] (* accepted *)
let d = (fun x -> x) [@inline malformed attribute] (* rejected *)
let e = (fun x -> x) [@inline malformed_attribute] (* rejected *)
let f = (fun x -> x) [@inline : malformed_attribute] (* rejected *)
let g = (fun x -> x) [@inline ? malformed_attribute] (* rejected *)

let h x = (a [@inlined]) x (* accepted *)
let i x = (a [@inlined never]) x (* accepted *)
let j x = (a [@inlined always]) x (* accepted *)
let k x = (a [@inlined malformed]) x (* rejected *)

let l x = x [@@inline] (* accepted *)


let test x =
  let[@local always] f1 x = x (* ok *) in
  let[@local never] f2 x = x (* ok *) in
  let[@local malformed] f3 x = x (* bad payload *) in
  let[@local] f4 x = 2 * x (* not local *) in
  let[@local] f5 x = f1 x (* ok *) in
  let[@local] f6 x = 3 * x (* ok *) in
  let r =
    if x = 1 then f1 x
    else if x = 2 then f4 x
    else if x = 3 then f1 x
    else f5 x
  in
  f4 (f6 r)
