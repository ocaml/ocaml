(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type 'a t = A of 'a
let rec A x = A (A ())

let f ~x:_ ~y:_ ~z:_ = ()

let c =
  (* Should works, OCaml up to 5.4 *)
  let rec (_ as f) = fun () -> f () in
  f ()

let rec x = f ~x
let rec x = f ~x ~z:0

let g ~omitted_g ~given = fun ~omitted_f:_ ~given:_ -> given ~omitted_f:()
let rec f : omitted_g:_ -> omitted_f:_ -> given:_ -> _ =
  g ~given:(f ~omitted_g:() ~given:0)


let (a, b) = c, c
let t : string = 10
