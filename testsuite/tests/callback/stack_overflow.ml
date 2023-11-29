(* TEST
 include unix;
 modules = "stack_overflow_.c";
 libunix;
 {
   bytecode;
 }{
   native;
 }
*)

external caml_to_c : (unit -> 'a) -> 'a = "caml_to_c"

let rec deep = function
  | 0 ->
     ref 42
  | n ->
     caml_to_c (fun () -> deep (n-1))

open Effect

type eff = effect E : unit

let eff = Effect.create ()

let () =
  Printf.printf "%d\n%!" (!(deep 1000));
  Printf.printf "%d\n%!"
    (run_with eff deep 1000
     { result = (fun x -> !x);
       exn = (fun e -> raise e);
       operation =
         (fun (type a) (E : (a, eff) operation) k -> assert false) })
