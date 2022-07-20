(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
*)

open Effect
open Effect.Deep

let rec foo i =
  if i = 0 then ()
  else begin
    ignore (failwith "exn");
    foo i
  end
  [@@inline never]

let rec bar i =
  if i = 0 then ()
  else begin
    foo i;
    bar i
  end
  [@@inline never]

type _ t += Wait : unit t

let task1 () =
  try
    bar 42; None
  with e ->
    Some (e, Printexc.get_raw_backtrace ())

let rec task2 i =
  if i = 0 then ()
  else begin
    perform Wait;
    task2 i
  end
  [@@inline never]

let main () =
  let (x, bt) = Option.get (task1 ()) in
  match_with task2 42
  { retc = Fun.id;
    exnc = (fun e ->
      let open Printexc in
      print_raw_backtrace stdout (get_raw_backtrace ()));
    effc = fun (type a) (e : a t) ->
      match e with
      | Wait -> Some (fun (k : (a, _) continuation) ->
          discontinue_with_backtrace k x bt)
      | _ -> None }

let _ = main ()
