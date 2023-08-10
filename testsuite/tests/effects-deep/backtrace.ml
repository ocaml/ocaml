(* TEST_BELOW
(* Blank lines added here to preserve locations. *)

*)

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

type wait = effect
  | Wait : unit

let wait = Effect.create ~name:"wait" ()

let task1 () =
  try
    bar 42; None
  with e ->
    Some (e, Printexc.get_raw_backtrace ())

let rec task2 i =
  if i = 0 then ()
  else begin
    Effect.perform wait Wait;
    task2 i
  end
  [@@inline never]

let main () =
  let (x, bt) = Option.get (task1 ()) in
  Effect.run_with wait task2 42
    { result = Fun.id;
      exn = (fun e ->
        let open Printexc in
        print_raw_backtrace stdout (get_raw_backtrace ()));
      operation =
        (fun op k -> Effect.discontinue_with_backtrace k x bt); }

let _ = main ()

(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 {
   reference = "${test_source_directory}/backtrace.byte.reference";
   bytecode;
 }
 {
   reference = "${test_source_directory}/backtrace.opt.reference";
   native;
 }
*)
