open Printf

let f () = [3.14; 2.718]

let do_save s =
  let oc = open_out_bin s in
  Marshal.to_channel oc f [Marshal.Closures];
  close_out oc

let do_load s =
  let ic = open_in_bin s in
  begin try
    let g : unit -> float list = Marshal.from_channel ic in
    List.iter (fun n -> printf "%.5f\n" n) (g())
  with Failure err ->
    printf "Error during unmarshaling\n"
  end;
  close_in ic

let _ =
  match Sys.argv with
  | [| _ ; "save"; s |] -> do_save s
  | [| _ ; "load"; s |] -> do_load s
  | _ -> eprintf "Usage: closures [save|load] <filename>\n"; exit 2

