let f x = print_string "This is Main.f\n"; x

let () = Registry.register f

let _ =
  Dynlink.init ();
  Dynlink.allow_unsafe_modules true;
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    Printf.printf "Loading %s\n" name; flush stdout;
    try
      if name.[0] = '-'
      then Dynlink.loadfile_private
        (String.sub name 1 (String.length name - 1))
      else Dynlink.loadfile name
    with
      | Dynlink.Error err ->
          Printf.printf "Dynlink error: %s\n"
            (Dynlink.error_message err)
      | exn ->
          Printf.printf "Error: %s\n" (Printexc.to_string exn)
  done;
  flush stdout;
  try
    let oc = open_out_bin "marshal.data" in
    Marshal.to_channel oc (Registry.get_functions()) [Marshal.Closures];
    close_out oc;
    let ic = open_in_bin "marshal.data" in
    let l = (Marshal.from_channel ic : (int -> int) list) in
    close_in ic;
    List.iter
      (fun f ->
        let res = f 0 in
        Printf.printf "Result is: %d\n" res)
      l
  with Failure s ->
    Printf.printf "Failure: %s\n" s
