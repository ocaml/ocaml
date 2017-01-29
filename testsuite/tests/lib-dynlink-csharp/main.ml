let load s =
  Printf.printf "Loading %s\n%!" (Filename.basename s);
  try
    Dynlink.loadfile s
  with Dynlink.Error e ->
    print_endline (Dynlink.error_message e)

(* Callback must be linked to load Unix dynamically *)
let _ = Callback.register
module CamlinternalBigarray = CamlinternalBigarray

let () =
  ignore (Hashtbl.hash 42.0);
  print_endline "Main is running.";
  Dynlink.init ();
  Dynlink.allow_unsafe_modules true;
  let s1,s2,s3 =
    Dynlink.adapt_filename (Filename.concat (Sys.getenv "UNIX_LIB_DIR") "unix.cma"),
    Dynlink.adapt_filename (Filename.concat (Sys.getenv "BIGARRAY_LIB_DIR") "bigarray.cma"),
    Dynlink.adapt_filename "plugin.cmo"
  in
  load s1;
  load s2;
  load s3;
  print_endline "OK."
