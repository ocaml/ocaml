open Format;;
let f1 x = printf "f1 %d@." x;;
let f2 x = printf "f2 %f@." x;;
let f3 x = printf "f3 %s@." x;;
f1 1;;
f2 1.1;;
f3 "1.1.1";;
raise (Failure "test");;
let global_handler e =
  (* Note that I need to give the complete name for eprintf since
     Format is not opened in the new environment of global_handler. *)
  Format.eprintf "global_handler: %s@." (Printexc.to_string e)
