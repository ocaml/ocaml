(* TEST
   frame_pointers;
   readonly_files = "fp_backtrace.c";
   all_modules = "${readonly_files} exceptions.ml";
   native;
 *)

external fp_backtrace : string -> unit = "fp_backtrace" [@@noalloc]

exception FortyTwo

(* We want to ensure backtraces from raiser, handler and catcher are correct.
 *)
let [@inline never] handler i =
  Printf.printf "# handler %d\n%!" i;
  fp_backtrace Sys.argv.(0);
  i + 1

let [@inline never] raiser i =
  Printf.printf "# raiser %d\n%!" i;
  fp_backtrace Sys.argv.(0);
  match i with
  | 42 -> raise FortyTwo
  | _ -> i

let [@inline never] catcher i =
  Printf.printf "# catcher %d\n%!" i;
  fp_backtrace Sys.argv.(0);
  try raiser i with
  | FortyTwo -> ignore (handler i);
                i

let () =
  ignore (catcher 42)
