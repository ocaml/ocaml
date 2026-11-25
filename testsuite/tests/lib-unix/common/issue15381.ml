(* TEST
 include unix;
 hasunix;
 not target-windows;
 { bytecode; }{ native; }
*)

let () =
  let f = "f.txt" in close_out (open_out f);
  List.iter (fun t -> match Unix.utimes f t t with
    | () -> Printf.printf "utimes %.1f -> ok, mtime %.3f\n" t (Unix.stat f).Unix.st_mtime
    | exception Unix.Unix_error (e,_,_) ->
        Printf.printf "utimes %.1f -> %s\n" t (Unix.error_message e))
    [-100.0; -100.5; 100.5; -0.25]
