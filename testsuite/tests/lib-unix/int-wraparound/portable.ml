(* TEST
 modules = "id_types.c";
 include unix;
 hasunix;
 not target-windows;
 { bytecode; }{ native; }
*)

external gid_bits : unit -> int = "gid_bits"
external gid_is_unsigned : unit -> bool = "gid_is_unsigned"

let () =
  assert (Unix.alarm 0 = 0);
  Unix.kill (Unix.getpid ()) 0;
  (try Unix.kill (-2) 0
   with Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> ());
  (try ignore (Unix.waitpid [Unix.WNOHANG] (-2))
   with Unix.Unix_error (Unix.ECHILD, _, _) -> ());
  (match Unix.alarm (-1) with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  let check_gid gid =
    if not (gid_is_unsigned ()) || gid_bits () >= Sys.word_size then
      (try assert ((Unix.getgrgid gid).gr_gid = gid)
       with Not_found | Unix.Unix_error _ -> ())
    else
      match Unix.getgrgid gid with
      | exception Invalid_argument _ -> ()
      | _ -> assert false
  in
  check_gid (-2);
  check_gid (-1)
