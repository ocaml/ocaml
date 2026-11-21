(* TEST
 include unix;
 hasunix;
 not target-windows;
 { bytecode; }{ native; }
*)

let () =
  assert (Unix.alarm 0 = 0);
  Unix.kill (Unix.getpid ()) 0;
  (match Unix.alarm (-1) with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  (if Sys.int_size = 31 then
     (try assert ((Unix.getgrgid (-2)).gr_gid = -2)
      with Not_found | Unix.Unix_error _ -> ())
   else
     match Unix.getgrgid (-2) with
     | exception Invalid_argument _ -> ()
     | _ -> assert false);
  (try ignore (Unix.getgrgid (-1))
   with Not_found | Unix.Unix_error _ -> ())
