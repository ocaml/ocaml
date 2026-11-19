(* TEST
   hasunix;
   hassysthreads;
   include systhreads;
   {
     arguments = "10 10 100";
     bytecode;
   }
   {
     arguments = "100 1 100";
     native;
   }
   {
     arguments = "10 10 100";
     native;
   }
   {
     arguments = "1 100 100";
     native;
   }
 *)

(** raise a Unix_error from C after registering a local root,
    do *not* replace this with an OCaml raise, that wouldn't register a local root
 *)
let raise_catch_uerror () =
  match Unix.openfile "/nosuchfile.ocaml" [Unix.O_RDONLY] 0 with
  | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
      ()
  | _ -> assert false

let in_thread n =
  for _ = 1 to n do
    let s = String.make 16384 'x' in
    raise_catch_uerror ();
    let _ : string = Sys.opaque_identity s in
    ()
  done

let () =
  let outer_loop = int_of_string Sys.argv.(1)
  and threads = int_of_string Sys.argv.(2)
  and inner_loop = int_of_string Sys.argv.(3) in
  for _ = 1 to outer_loop do
    let t = Array.init threads (fun _ -> Thread.create in_thread inner_loop) in
    Array.iter Thread.join t
  done;
  Gc.full_major ()
