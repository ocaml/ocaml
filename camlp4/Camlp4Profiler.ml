(* camlp4r *)

value count =
  let h = Hashtbl.create 1007 in
  let () = at_exit (fun () ->
    let assoc = Hashtbl.fold (fun k v a -> [ (k, v.val) :: a ]) h [] in
    let out = open_out "camlp4_profiler.out" in
    let () = Marshal.to_channel out assoc [] in
    close_out out) in
  fun s ->
    try incr (Hashtbl.find h s)
    with [ Not_found -> Hashtbl.add h s (ref 1) ];

value load = Marshal.from_channel;
