module Debug = struct value mode _ = False; end;

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

value main () =

  let profile = List.sort (fun (_, v1) (_, v2) -> compare v1 v2) (load stdin) in

  List.iter
    (fun (k, v) -> Format.printf "%-75s: %d@." k v)
    profile;

if Sys.argv.(0) = "camlp4prof" then main () else ();
