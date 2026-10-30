(* TEST
   ocamlrunparam += "V=1,v=0";
   flags = "-runtime-variant=d";
   {
     bytecode;
   }
   {
     native;
   }
*)
let () =
  let x = Array.init 10000 (fun _ -> String.make 10 'a') in
  Gc.full_major ();
  let _alive = Sys.opaque_identity x in
  Gc.full_major ();
  let x = Array.init 100000 (fun _ -> String.make 10 'a') in
  Array.iteri (fun i _ -> if i mod 2 = 0 then x.(i) <- "") x;
  Gc.compact ();
  Gc.full_major ();
  let _alive = Sys.opaque_identity x in
  Gc.compact ();
  Gc.full_major ()
