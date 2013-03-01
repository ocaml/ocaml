type toto = {f: float * int;
          s: string * int;
           i: int;
          l: toto list}

let arr = Array.create 156 1
let l = ref [3; 4; 5]
let f = ref 5.0
let r1 =
  {f = 5.0 +. 7.0, 5;
   s = "testtest", 43;
   i = 45;
   l = []}

let r2 =
  {f = 5.0 +. 7.0, 5;
   s = "testtest", 43;
   i = 45;
   l = [r1;r1;r1;r1;r1]}

let s = "titi"

let _ =
  let t = (!f, s, 4.5) in
  Gc.dump_heap ();
  let f1 (x,_,_) = x in
  let f2 (_,x,_) = x in

  for i = 0 to 5 do
    let _ = Array.make 1000 (int_of_float !f) in
    (* Unix.sleep 2; *)
    Gc.dump_heap ()
  done;
  let x = Test1.z + 1 in
  let y = x + 42 + (List.hd !l) in
  (* Gc.dump_heap (); *)
  print_float (f1 t);
  print_endline (f2 t);
  Printf.printf "%d %f \n%!" y (!f +. 5.)

(* let f = fun () -> *)
(*   for i = 0 to 5 do *)
(*     Array.make 1000 (int_of_float 5.0); *)
(*     (\* Unix.sleep 2; *\) *)
(*     Gc.dump_heap () *)
(*   done *)

(* let _ = *)
(*   let x,y = 1, f () in *)
(*   Gc.dump_heap (); *)
(*   Unix.sleep 1; *)
(*   Gc.dump_heap (); *)
(*   x; *)
(*   Unix.sleep 1; *)
(*   Gc.dump_heap () *)

