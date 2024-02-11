(* TEST
 no-tsan; (* Takes too much time with tsan *)
 {
 bytecode;
 }
 {
 native;
 }
*)

(* Compaction crash when there is only one heap chunk and it is fully used. *)
let c = ref []

let () =
  for i = 0 to 25000 do
    c := 0 :: !c;
    Gc.compact ()
  done
