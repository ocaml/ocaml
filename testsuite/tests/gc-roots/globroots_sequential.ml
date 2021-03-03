(* TEST
   flags += " -w a "
   modules = "globrootsprim.c globroots.ml"
*)
open Globroots

module Test(G: GLOBREF) = struct

  let size = 1024

  let vals = Array.init size Int.to_string

  let a = Array.init size (fun i -> G.register (Int.to_string i))

  let check () =
    for i = 0 to size - 1 do
      if G.get a.(i) <> vals.(i) then begin
        print_string "Error on "; print_int i; print_string ": ";
        print_string (String.escaped (G.get a.(i))); print_newline()
      end
    done

  let change () =
    match Random.int 37 with
    | 0 ->
        Gc.full_major()
    | 1|2|3|4 ->
        Gc.minor()
    | 5|6|7|8|9|10|11|12 ->             (* update with young value *)
        let i = Random.int size in
        G.set a.(i) (Int.to_string i)
    | 13|14|15|16|17|18|19|20 ->        (* update with old value *)
        let i = Random.int size in
        G.set a.(i) vals.(i)
    | 21|22|23|24|25|26|27|28 ->        (* re-register young value *)
        let i = Random.int size in
        G.remove a.(i);
        a.(i) <- G.register (Int.to_string i)
    | (*29|30|31|32|33|34|35|36*) _ ->  (* re-register old value *)
        let i = Random.int size in
        G.remove a.(i);
        a.(i) <- G.register vals.(i)

  let test n =
    for i = 1 to n do
      change(); check ();
      print_string "."; flush stdout
    done
end

module TestClassic = Test(Classic)
module TestGenerational = Test(Generational)

let _ = young2old (); Gc.full_major ()

let _ =
  assert (static2young (1, 1) Gc.full_major == 0x42)

let _ =
  let n =
    if Array.length Sys.argv < 2 then 10000 else int_of_string Sys.argv.(1) in
  print_string "Non-generational API\n";
  TestClassic.test n;
  print_newline();
  print_string "Generational API\n";
  TestGenerational.test n;
  print_newline()
