(* TEST
 flags = "-g";
 native;
*)

(* Test DWARF emission with records and variants *)
type point = { x : int; y : int }

type shape =
  | Circle of int
  | Rectangle of int * int
  | Point of point

let area = function
  | Circle r -> 3 * r * r
  | Rectangle (w, h) -> w * h
  | Point _ -> 0

let () =
  let shapes = [
    Circle 5;
    Rectangle (4, 6);
    Point { x = 10; y = 20 }
  ] in
  List.iter (fun s -> Printf.printf "%d\n" (area s)) shapes
