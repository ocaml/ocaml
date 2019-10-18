(* TEST *)

module IntMap = Map.Make(struct type t = int let compare x y = x-y end)

let m1 = IntMap.add 0 "A" (IntMap.add 4 "Y" (IntMap.singleton 3 "X1"))
let m2 = IntMap.add 0 "B" (IntMap.add 4 "Y" (IntMap.singleton 5 "X2"))

let show m = IntMap.iter (fun k v -> Printf.printf "%d %s\n" k v) m

let () =
  print_endline "Union+concat";
  let f1 _ l r =
    match l, r with
    | Some x, None | None, Some x -> Some x
    | Some x, Some y when x = y -> None
    | Some x, Some y -> Some (x ^ y)
    | _ -> assert false
  in
  show (IntMap.merge f1 m1 m2);

  print_endline "Inter";
  let f2 _ l r =
    match l, r with
    | Some x, Some y when x = y -> Some x
    | _ -> None
  in
  show (IntMap.merge f2 m1 m2);

  print_endline "Union+concat (with Map.union)";
  let f3 _ l r = if l = r then None else Some (l ^ r) in
  show (IntMap.union f3 m1 m2);
  ()

let show m = IntMap.iter (fun k v -> Printf.printf "%d -> %d\n" k v) m

let update x f m =
  let yp = IntMap.find_opt x m in
  let y = f yp in
  match yp, y with
  | _, None -> IntMap.remove x m
  | None, Some z -> IntMap.add x z m
  | Some zp, Some z -> if zp == z then m else IntMap.add x z m

let () =
  print_endline "Update";
  let rec init m  = function
    | -1 -> m
    | n -> init (IntMap.add n n m) (n - 1)
  in
  let n = 9 in
  let m = init IntMap.empty n in
  for i = 0 to n + 1 do
    for j = 0 to n + 1 do
      List.iter (function (k, f) ->
          let m1 = update i f m in
          let m2 = IntMap.update i f m in
          if not (IntMap.equal ( = ) m1 m2 && ((m1 == m) = (m2 == m))) then
          begin
            Printf.printf "ERROR: %s: %d -> %d\n" k i j;
            print_endline "expected result:";
            show m1;
            print_endline "result:";
            show m2;
          end
        )
      [
        "replace",                 (function None -> None   | Some _ -> Some j);
        "delete if exists, bind otherwise",
                                   (function None -> Some j | Some _ -> None);
        "delete",                  (function None -> None   | Some _ -> None);
        "insert",                  (function None -> Some j | Some _ -> Some j);
      ]
    done;
  done

let () =
  let l = [0, "A"; 1, "B"; 2, "C"; 3, "D"; 4, "E"; 5, "F"; 6, "G"; 7, "H"; 8, "I"; 9, "J"] in
  let map = List.fold_right (fun (key, v) map -> IntMap.add key v map) l IntMap.empty in
  let l1 = IntMap.fold (fun key v accu -> (key, v) :: accu) map [] in
  let l2 = IntMap.fold_descending (fun key v accu -> (key, v) :: accu) map [] in
  Printf.printf "%B\n" (List.rev l = l1);
  Printf.printf "%B\n" (l = l2)
