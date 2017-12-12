(* TEST
   * expect
*)

let partition_map f xs =
 let rec part left right = function
   | [] -> List.rev left, List.rev right
   | x::xs ->
       match f x with
       | `Left v -> part (v::left) right xs
       | `Right v -> part left (v::right) xs
 in
 part [] [] xs
;;

let f xs : (int list * int list) = partition_map (fun x -> if x then `Left ()
else `Right ()) xs
;;
[%%expect{|
val partition_map :
  ('a -> [< `Left of 'b | `Right of 'c ]) -> 'a list -> 'b list * 'c list =
  <fun>
Line _, characters 35-96:
Error: This expression has type unit list * unit list
       but an expression was expected of type int list * int list
       Type unit is not compatible with type int
|}]
