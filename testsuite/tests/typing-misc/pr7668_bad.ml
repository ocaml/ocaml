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
Lines 12-13, characters 35-18:
12 | ...................................partition_map (fun x -> if x then `Left ()
13 | else `Right ()) xs
Error: This expression has type unit list * unit list
       but an expression was expected of type int list * int list
       Type unit is not compatible with type int
|}]

module M : sig
  type t = [
    | `A of int
    | `B of [ `BA | `BB of unit list ]
    | `C of unit ]

  val a : t -> t
end = struct
  type t = [
    | `A of int
    | `B of [ `BA | `BB of unit list ]
    | `C of unit ]

let a b =
  let f = function
    | Ok x -> x
    | Error _ -> `C ()
  in
  f (match b with
      | `A pc ->
        begin match pc with
          | 1 -> Ok (`B `BA)
          | _ -> Ok (`B (`BB [1;2;3]))
        end
      | _ -> assert false)

end
;;
[%%expect{|
Lines 8-27, characters 6-3:
 8 | ......struct
 9 |   type t = [
10 |     | `A of int
11 |     | `B of [ `BA | `BB of unit list ]
12 |     | `C of unit ]
...
24 |         end
25 |       | _ -> assert false)
26 |
27 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t =
               [ `A of int | `B of [ `BA | `BB of unit list ] | `C of unit ]
           val a :
             [> `A of int ] ->
             [> `B of [> `BA | `BB of int list ] | `C of unit ]
         end
       is not included in
         sig
           type t =
               [ `A of int | `B of [ `BA | `BB of unit list ] | `C of unit ]
           val a : t -> t
         end
       Values do not match:
         val a :
           [> `A of int ] ->
           [> `B of [> `BA | `BB of int list ] | `C of unit ]
       is not included in
         val a : t -> t
       The type
         [ `A of int | `B of [ `BA | `BB of unit list ] | `C of unit ] ->
         [> `B of [> `BA | `BB of int list ] | `C of unit ]
       is not compatible with the type t -> t
       Type [> `B of [> `BA | `BB of int list ] | `C of unit ]
       is not compatible with type
         t = [ `A of int | `B of [ `BA | `BB of unit list ] | `C of unit ]
       Types for tag `BB are incompatible
|}]
