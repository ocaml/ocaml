(* TEST
    flags = "-dsource"
    * expect
*)

let type nonrec t = t in 0
[%%expect{|

;;let open struct type nonrec t = t end in 0;;
Line 1, characters 20-21:
1 | let type nonrec t = t in 0
                        ^
Error: Unbound type constructor t
|}]
;;

let type t = A of u | B and u = C of t in match B with A _ -> 2 | B -> 3
[%%expect{|

;;let open struct type t =
                    | A of u
                    | B
                  and u =
                    | C of t  end in
    match B with | A _ -> 2 | B -> 3;;
- : int = 3
|}]
;;

let type%test t = A of u | B and u = C of t in match B with A _ -> 2 | B -> 3
[%%expect{|

;;let open struct [%%test type t =
                            | A of u
                            | B
                          and u =
                            | C of t ] end in
    match B with | A _ -> 2 | B -> 3;;
Line 1, characters 9-13:
1 | let type%test t = A of u | B and u = C of t in match B with A _ -> 2 | B -> 3
             ^^^^
Error: Uninterpreted extension 'test'.
|}]
;;
