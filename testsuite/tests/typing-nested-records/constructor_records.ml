(* TEST
 expect;
*)

(* Nested record definitions are rejected inside constructor inline
   records, for all constructor paths. *)

type variant = A of { value : { x : int } }
[%%expect{|
Line 1, characters 22-41:
1 | type variant = A of { value : { x : int } }
                          ^^^^^^^^^^^^^^^^^^^
Error: Nested record definitions are not supported inside
       constructor inline records.
|}]

type _ gadt = G : { value : { x : int } } -> int gadt
[%%expect{|
Line 1, characters 20-39:
1 | type _ gadt = G : { value : { x : int } } -> int gadt
                        ^^^^^^^^^^^^^^^^^^^
Error: Nested record definitions are not supported inside
       constructor inline records.
|}]

exception E of { value : { x : int } }
[%%expect{|
Line 1, characters 17-36:
1 | exception E of { value : { x : int } }
                     ^^^^^^^^^^^^^^^^^^^
Error: Nested record definitions are not supported inside
       constructor inline records.
|}]

type ext = ..
type ext += X : { value : { x : int } } -> ext
[%%expect{|
type ext = ..
Line 2, characters 18-37:
2 | type ext += X : { value : { x : int } } -> ext
                      ^^^^^^^^^^^^^^^^^^^
Error: Nested record definitions are not supported inside
       constructor inline records.
|}]

(* Nested record definitions are still supported in ordinary records. *)

type ordinary = { value : { x : int } }
[%%expect{|
type ordinary = { value : { x : int; }; }
|}]
