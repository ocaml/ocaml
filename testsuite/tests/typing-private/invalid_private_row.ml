(* TEST
   * expect
*)

(** Error message for trying to make private a row type variable
    that only exists syntactically *)

type a = [`A | `C | `D]
type b = [`B | `D | `E]
type c = private [< a | b > `A `B `C `D `E]
[%%expect {|
type a = [ `A | `C | `D ]
type b = [ `B | `D | `E ]
Line 6, characters 0-43:
6 | type c = private [< a | b > `A `B `C `D `E]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This private row type declaration is invalid.
       The type expression on the right-hand side reduces to
         [ `A | `B | `C | `D | `E ]
       which does not have a free row type variable.
       Hint: If you intended to define a private type abbreviation,
       write explicitly
         private [ `A | `B | `C | `D | `E ]
|}]

type u = private < x:int; .. > as 'a constraint 'a = < x: int > ;;
[%%expect {|
Line 1, characters 0-63:
1 | type u = private < x:int; .. > as 'a constraint 'a = < x: int > ;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This private row type declaration is invalid.
       The type expression on the right-hand side reduces to
         < x : int >
       which does not have a free row type variable.
       Hint: If you intended to define a private type abbreviation,
       write explicitly
         private < x : int >
|}]

type u = private [> `A ] as 'a constraint 'a = [< `A ] ;;
[%%expect {|
Line 1, characters 0-54:
1 | type u = private [> `A ] as 'a constraint 'a = [< `A ] ;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This private row type declaration is invalid.
       The type expression on the right-hand side reduces to
         [ `A ]
       which does not have a free row type variable.
       Hint: If you intended to define a private type abbreviation,
       write explicitly
         private [ `A ]
|}]
