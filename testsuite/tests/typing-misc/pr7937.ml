(* TEST
   * expect
*)

type 'a r = [< `X of int & 'a ] as 'a

let f: 'a. 'a r -> 'a r = fun x -> true;;
[%%expect {|
type 'a r = 'a constraint 'a = [< `X of int & 'a ]
Line 3, characters 35-39:
3 | let f: 'a. 'a r -> 'a r = fun x -> true;;
                                       ^^^^
Error: This expression has type bool but an expression was expected of type
         ([< `X of int & 'a ] as 'a) r
       Types for tag `X are incompatible
|}, Principal{|
type 'a r = 'a constraint 'a = [< `X of int & 'a ]
Line 3, characters 35-39:
3 | let f: 'a. 'a r -> 'a r = fun x -> true;;
                                       ^^^^
Error: This expression has type bool but an expression was expected of type
         ([< `X of 'b & 'a & 'c & 'd & 'e ] as 'a) r
       Types for tag `X are incompatible
|}]

let g: 'a. 'a r -> 'a r = fun x -> { contents = 0 };;
[%%expect {|
Line 1, characters 35-51:
1 | let g: 'a. 'a r -> 'a r = fun x -> { contents = 0 };;
                                       ^^^^^^^^^^^^^^^^
Error: This expression has type int ref
       but an expression was expected of type ([< `X of int & 'a ] as 'a) r
       Types for tag `X are incompatible
|}, Principal{|
Line 1, characters 35-51:
1 | let g: 'a. 'a r -> 'a r = fun x -> { contents = 0 };;
                                       ^^^^^^^^^^^^^^^^
Error: This expression has type int ref
       but an expression was expected of type
         ([< `X of 'b & 'a & 'c & 'd & 'e ] as 'a) r
       Types for tag `X are incompatible
|}]

let h: 'a. 'a r -> _ = function true | false -> ();;
[%%expect {|
Line 1, characters 32-36:
1 | let h: 'a. 'a r -> _ = function true | false -> ();;
                                    ^^^^
Error: This pattern matches values of type bool
       but a pattern was expected which matches values of type
         ([< `X of int & 'a ] as 'a) r
       Types for tag `X are incompatible
|}]


let i: 'a. 'a r -> _ = function { contents = 0 } -> ();;
[%%expect {|
Line 1, characters 32-48:
1 | let i: 'a. 'a r -> _ = function { contents = 0 } -> ();;
                                    ^^^^^^^^^^^^^^^^
Error: This pattern matches values of type int ref
       but a pattern was expected which matches values of type
         ([< `X of int & 'a ] as 'a) r
       Types for tag `X are incompatible
|}]
