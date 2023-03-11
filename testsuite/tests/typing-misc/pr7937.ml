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
|}]

let g: 'a. 'a r -> 'a r = fun x -> { contents = 0 };;
[%%expect {|
Line 1, characters 35-51:
1 | let g: 'a. 'a r -> 'a r = fun x -> { contents = 0 };;
                                       ^^^^^^^^^^^^^^^^
Error: This expression has type int ref
       but an expression was expected of type ([< `X of int & 'a ] as 'a) r
|}]

let h: 'a. 'a r -> _ = function true | false -> ();;
[%%expect {|
Line 1, characters 32-36:
1 | let h: 'a. 'a r -> _ = function true | false -> ();;
                                    ^^^^
Error: This pattern should not be a boolean literal, the expected type is
       ([< `X of int & 'a ] as 'a) r
|}]


let i: 'a. 'a r -> _ = function { contents = 0 } -> ();;
[%%expect {|
Line 1, characters 32-48:
1 | let i: 'a. 'a r -> _ = function { contents = 0 } -> ();;
                                    ^^^^^^^^^^^^^^^^
Error: This pattern should not be a record, the expected type is
       ([< `X of int & 'a ] as 'a) r
|}]
