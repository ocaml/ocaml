(* TEST
 expect;
*)
external fail: (int -> int as 'a) -> 'a = "%identity"

[%%expect{|
Line 1, characters 37-39:
1 | external fail: (int -> int as 'a) -> 'a = "%identity"
                                         ^^
Error: This external declaration has a non-syntactic arity,
       its arity is greater than its syntactic arity.
|}]
