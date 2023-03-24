(* TEST
   * expect
*)

module X = struct
  let t = ref None
end

module type X' = module type of X
[%%expect{|
module X : sig val t : '_weak1 option ref end
Line 5, characters 32-33:
5 | module type X' = module type of X
                                    ^
Error: The type of this module, sig val t : '_weak1 option ref end,
       contains the non-generalizable type variable(s): '_weak1.
       (see manual section 6.1.2)
Line 2, characters 6-7:
2 |   let t = ref None
          ^
  This value has the non-generalizable type '_weak1 option ref.
|}]
