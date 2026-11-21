(* TEST
 expect;
*)

(* This demonstrates that rigid-variants mode is still required.
   Removing expand_head_rigid will cause this to fail to typecheck.
*)

type 'a t = int constraint 'a = [< `A of < num : int; .. > ]

module X : sig
  class ['a] c :
    'a ->
    object ('c)
      constraint 'a = int * [< `A of < num : int; .. > ]
    end
end = struct
  class ['a] c (a : 'a) =
  object (s)
      constraint 'a = 'b t * 'b
  end
end

[%%expect{|
type 'a t = int constraint 'a = [< `A of < num : int; .. > ]
module X :
  sig
    class ['a] c :
      'a ->
      object constraint 'a = int * ([< `A of < num : int; .. > ] as 'b) end
  end
|}]

(* Same thing as a GADT *)
module M : sig
  type _ g = G : ([< `A of < num : int; .. > ] as 'b) -> (int * 'b) g
end = struct
  type _ g = G : 'b -> ('b t * 'b) g
end

[%%expect{|
module M :
  sig type _ g = G : 'b -> (int * ([< `A of < num : int; .. > ] as 'b)) g end
|}]


(* This leads to an assert with rigid-variants disabled,
   Includecore.type_manifest*)

module M : sig
  type 'b p = private [< `B of int * 'b ]
    constraint 'b = [< `A of < num : int; .. > ]
end = struct
  type 'b p = [ `B of 'b t * 'b ]
end

[%%expect{|
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type 'b p = [ `B of 'b t * 'b ]
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'a p = [ `B of 'a t * 'a ]
             constraint 'a = [< `A of < num : int; .. > ]
         end
       is not included in
         sig
           type 'a p = private [< `B of int * 'a ]
             constraint 'a = [< `A of < num : int; .. > ]
         end
       Type declarations do not match:
         type 'a p = [ `B of 'a t * 'a ]
           constraint 'a = [< `A of < num : int; .. > ]
       is not included in
         type 'a p = private [< `B of int * 'a ]
           constraint 'a = [< `A of < num : int; .. > & < num : int; .. > ]
       The type "([< `A of < num : int; .. > ] as 'a) t * 'a"
       is not equal to the type
         "int * [< `A of < num : int; .. > & < num : int; .. > ]"
       Types for tag "`A" are incompatible
|}]

(* This case of a module not matching it's own signature is due to
   rigid-variants not applying. It would be correct to accept it *)

module M : sig
  type 'b q = private 'b t * 'b
end = struct
  type 'b q = 'b t * 'b
end

[%%expect{|
Line 1:
Error: In module "M":
       Modules do not match:
         sig
           type 'a q = 'a M.q constraint 'a = [< `A of < num : int; .. > ]
         end
       is not included in
         sig
           type 'a q = private 'a t * 'a
             constraint 'a = [< `A of < num : int; .. > ]
         end
       In module "M":
       Type declarations do not match:
         type 'a q = 'a M.q
           constraint 'a = [< `A of < num : int; .. > & < num : int; .. > ]
       is not included in
         type 'a q = private 'a t * 'a
           constraint 'a = [< `A of < num : int; .. > ]
       The type "[< `A of < num : int; .. > & < num : int; .. > ]"
       is not equal to the type "[< `A of < num : int; .. > ]"
       Types for tag "`A" are incompatible
|}]

(* Again, this should be accepted. *)
type +'b vv = V of ('b t * 'b) constraint 'b = [< `A of < num : int; .. > ]

[%%expect{|
Line 1, characters 20-29:
1 | type +'b vv = V of ('b t * 'b) constraint 'b = [< `A of < num : int; .. > ]
                        ^^^^^^^^^
Error: Constraints are not satisfied in this type.
       Type "[< `A of < num : int; .. > & < num : int; .. > ] t"
       should be an instance of "[< `A of < num : int; .. > ] t"
|}]
