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
