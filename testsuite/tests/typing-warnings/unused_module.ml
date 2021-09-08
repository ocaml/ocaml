(* TEST
   flags = " -w +60 "
   * expect
*)
module M : sig end = struct
  module A = struct
    let x = 1
  end
  module type S = sig
    module M : sig end
  end
  module _ (Q : S with module M = A) = struct
    let _ = Q.M.x
  end
end
[%%expect {|
Line 6, characters 4-22:
6 |     module M : sig end
        ^^^^^^^^^^^^^^^^^^
Warning 60 [unused-module]: unused module M.
module M : sig end
|}]

module M : sig end = struct
  module A = struct
    let x = 1
  end
  module type S = sig
    module M : sig end
  end
  module _ (Q : S with module M = A) = struct
    module _ = Q
  end
end
[%%expect {|
Line 6, characters 4-22:
6 |     module M : sig end
        ^^^^^^^^^^^^^^^^^^
Warning 60 [unused-module]: unused module M.
module M : sig end
|}]
