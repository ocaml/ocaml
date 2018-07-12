(* TEST
   * expect
*)

(* cannot_alias.ml *)
module Termsig = struct
  module Term0 = struct
    module type S = sig
      module Id : sig end
    end
  end
  module Term = struct
    module type S = sig
      module Term0 : Term0.S
      module T = Term0
    end
  end
end;;
[%%expect{|
module Termsig :
  sig
    module Term0 : sig module type S = sig module Id : sig  end end end
    module Term :
      sig module type S = sig module Term0 : Term0.S module T = Term0 end end
  end
|}]

module Make1 (T' : Termsig.Term.S) = struct
  module T = struct
    include T'.T
    let u = 1
  end
end;;
[%%expect{|
Line _, characters 19-33:
  module Make1 (T' : Termsig.Term.S) = struct
                     ^^^^^^^^^^^^^^
Error: This module type contains an alias for Term0.
       Internal aliases are not allowed in functor argument types.
|}]

module Make2 (T' : Termsig.Term.S) = struct
  module T = struct
    include T'.T
    module Id2 = Id
    let u = 1
  end
end;;
[%%expect{|
Line _, characters 19-33:
  module Make2 (T' : Termsig.Term.S) = struct
                     ^^^^^^^^^^^^^^
Error: This module type contains an alias for Term0.
       Internal aliases are not allowed in functor argument types.
|}]

module Make3 (T' : Termsig.Term.S) = struct
  module T = struct
    include T'.T
    module Id2 = Id
    let u = 1
    let u = 1
  end
end;;
[%%expect{|
Line _, characters 19-33:
  module Make3 (T' : Termsig.Term.S) = struct
                     ^^^^^^^^^^^^^^
Error: This module type contains an alias for Term0.
       Internal aliases are not allowed in functor argument types.
|}]

(* cannot_alias2.ml *)
module type S = sig
  module Term0 : sig module Id : sig end end
  module T = Term0
end;;

module Make1 (T' : S)  = struct
  module Id = T'.T.Id
  module Id2 = Id
end;;
[%%expect{|
module type S =
  sig module Term0 : sig module Id : sig  end end module T = Term0 end
Line _, characters 19-20:
  module Make1 (T' : S)  = struct
                     ^
Error: This module type contains an alias for Term0.
       Internal aliases are not allowed in functor argument types.
|}]

module Make2 (T' : S) : sig module Id : sig end module Id2 = Id end
                        with module Id := T'.Term0.Id  = struct
  module Id = T'.T.Id
  module Id2 = Id
end;;
[%%expect{|
Line _, characters 19-20:
  module Make2 (T' : S) : sig module Id : sig end module Id2 = Id end
                     ^
Error: This module type contains an alias for Term0.
       Internal aliases are not allowed in functor argument types.
|}]

module Make3 (T' : S) = struct
  module T = struct
    module Id = T'.T.Id
    module Id2 = Id
    let u = 1
    let u = 1
  end
end;;
[%%expect{|
Line _, characters 19-20:
  module Make3 (T' : S) = struct
                     ^
Error: This module type contains an alias for Term0.
       Internal aliases are not allowed in functor argument types.
|}]

(* unsoundness if Make1 were accepted *)
module M = Make1 (struct module Term0 =
  struct module Id = struct let x = "a" end end module T = Term0 end);;
M.Id.x;;
[%%expect{|
Line _, characters 11-16:
  module M = Make1 (struct module Term0 =
             ^^^^^
Error: Unbound module Make1
|}]
