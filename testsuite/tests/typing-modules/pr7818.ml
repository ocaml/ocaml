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
module Make1 :
  functor
    (T' : sig
            module Term0 : Termsig.Term0.S
            module T : sig module Id : sig  end end
          end) ->
    sig module T : sig module Id : sig  end val u : int end end
|}]

module Make2 (T' : Termsig.Term.S) = struct
  module T = struct
    include T'.T
    module Id2 = Id
    let u = 1
  end
end;;
[%%expect{|
module Make2 :
  functor
    (T' : sig
            module Term0 : Termsig.Term0.S
            module T : sig module Id : sig  end end
          end) ->
    sig
      module T : sig module Id : sig  end module Id2 = Id val u : int end
    end
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
module Make3 :
  functor
    (T' : sig
            module Term0 : Termsig.Term0.S
            module T : sig module Id : sig  end end
          end) ->
    sig
      module T : sig module Id : sig  end module Id2 = Id val u : int end
    end
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
module Make1 :
  functor
    (T' : sig
            module Term0 : sig module Id : sig  end end
            module T : sig module Id : sig  end end
          end) ->
    sig module Id : sig  end module Id2 = Id end
|}]

module Make2 (T' : S) : sig module Id : sig end module Id2 = Id end
                        with module Id := T'.Term0.Id  = struct
  module Id = T'.T.Id
  module Id2 = Id
end;;
[%%expect{|
Line _, characters 57-107:
  .........................................................struct
    module Id = T'.T.Id
    module Id2 = Id
  end..
Error: Signature mismatch:
       Modules do not match:
         sig module Id : sig  end module Id2 = Id end
       is not included in
         sig module Id2 = T'.Term0.Id end
       In module Id2:
       Module T'.Term0.Id cannot be aliased
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
module Make3 :
  functor
    (T' : sig
            module Term0 : sig module Id : sig  end end
            module T : sig module Id : sig  end end
          end) ->
    sig
      module T : sig module Id : sig  end module Id2 = Id val u : int end
    end
|}]

(* unsoundness if Make1 returned an Id.x field *)
module M = Make1 (struct module Term0 =
  struct module Id = struct let x = "a" end end module T = Term0 end);;
M.Id.x;;
[%%expect{|
module M : sig module Id : sig  end module Id2 = Id end
Line _, characters 0-6:
  M.Id.x;;
  ^^^^^^
Error: Unbound value M.Id.x
|}]
