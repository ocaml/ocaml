(* TEST
 expect;
*)

(* Regression test for https://github.com/oxcaml/oxcaml/pull/4121 *)

module type Destructive_module_subst = sig
  module type S = sig
    module A : sig end
  end

  module rec M : sig
    module A : sig type t end
    include S with module A := A
  end

  and N : sig
    type alias_MAt = M.A.t
  end
end
[%%expect{|
module type Destructive_module_subst =
  sig
    module type S = sig module A : sig end end
    module rec M : sig module A : sig type t end end
    and N : sig type alias_MAt = M.A.t end
  end
|}]

module type Destructive_type_subst = sig
  module type S = sig
    type a
  end

  module rec M : sig
    type a
    include S with type a := a
  end

  and N : sig
    type 'a require_value
    type require_Ma_value = M.a require_value
  end
end
[%%expect{|
module type Destructive_type_subst =
  sig
    module type S = sig type a end
    module rec M : sig type a end
    and N :
      sig type 'a require_value type require_Ma_value = M.a require_value end
  end
|}]

(* Destructive module type substitution *)
module type P = sig
  module type T
  module A:T
end

module rec X: P with module type T := sig type t end = struct
  module A = struct type t end
end
and Y : sig type t = X.A.t end = struct
   type t = X.A.t
end
[%%expect{|
module type P = sig module type T module A : T end
module rec X : sig module A : sig type t end end
and Y : sig type t = X.A.t end
|}]

module type No_false_dangling_reference = sig
  module type S = sig
    module A : sig type t end
    module C = A
  end

  module A2 : sig type t end

  module rec M : sig
    include S with module A := A2
  end
  and N : sig
    type t = M.C.t
  end
end
[%%expect{|
module type No_false_dangling_reference =
  sig
    module type S = sig module A : sig type t end module C = A end
    module A2 : sig type t end
    module rec M : sig module C = A2 end
    and N : sig type t = M.C.t end
  end
|}]
