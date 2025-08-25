(* TEST
  native;
*)

(* This duplicates some of the tests from `include_functor.ml` (using just
   asserts to check the results).  It exists to test `transl_store_structure`
   in `translmod.ml`, which is a part of the typedtree -> lambda code pass
   only exercised by native compilation *)

(* Test 1: Basic usage in structs *)
module type S = sig
  type t
  val x : t
end

module F1 (X : S) = struct
  let y = X.x
end

module M1 = struct
  type t = int
  let x = 5

  include functor F1
end

let () = assert Int.(equal M1.y 5);;

(* Test 5: Include functor in signature *)
module type T = sig
  type s
  val f : s -> bool
end

module type F5 = functor (X : S) -> T with type s = X.t

module type M5_sig = sig
  type t
  val x : t

  include T with type s = t
end

module M5_impl : M5_sig = struct
  type t = int
  type s = t

  let x = 5
  let f s = x = s
end
let () = assert (M5_impl.f M5_impl.x);;

(* Test 9: Nested module names work *)
module type Eq9 = sig
  type t
  val z : t
  val equal : t -> t -> bool
end

module type S9 = sig
  module Foo : Eq9
end

module F9 (X : S9) = struct
  let eq_z = X.Foo.equal X.Foo.z
end

module M9 = struct
  module Foo : Eq9 = struct
    include Int
    let z = 7
  end
  include functor F9
end

let () = assert (M9.eq_z M9.Foo.z);;

module M9' = struct
  module Foo : Eq9 with type t = int = struct
    include Int
    let z = 6
  end
  include functor F9
end

let () = assert (not (M9'.eq_z 5))
let () = assert (M9'.eq_z 6);;


(* Test 11: Include functor should work at the toplevel (and check shadowing). *)
type t = int
let x : t = 3
let x : t = 5
include functor F1

let () = assert (Int.(equal y 5));;

(* Test 12: Check that things get marked used appropriately when they are
   used by include functor.  This code should produce no warning. *)
module M12 : sig val y : int list end = struct
  module Bar = struct
    type t = int
    let x = 5
    let q = "foo"
  end

  module F (G :
    sig
      module T_sub : sig type t val x : t end
                  -> sig type t val x : t end
    end) = struct
    module Foo = G.T_sub(Bar)
    let y = Foo.x
  end

  module T_sub (X : sig type t val x : t end) = struct
    type t = X.t list
    let x = [X.x]
    let z = "something"
  end
  include functor F
end;;
