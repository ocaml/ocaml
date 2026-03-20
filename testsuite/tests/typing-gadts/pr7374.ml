(* TEST
 expect;
*)

type ('a, 'b) eq = Refl : ('a, 'a) eq

module type S = sig
  type 'a t constraint 'a = [`Rec of 'b]
end;;
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
module type S = sig type 'a t constraint 'a = [ `Rec of 'b ] end
|}]

module type Fix = sig
  module X : S
  type t
  val uniq : ('a, [`Rec of 'a] X.t) eq -> ('a, t) eq
end

module Fix (X : S) : Fix with module X = X = struct
  module X = X
  type t = [`Rec of 'a] X.t as 'a
  let uniq : type a . (a, [`Rec of a] X.t) eq -> (a, t) eq =
    fun Refl -> Refl
end;; (* should fail *)
[%%expect{|
module type Fix =
  sig
    module X : S
    type t
    val uniq : ('a, [ `Rec of 'a ] X.t) eq -> ('a, t) eq
  end
Line 11, characters 16-20:
11 |     fun Refl -> Refl
                     ^^^^
Error: The constructor "Refl" has type "(a, a) eq"
       but an expression was expected of type "(a, t) eq"
       Type "a" is not compatible with type
         "t" = "([ `Rec of 'a X.t ] as 'a) X/2.t"
       Line 8, characters 2-14:
         Definition of module "X"
       Line 7, characters 12-13:
         Definition of module "X/2"
|}]

(* Trigger the unsoundness if Fix were definable *)
module Make(Fix : functor (X : S) -> Fix with module X = X) = struct
  module Id = struct
    type 'a t = 'b constraint 'a = [ `Rec of 'b ]
  end
  module Bad = Fix(Id)
  let magic : type a b. a -> b =
    fun x ->
    let Refl = (Bad.uniq Refl : (a,Bad.t) eq) in
    let Refl = (Bad.uniq Refl : (b,Bad.t) eq) in x
end
[%%expect{|
module Make :
  (Fix : (X : S) ->
           sig
             module X :
               sig type 'a t = 'a X.t constraint 'a = [ `Rec of 'b ] end
             type t
             val uniq : ('a, [ `Rec of 'a ] X.t) eq -> ('a, t) eq
           end)
    ->
    sig
      module Id : sig type 'a t = 'b constraint 'a = [ `Rec of 'b ] end
      module Bad :
        sig
          module X :
            sig type 'a t = 'a Id.t constraint 'a = [ `Rec of 'b ] end
          type t = Fix(Id).t
          val uniq : ('a, [ `Rec of 'a ] X.t) eq -> ('a, t) eq
        end
      val magic : 'a -> 'b
    end
|}]

(* addendum: ensure that hidden paths are checked too *)
module F (X : sig type 'a t end) = struct
  open X
  let f : type a b. (a, b t) eq -> (b, a t) eq -> (a, a t t) eq =
    fun Refl Refl -> Refl;;
end;; (* should fail *)
[%%expect{|
Line 4, characters 21-25:
4 |     fun Refl Refl -> Refl;;
                         ^^^^
Error: The constructor "Refl" has type "(a, a) eq"
       but an expression was expected of type "(a, a X.t X.t) eq"
       Type "a" = "b X.t" is not compatible with type "a X.t X.t"
       Type "b" is not compatible with type "a X.t"
|}]
