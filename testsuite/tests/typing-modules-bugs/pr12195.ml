(* TEST
 expect;
*)
module Foo = struct
  type 'a t

  let create : ('a -> unit) -> 'a t =
    fun  _ -> assert false
end

module type Bar = sig
  type s = A

  val created : s Foo.t
end

let baz : (module Bar) =
  (module (struct
    type s = A

    let created = Foo.create (fun _ -> ())
  end))

[%%expect{|
module Foo : sig type 'a t val create : ('a -> unit) -> 'a t end
module type Bar = sig type s = A val created : s Foo.t end
Lines 15-19, characters 11-5:
15 | ...........struct
16 |     type s = A
17 |
18 |     let created = Foo.create (fun _ -> ())
19 |   end..
Error: Signature mismatch:
       Modules do not match:
         sig type s = A val created : '_weak1 Foo.t end
       is not included in
         Bar
       Values do not match:
         val created : '_weak1 Foo.t
       is not included in
         val created : s Foo.t
       The type "'_weak1 Foo.t" is not compatible with the type "s Foo.t"
       The type constructor "s" would escape its scope
|}]
