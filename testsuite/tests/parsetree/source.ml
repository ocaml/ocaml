[@@@foo]

let (x[@foo]) : unit [@foo] = ()[@foo]
  [@@foo]

type t =
  | Foo of (t[@foo]) [@foo]
[@@foo]

[@@@foo]


module M = struct
  type t = {
    l : (t [@foo]) [@foo]
  }
    [@@foo]
    [@@foo]

  [@@@foo]
end[@foo]
[@@foo]

module type S = sig

  include (module type of (M[@foo]))[@foo] with type t := M.t[@foo]
    [@@foo]

  [@@@foo]

end[@foo]
[@@foo]

[@@@foo]
type 'a with_default
  =  ?size:int       (** default [42] *)
  -> ?resizable:bool (** default [true] *)
  -> 'a

type obj = <
  meth1 : int -> int;
  (** method 1 *)

  meth2: unit -> float (** method 2 *);
>

type var = [
  | `Foo (** foo *)
  | `Bar of int * string (** bar *)
]

[%%foo let x = 1 in x]
let [%foo 2+1] : [%foo bar.baz] = [%foo "foo"]

[%%foo module M = [%bar] ]
let [%foo let () = () ] : [%foo type t = t ] = [%foo class c = object end]

[%%foo: 'a list]
let [%foo: [`Foo] ] : [%foo: t -> t ] = [%foo: < foo : t > ]

[%%foo? _ ]
[%%foo? Some y when y > 0]
let [%foo? (Bar x | Baz x) ] : [%foo? #bar ] = [%foo? { x }]

[%%foo: module M : [%baz]]
let [%foo: include S with type t = t ]
  : [%foo: val x : t  val y : t]
  = [%foo: type t = t ]
let int_with_custom_modifier =
  1234567890_1234567890_1234567890_1234567890_1234567890z
let float_with_custom_modifier =
  1234567890_1234567890_1234567890_1234567890_1234567890.z

let int32     = 1234l
let int64     = 1234L
let nativeint = 1234n

let hex_without_modifier = 0x32f
let hex_with_modifier    = 0x32g

let float_without_modifer = 1.2e3
let float_with_modifer    = 1.2g
let%foo x = 42
let%foo _ = () and _ = ()
let%foo _ = ()

(* Expressions *)
let () =
  let%foo[@foo] x = 3
  and[@foo] y = 4 in
  (let module%foo[@foo] M = M in ()) ;
  (let open%foo[@foo] M in ()) ;
  (fun%foo[@foo] x -> ()) ;
  (function%foo[@foo] x -> ()) ;
  (try%foo[@foo] () with _ -> ()) ;
  (if%foo[@foo] () then () else ()) ;
  while%foo[@foo] () do () done ;
  for%foo[@foo] x = () to () do () done ;
  assert%foo[@foo] true ;
  lazy%foo[@foo] x ;
  object%foo[@foo] end ;
  begin%foo[@foo] 3 end ;
  new%foo[@foo] x ;

  match%foo[@foo] () with
  (* Pattern expressions *)
  | lazy%foo[@foo] x -> ()
  | exception%foo[@foo] x -> ()

(* Class expressions *)
class x =
  fun[@foo] x ->
  let[@foo] x = 3 in
  object[@foo]
    inherit[@foo] x
    val[@foo] x = 3
    val[@foo] virtual x : t
    val![@foo] mutable x = 3
    method[@foo] x = 3
    method[@foo] virtual x : t
    method![@foo] private x = 3
    initializer[@foo] x
  end

(* Class type expressions *)
class type t =
  object[@foo]
    inherit[@foo] t
    val[@foo] x : t
    val[@foo] mutable x : t
    method[@foo] x : t
    method[@foo] private x : t
    constraint[@foo] t = t'
    [@@@abc]
    [%%id]
    [@@@aaa]
  end

(* Type expressions *)
type t =
  (module%foo[@foo] M)

(* Module expressions *)
module M =
  functor[@foo] (M : S) ->
    (val[@foo] x)
    (struct[@foo] end)
