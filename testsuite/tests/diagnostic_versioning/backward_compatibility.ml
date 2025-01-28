(* TEST
   flags="-I ${ocamlsrcdir}/utils";
   expect;
*)


open Log
open Diagnostic
module H = Diagnostic_history

module V = H.Make()

let v1_0 = { H.major = 1; minor = 0}
let v1_1 = { H.major = 1; minor=1}
let v1_2 = { H.major = 1; minor=2}
let v2_0 = { H.major = 2; minor=0}

let u1 = V.new_version v1_0
let u1_1 = V.new_version v1_1
let u1_2 = V.new_version v1_2
let u2_0 = V.new_version v2_0
[%%expect {|
module H = Diagnostic_history
module V : Diagnostic_history.S
val v1_0 : H.version = {H.major = 1; minor = 0}
val v1_1 : H.version = {H.major = 1; minor = 1}
val v1_2 : H.version = {H.major = 1; minor = 2}
val v2_0 : H.version = {H.major = 2; minor = 0}
val u1 : V.id Diagnostic_history.update = <abstr>
val u1_1 : V.id Diagnostic_history.update = <abstr>
val u1_2 : V.id Diagnostic_history.update = <abstr>
val u2_0 : V.id Diagnostic_history.update = <abstr>
|}]

module S = New_sum(V)(struct
    let name = "test_record"
    let description = "A sum type"
    let update = u1
  end)()
[%%expect {|
module S :
  sig
    type id
    type 'a constructor
    type t = id Diagnostic.diagnostic
    type raw_type = id Diagnostic.sum
    val scheme : t
    val raw_type : id Diagnostic.sum Diagnostic.typ
    val deprecate :
      V.id Diagnostic.update -> 'a constructor -> 'a constructor
    val delete : V.id Diagnostic.update -> 'a constructor -> 'a constructor
    val seal : V.id Diagnostic.update -> unit
    val app :
      Diagnostic_history.version option -> 'a constructor -> 'a -> raw_type
    val new_constr :
      ?desc:string ->
      V.id Diagnostic.update -> string -> 'a Diagnostic.typ -> 'a constructor
    val new_constr0 :
      ?desc:string -> V.id Diagnostic.update -> string -> unit constructor
    val refine :
      ?desc:string ->
      V.id Diagnostic.update ->
      'a constructor ->
      ('b -> 'a) -> string -> 'b Diagnostic.typ -> 'b constructor
    val expand :
      V.id Diagnostic.update ->
      'a constructor -> ('b -> 'a) -> 'b Diagnostic.typ -> 'b constructor
    val publish : V.id Diagnostic.update -> 'a constructor -> 'a constructor
  end
|}]

let a = S.new_constr0 u1 "A"
let b = S.new_constr0 u1 "B"
let u = S.new_constr0 u1 "U"
let () = S.seal u1
[%%expect {|
val a : unit S.constructor = <abstr>
val b : unit S.constructor = <abstr>
val u : unit S.constructor = <abstr>
|}]


let u = S.delete u1_1 u


type r = {
  maybe: bool;
  possibly: bool;
}
module Inline_b = New_record(V)(struct
  let name = "inline_b"
  let description = "A record under a constructor"
  let update = u1_1
end) ()
let ib_contents = Inline_b.new_field u1_1 "content" Unit
let maybe = Inline_b.new_field u1_1 "maybe" Bool
let possibly = Inline_b.new_field u1_1 "possibly" Bool
let () = S.seal u1_1
type _ extension += BR: r extension
let btyp =
  let pull v x =
    let open Inline_b in
    make v [
      ib_contents ^= ();
      maybe ^= x.maybe;
      possibly ^= x.possibly
    ]
  in
  Custom { id=BR; pull; default = Inline_b.raw_type }

let b = S.expand u1_1 b (fun _x ->()) btyp
let () = S.seal u1_1
[%%expect {|
val u : unit S.constructor = <abstr>
type r = { maybe : bool; possibly : bool; }
module Inline_b :
  sig
    type id
    type nonrec 'a field = ('a, id) Diagnostic.field
    type definition = id Diagnostic.record
    type t = id Diagnostic.diagnostic
    type raw_type = definition
    val scheme : t
    val raw_type : definition Diagnostic.typ
    val deprecate : V.id Diagnostic.update -> 'a field -> 'a field
    val delete : V.id Diagnostic.update -> 'a field -> 'a field
    val seal : V.id Diagnostic.update -> unit
    val new_field :
      ?opt:bool ->
      ?desc:string ->
      V.id Diagnostic.update -> string -> 'a Diagnostic.typ -> 'a field
    val new_field_opt :
      ?desc:string ->
      V.id Diagnostic.update -> string -> 'a Diagnostic.typ -> 'a field
    val make_required : V.id Diagnostic.update -> 'a field -> unit
    type record_fragment
    val make :
      Diagnostic.version option -> record_fragment list -> definition
    val ( ^= ) : 'a field -> 'a -> record_fragment
    val ( ^=? ) : 'a field -> 'a option -> record_fragment
  end
val ib_contents : unit Inline_b.field = <abstr>
val maybe : bool Inline_b.field = <abstr>
val possibly : bool Inline_b.field = <abstr>
type _ Diagnostic.extension += BR : r Diagnostic.extension
val btyp : r Diagnostic.typ =
  Custom {Diagnostic.id = BR; pull = <fun>; default = Record <abstr>}
val b : r S.constructor = <abstr>
|}]

let c =
  S.refine u1_2 b
    (fun maybe -> {maybe;possibly=false})
    "C" Bool
let () = S.seal u1_2

let c = S.publish u2_0 c
let () = S.seal u2_0
[%%expect {|
val c : bool S.constructor = <abstr>
val c : bool S.constructor = <abstr>
|}]

type s =
  | A
  | B of r
  | C of bool


type _ extension += S: s extension

let styp =
  let pull v = function
    | A -> S.app v a ()
    | B r -> S.app v b r
    | C x -> S.app v c x
  in
  Custom { id = S; pull; default = S.raw_type}
[%%expect {|
type s = A | B of r | C of bool
type _ Diagnostic.extension += S : s Diagnostic.extension
val styp : s Diagnostic.typ =
  Custom {Diagnostic.id = S; pull = <fun>; default = Sum <abstr>}
|}]

module R = New_record(V)(struct
    let name = "test_record"
    let description = "Main record"
    let update = u1
  end)()

let s = R.new_field u1 "s" styp
let x = R.new_field u1 "x" Int
let y = R.new_field u1 "y" Bool
let () = R.seal u1

let z = R.new_field u1_1 "z" Int
let x = R.deprecate u1_1 x
let () = R.seal u1_1

let x = R.delete u2_0 x
let () = R.seal u2_0
[%%expect {|
module R :
  sig
    type id
    type nonrec 'a field = ('a, id) Diagnostic.field
    type definition = id Diagnostic.record
    type t = id Diagnostic.diagnostic
    type raw_type = definition
    val scheme : t
    val raw_type : definition Diagnostic.typ
    val deprecate : V.id Diagnostic.update -> 'a field -> 'a field
    val delete : V.id Diagnostic.update -> 'a field -> 'a field
    val seal : V.id Diagnostic.update -> unit
    val new_field :
      ?opt:bool ->
      ?desc:string ->
      V.id Diagnostic.update -> string -> 'a Diagnostic.typ -> 'a field
    val new_field_opt :
      ?desc:string ->
      V.id Diagnostic.update -> string -> 'a Diagnostic.typ -> 'a field
    val make_required : V.id Diagnostic.update -> 'a field -> unit
    type record_fragment
    val make :
      Diagnostic.version option -> record_fragment list -> definition
    val ( ^= ) : 'a field -> 'a -> record_fragment
    val ( ^=? ) : 'a field -> 'a option -> record_fragment
  end
val s : s R.field = <abstr>
val x : int R.field = <abstr>
val y : bool R.field = <abstr>
val z : int R.field = <abstr>
val x : int R.field = <abstr>
val x : int R.field = <abstr>
|}]


let test backend ?v sval =
  let version = match v with
    | None -> Diagnostic_validation.Downward_compatible v2_0
    | Some x -> Diagnostic_validation.Exact x
  in
  let log =
    let open Diagnostic_backends in
    backend.make ~version ~device:Log.Device.std R.scheme
  in
  log.%[s] <- sval;
  log.%[x] <- 0;
  log.%[y] <- false;
  log.%[z] <- 2;
  Log.flush log

let json = Diagnostic_backends.json
let sexp = Diagnostic_backends.sexp
let bval = B { possibly=false; maybe=true};;
let cval = C true
[%%expect {|
val test :
  Diagnostic_backends.t -> ?v:Diagnostic_history.version -> s -> unit = <fun>
val json : Diagnostic_backends.t =
  {Diagnostic_backends.name = "json"; make = <fun>}
val sexp : Diagnostic_backends.t =
  {Diagnostic_backends.name = "sexp"; make = <fun>}
val bval : s = B {maybe = true; possibly = false}
val cval : s = C true
|}]

let rx = version_range x
[%%expect {|
val rx : Diagnostic_history.Lifetime.t =
  {Diagnostic_history.Lifetime.inception = None;
   publication = Some {Diagnostic_history.major = 1; minor = 0};
   expansion = None;
   deprecation = Some {Diagnostic_history.major = 1; minor = 1};
   deletion = Some {Diagnostic_history.major = 2; minor = 0}}
|}]


let xst =
  H.Lifetime.stage_at (Some v1_0) (version_range x),
  H.Lifetime.stage_at (Some v1_1) (version_range x),
  H.Lifetime.stage_at (Some v2_0) (version_range x)
[%%expect {|
val xst : H.Lifetime.point * H.Lifetime.point * H.Lifetime.point =
  (H.Lifetime.Publication, H.Lifetime.Deprecation, H.Lifetime.Deletion)
|}]


let () = test sexp ~v:v2_0 A
[%%expect {|
((metadata ((version (2 0)) (downward_compatible false) (valid Full)))
 (s A) (y false) (z 2))
|}]

let () = test sexp ~v:v2_0 (B { possibly=false; maybe=true})
[%%expect {|
((metadata ((version (2 0)) (downward_compatible false) (valid Full)))
 (s (B ((content 0) (maybe true) (possibly false)))) (y false) (z 2))
|}]

let () = test sexp ~v:v2_0 bval
[%%expect {|
((metadata ((version (2 0)) (downward_compatible false) (valid Full)))
 (s (B ((content 0) (maybe true) (possibly false)))) (y false) (z 2))
|}]

let () = test sexp ~v:v2_0 cval
[%%expect {|
((metadata ((version (2 0)) (downward_compatible false) (valid Full)))
 (s (C true)) (y false) (z 2))
|}]

let () = test json ~v:v1_0 A
[%%expect {|
{
  "metadata" :
    { "version" : [1, 0], "downward_compatible" : false, "valid" : ["Full"]},
  "s" : ["A"],
  "x" : 0,
  "y" : false
}
|}]

let () = test json ~v:v1_1 A
[%%expect {|
{
  "metadata" :
    {
      "version" : [1, 1],
      "downward_compatible" : false,
      "valid" : ["Deprecated"],
      "deprecated_paths" : [["x"]]
    },
  "s" : ["A"],
  "x" : 0,
  "y" : false,
  "z" : 2
}
|}]

let () = test json ~v:v2_0 A
[%%expect {|
{
  "metadata" :
    { "version" : [2, 0], "downward_compatible" : false, "valid" : ["Full"]},
  "s" : ["A"],
  "y" : false,
  "z" : 2
}
|}]

let () = test json ~v:v1_0 bval
[%%expect {|
{
  "metadata" :
    { "version" : [1, 0], "downward_compatible" : false, "valid" : ["Full"]},
  "s" : ["B"],
  "x" : 0,
  "y" : false
}
|}]
let () = test json ~v:v1_1 bval
[%%expect {|
{
  "metadata" :
    {
      "version" : [1, 1],
      "downward_compatible" : false,
      "valid" : ["Deprecated"],
      "deprecated_paths" : [["x"]]
    },
  "s" : ["B", { "content" : 0, "maybe" : true, "possibly" : false}],
  "x" : 0,
  "y" : false,
  "z" : 2
}
|}]
let () = test json ~v:v2_0 bval
[%%expect {|
{
  "metadata" :
    { "version" : [2, 0], "downward_compatible" : false, "valid" : ["Full"]},
  "s" : ["B", { "content" : 0, "maybe" : true, "possibly" : false}],
  "y" : false,
  "z" : 2
}
|}]

let () = test json ~v:v1_0 cval
[%%expect {|
{
  "metadata" :
    { "version" : [1, 0], "downward_compatible" : false, "valid" : ["Full"]},
  "s" : ["B"],
  "x" : 0,
  "y" : false
}
|}]

let () = test json ~v:v1_1 cval
[%%expect {|
{
  "metadata" :
    {
      "version" : [1, 1],
      "downward_compatible" : false,
      "valid" : ["Deprecated"],
      "deprecated_paths" : [["x"]]
    },
  "s" : ["B", { "content" : 0, "maybe" : true, "possibly" : false}],
  "x" : 0,
  "y" : false,
  "z" : 2
}
|}]



let () = test json ~v:v1_2 cval
[%%expect {|
{
  "metadata" :
    { "version" : [1, 2], "downward_compatible" : false, "valid" : ["Full"]},
  "s" : ["C", true],
  "y" : false,
  "z" : 2
}
|}]


let () = test json ~v:v2_0 cval
[%%expect {|
{
  "metadata" :
    { "version" : [2, 0], "downward_compatible" : false, "valid" : ["Full"]},
  "s" : ["C", true],
  "y" : false,
  "z" : 2
}
|}]

let () = test json cval
[%%expect{|
{
  "metadata" :
    { "version" : [2, 0], "downward_compatible" : true, "valid" : ["Full"]},
  "s" :
    ["B",
      {
        "next" : ["C", true],
        "content" : 0,
        "maybe" : true,
        "possibly" : false
      }],
  "x" : 0,
  "y" : false,
  "z" : 2
}
|}]

let () = test sexp bval

[%%expect{|
((metadata ((version (2 0)) (downward_compatible true) (valid Full)))
 (s (B ((content 0) (maybe true) (possibly false)))) (x 0) (y false)
 (z 2))
|}]
