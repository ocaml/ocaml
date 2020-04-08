(* TEST
   flags = "-I ${ocamlsrcdir}/parsing"
   include ocamlcommon
   * expect
*)

module L = Longident
[%%expect {|
module L = Longident
|}]

let flatten_ident = L.flatten (L.Lident "foo")
[%%expect {|
val flatten_ident : string list = ["foo"]
|}]
let flatten_dot = L.flatten (L.Ldot (L.Lident "M", "foo"))
[%%expect {|
val flatten_dot : string list = ["M"; "foo"]
|}]
let flatten_apply = L.flatten (L.Lapply (L.Lident "F", L.Lident "X"))
[%%expect {|
>> Fatal error: Longident.flat
Exception: Misc.Fatal_error.
|}]

let unflatten_empty = L.unflatten []
[%%expect {|
val unflatten_empty : L.t option = None
|}]
let unflatten_sing = L.unflatten ["foo"]
[%%expect {|
val unflatten_sing : L.t option = Some (L.Lident "foo")
|}]
let unflatten_dot = L.unflatten ["M"; "N"; "foo"]
[%%expect {|
val unflatten_dot : L.t option =
  Some (L.Ldot (L.Ldot (L.Lident "M", "N"), "foo"))
|}]

let last_ident = L.last (L.Lident "foo")
[%%expect {|
val last_ident : string = "foo"
|}]
let last_dot = L.last (L.Ldot (L.Lident "M", "foo"))
[%%expect {|
val last_dot : string = "foo"
|}]
let last_apply = L.last (L.Lapply (L.Lident "F", L.Lident "X"))
[%%expect {|
>> Fatal error: Longident.last
Exception: Misc.Fatal_error.
|}]
let last_dot_apply = L.last
    (L.Ldot (L.Lapply (L.Lident "F", L.Lident "X"), "foo"))
[%%expect {|
val last_dot_apply : string = "foo"
|}];;

let parse_empty = L.parse ""
[%%expect {|
val parse_empty : L.t = L.Lident ""
|}]
let parse_ident = L.parse "foo"
[%%expect {|
val parse_ident : L.t = L.Lident "foo"
|}]
let parse_dot = L.parse "M.foo"
[%%expect {|
val parse_dot : L.t = L.Ldot (L.Lident "M", "foo")
|}]
let parse_path = L.parse "M.N.foo"
[%%expect {|
val parse_path : L.t = L.Ldot (L.Ldot (L.Lident "M", "N"), "foo")
|}]
let parse_complex = L.parse "M.F(M.N).N.foo"
(* the result below is a known misbehavior of Longident.parse
   which does not handle applications properly. Fixing it
   would be nice, but we soo no convenient way to do it without
   introducing unpleasant dependencies. *)
[%%expect {|
val parse_complex : L.t =
  L.Ldot (L.Ldot (L.Ldot (L.Ldot (L.Lident "M", "F(M"), "N)"), "N"), "foo")
|}]

let string_of_longident lid = Format.asprintf "%a" Pprintast.longident lid
[%%expect{|
val string_of_longident : Longident.t -> string = <fun>
|}]
let str_empty   = string_of_longident parse_empty
[%%expect {|
val str_empty : string = ""
|}]
let str_ident   = string_of_longident parse_ident
[%%expect {|
val str_ident : string = "foo"
|}]
let str_dot     = string_of_longident parse_dot
[%%expect {|
val str_dot : string = "M.foo"
|}]
let str_path    = string_of_longident parse_path
[%%expect {|
val str_path : string = "M.N.foo"
|}]


let str_complex = string_of_longident
   (let (&.) p word = L.Ldot(p, word) in
    L.Lapply(L.Lident "M" &. "F", L.Lident "M" &. "N") &. "N" &. "foo")
[%%expect{|
val str_complex : string = "M.F(M.N).N.foo"
|}]
