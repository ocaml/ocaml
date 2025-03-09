(* TEST
 flags = "-I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/toplevel";
 include ocamltoplevel;
 expect;
*)
[@@@alert "-deprecated"]

module L = Longident
let mknoloc = Location.mknoloc
let lident l = mknoloc (L.Lident l)

let () =
  Topdirs.dir_install_printer Format.err_formatter
   (L.parse "Location.print_loc")

[%%expect {|
module L = Longident
val mknoloc : 'a -> 'a Location.loc = <fun>
val lident : string -> L.t Location.loc = <fun>
|}]

let flatten_ident = L.flatten (L.Lident "foo")
[%%expect {|
val flatten_ident : string list = ["foo"]
|}]
let flatten_dot = L.flatten (L.Ldot (lident "M", mknoloc "foo"))
[%%expect {|
val flatten_dot : string list = ["M"; "foo"]
|}]
let flatten_apply = L.flatten (L.Lapply (lident "F", lident "X"))
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
  Some
   (L.Ldot
     ({Location.txt =
        L.Ldot ({Location.txt = L.Lident "M"; loc = File "_none_", line 1},
         {Location.txt = "N"; loc = File "_none_", line 1});
       loc = File "_none_", line 1},
     {Location.txt = "foo"; loc = File "_none_", line 1}))
|}]

let last_ident = L.last (L.Lident "foo")
[%%expect {|
val last_ident : string = "foo"
|}]
let last_dot = L.last (L.Ldot (lident "M", mknoloc "foo"))
[%%expect {|
val last_dot : string = "foo"
|}]
let last_apply = L.last (L.Lapply (lident "F", lident "X"))
[%%expect {|
>> Fatal error: Longident.last
Exception: Misc.Fatal_error.
|}]
let last_dot_apply = L.last
    (L.Ldot (mknoloc (L.Lapply (lident "F", lident "X")), mknoloc "foo"))
[%%expect {|
val last_dot_apply : string = "foo"
|}];;

type parse_result = { flat: L.t; spec:L.t; any_is_correct:bool }
let test specialized s =
  let spec = specialized (Lexing.from_string s) in
  { flat = L.parse s;
    spec;
    any_is_correct = Parse.longident (Lexing.from_string s) = spec;
  }

let parse_empty = L.parse ""
let parse_empty_val = Parse.longident (Lexing.from_string "")
[%%expect {|
type parse_result = { flat : L.t; spec : L.t; any_is_correct : bool; }
val test : (Lexing.lexbuf -> L.t) -> string -> parse_result = <fun>
val parse_empty : L.t = L.Lident ""
Exception: Syntaxerr.Error (Syntaxerr.Other Line 1, characters 0-0).
|}]
let parse_ident = test Parse.val_ident "foo"
[%%expect {|
val parse_ident : parse_result =
  {flat = L.Lident "foo"; spec = L.Lident "foo"; any_is_correct = true}
|}]
let parse_dot = test Parse.val_ident "M.foo"
[%%expect {|
val parse_dot : parse_result =
  {flat =
    L.Ldot ({Location.txt = L.Lident "M"; loc = File "_none_", line 1},
     {Location.txt = "foo"; loc = File "_none_", line 1});
   spec =
    L.Ldot ({Location.txt = L.Lident "M"; loc = Line 1, characters 0-1},
     {Location.txt = "foo"; loc = Line 1, characters 2-5});
   any_is_correct = true}
|}]
let parse_path = test Parse.val_ident "M.N.foo"
[%%expect {|
val parse_path : parse_result =
  {flat =
    L.Ldot
     ({Location.txt =
        L.Ldot ({Location.txt = L.Lident "M"; loc = File "_none_", line 1},
         {Location.txt = "N"; loc = File "_none_", line 1});
       loc = File "_none_", line 1},
     {Location.txt = "foo"; loc = File "_none_", line 1});
   spec =
    L.Ldot
     ({Location.txt =
        L.Ldot ({Location.txt = L.Lident "M"; loc = Line 1, characters 0-1},
         {Location.txt = "N"; loc = Line 1, characters 2-3});
       loc = Line 1, characters 0-3},
     {Location.txt = "foo"; loc = Line 1, characters 4-7});
   any_is_correct = true}
|}]
let parse_complex = test  Parse.type_ident "M.F(M.N).N.foo"
(* the result below is a known misbehavior of Longident.parse
   which does not handle applications properly. *)
[%%expect {|
val parse_complex : parse_result =
  {flat =
    L.Ldot
     ({Location.txt =
        L.Ldot
         ({Location.txt =
            L.Ldot
             ({Location.txt =
                L.Ldot
                 ({Location.txt = L.Lident "M"; loc = File "_none_", line 1},
                 {Location.txt = "F(M"; loc = File "_none_", line 1});
               loc = File "_none_", line 1},
             {Location.txt = "N)"; loc = File "_none_", line 1});
           loc = File "_none_", line 1},
         {Location.txt = "N"; loc = File "_none_", line 1});
       loc = File "_none_", line 1},
     {Location.txt = "foo"; loc = File "_none_", line 1});
   spec =
    L.Ldot
     ({Location.txt =
        L.Ldot
         ({Location.txt =
            L.Lapply
             ({Location.txt =
                L.Ldot
                 ({Location.txt = L.Lident "M"; loc = Line 1, characters 0-1},
                 {Location.txt = "F"; loc = Line 1, characters 2-3});
               loc = Line 1, characters 0-3},
             {Location.txt =
               L.Ldot
                ({Location.txt = L.Lident "M"; loc = Line 1, characters 4-5},
                {Location.txt = "N"; loc = Line 1, characters 6-7});
              loc = Line 1, characters 4-7});
           loc = Line 1, characters 0-8},
         {Location.txt = "N"; loc = Line 1, characters 9-10});
       loc = Line 1, characters 0-10},
     {Location.txt = "foo"; loc = Line 1, characters 11-14});
   any_is_correct = true}
|}]

let parse_op = test Parse.val_ident "M.(.%.()<-)"
(* the result below is another known misbehavior of Longident.parse. *)
[%%expect {|
val parse_op : parse_result =
  {flat =
    L.Ldot
     ({Location.txt =
        L.Ldot
         ({Location.txt =
            L.Ldot
             ({Location.txt = L.Lident "M"; loc = File "_none_", line 1},
             {Location.txt = "("; loc = File "_none_", line 1});
           loc = File "_none_", line 1},
         {Location.txt = "%"; loc = File "_none_", line 1});
       loc = File "_none_", line 1},
     {Location.txt = "()<-)"; loc = File "_none_", line 1});
   spec =
    L.Ldot ({Location.txt = L.Lident "M"; loc = Line 1, characters 0-1},
     {Location.txt = ".%.()<-"; loc = Line 1, characters 2-11});
   any_is_correct = true}
|}]


let parse_let_op = test Parse.val_ident "M.(let+*!)"
[%%expect {|
val parse_let_op : parse_result =
  {flat =
    L.Ldot ({Location.txt = L.Lident "M"; loc = File "_none_", line 1},
     {Location.txt = "(let+*!)"; loc = File "_none_", line 1});
   spec =
    L.Ldot ({Location.txt = L.Lident "M"; loc = Line 1, characters 0-1},
     {Location.txt = "let+*!"; loc = Line 1, characters 2-10});
   any_is_correct = true}
|}]

let constr = test Parse.constr_ident "true"
[%%expect{|
val constr : parse_result =
  {flat = L.Lident "true"; spec = L.Lident "true"; any_is_correct = true}
|}]

let prefix_constr = test Parse.constr_ident "A.B.C.(::)"
[%%expect{|
val prefix_constr : parse_result =
  {flat =
    L.Ldot
     ({Location.txt =
        L.Ldot
         ({Location.txt =
            L.Ldot
             ({Location.txt = L.Lident "A"; loc = File "_none_", line 1},
             {Location.txt = "B"; loc = File "_none_", line 1});
           loc = File "_none_", line 1},
         {Location.txt = "C"; loc = File "_none_", line 1});
       loc = File "_none_", line 1},
     {Location.txt = "(::)"; loc = File "_none_", line 1});
   spec =
    L.Ldot
     ({Location.txt =
        L.Ldot
         ({Location.txt =
            L.Ldot
             ({Location.txt = L.Lident "A"; loc = Line 1, characters 0-1},
             {Location.txt = "B"; loc = Line 1, characters 2-3});
           loc = Line 1, characters 0-3},
         {Location.txt = "C"; loc = Line 1, characters 4-5});
       loc = Line 1, characters 0-5},
     {Location.txt = "::"; loc = Line 1, characters 6-10});
   any_is_correct = true}
|}]



let mod_ext = test Parse.extended_module_path "A.F(B.C(X)).G(Y).D"
[%%expect{|
val mod_ext : parse_result =
  {flat =
    L.Ldot
     ({Location.txt =
        L.Ldot
         ({Location.txt =
            L.Ldot
             ({Location.txt =
                L.Ldot
                 ({Location.txt = L.Lident "A"; loc = File "_none_", line 1},
                 {Location.txt = "F(B"; loc = File "_none_", line 1});
               loc = File "_none_", line 1},
             {Location.txt = "C(X))"; loc = File "_none_", line 1});
           loc = File "_none_", line 1},
         {Location.txt = "G(Y)"; loc = File "_none_", line 1});
       loc = File "_none_", line 1},
     {Location.txt = "D"; loc = File "_none_", line 1});
   spec =
    L.Ldot
     ({Location.txt =
        L.Lapply
         ({Location.txt =
            L.Ldot
             ({Location.txt =
                L.Lapply
                 ({Location.txt =
                    L.Ldot
                     ({Location.txt = L.Lident "A";
                       loc = Line 1, characters 0-1},
                     {Location.txt = "F"; loc = Line 1, characters 2-3});
                   loc = Line 1, characters 0-3},
                 {Location.txt =
                   L.Lapply
                    ({Location.txt =
                       L.Ldot
                        ({Location.txt = L.Lident "B";
                          loc = Line 1, characters 4-5},
                        {Location.txt = "C"; loc = Line 1, characters 6-7});
                      loc = Line 1, characters 4-7},
                    {Location.txt = L.Lident "X";
                     loc = Line 1, characters 8-9});
                  loc = Line 1, characters 4-10});
               loc = Line 1, characters 0-11},
             {Location.txt = "G"; loc = Line 1, characters 12-13});
           loc = Line 1, characters 0-13},
         {Location.txt = L.Lident "Y"; loc = Line 1, characters 14-15});
       loc = Line 1, characters 0-16},
     {Location.txt = "D"; loc = Line 1, characters 17-18});
   any_is_correct = true}
|}]


let string_of_longident lid = Format.asprintf "%a" Pprintast.longident lid
[%%expect{|
val string_of_longident : Longident.t -> string = <fun>
|}]
let str_empty   = string_of_longident parse_empty
[%%expect {|
val str_empty : string = ""
|}]
let str_ident   = string_of_longident parse_ident.flat
[%%expect {|
val str_ident : string = "foo"
|}]
let str_dot     = string_of_longident parse_dot.flat
[%%expect {|
val str_dot : string = "M.foo"
|}]
let str_path    = string_of_longident parse_path.flat
[%%expect {|
val str_path : string = "M.N.foo"
|}]


let str_complex = string_of_longident
   (let (&.) p word = L.Ldot(mknoloc p, mknoloc word) in
    L.Lapply(mknoloc (L.Lident "M" &. "F"), mknoloc (L.Lident "M" &. "N")) &. "N" &. "foo")
[%%expect{|
val str_complex : string = "M.F(M.N).N.foo"
|}]
