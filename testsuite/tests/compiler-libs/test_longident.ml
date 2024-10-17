(* TEST
 flags = "-I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)
[@@@alert "-deprecated"]

module L = Longident
let mknoloc = Location.mknoloc

[%%expect {|
module L = Longident
val mknoloc : 'a -> 'a Location.loc = <fun>
|}]

let flatten_ident = L.flatten (L.Lident (mknoloc "foo"))
[%%expect {|
val flatten_ident : string list = ["foo"]
|}]
let flatten_dot = L.flatten (L.Ldot (L.Lident(mknoloc "M"), mknoloc "foo"))
[%%expect {|
val flatten_dot : string list = ["M"; "foo"]
|}]
let flatten_apply = L.flatten (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")))
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
val unflatten_sing : L.t option =
  Some
   (L.Lident
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}})
|}]
let unflatten_dot = L.unflatten ["M"; "N"; "foo"]
[%%expect {|
val unflatten_dot : L.t option =
  Some
   (L.Ldot
     (L.Ldot
       (L.Lident
         {Location.txt = "M";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}},
       {Location.txt = "N";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}),
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}}))
|}]

let last_ident = L.last (L.Lident (mknoloc "foo"))
[%%expect {|
val last_ident : string Location.loc =
  {Location.txt = "foo";
   loc =
    {Location.loc_start =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_end =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_ghost = true}}
|}]
let last_dot = L.last (L.Ldot (L.Lident (mknoloc "M"), mknoloc "foo"))
[%%expect {|
val last_dot : string Location.loc =
  {Location.txt = "foo";
   loc =
    {Location.loc_start =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_end =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_ghost = true}}
|}]
let last_apply = L.last (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")))
[%%expect {|
>> Fatal error: Longident.last
Exception: Misc.Fatal_error.
|}]
let last_dot_apply = L.last
    (L.Ldot (L.Lapply (L.Lident (mknoloc "F"), L.Lident (mknoloc "X")), mknoloc "foo"))
[%%expect {|
val last_dot_apply : string Location.loc =
  {Location.txt = "foo";
   loc =
    {Location.loc_start =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_end =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_ghost = true}}
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
val parse_empty : L.t =
  L.Lident
   {Location.txt = "";
    loc =
     {Location.loc_start =
       {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}}
Exception:
Syntaxerr.Error
 (Syntaxerr.Other
   {Location.loc_start =
     {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
    loc_end =
     {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
    loc_ghost = false}).
|}]
let parse_ident = test Parse.val_ident "foo"
[%%expect {|
val parse_ident : parse_result =
  {flat =
    L.Lident
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   spec =
    L.Lident
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 3};
        loc_ghost = false}};
   any_is_correct = true}
|}]
let parse_dot = test Parse.val_ident "M.foo"
[%%expect {|
val parse_dot : parse_result =
  {flat =
    L.Ldot
     (L.Lident
       {Location.txt = "M";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}},
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}});
   spec =
    L.Ldot
     (L.Lident
       {Location.txt = "M";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 1};
          loc_ghost = false}},
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 2};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 5};
        loc_ghost = false}});
   any_is_correct = true}
|}]
let parse_path = test Parse.val_ident "M.N.foo"
[%%expect {|
val parse_path : parse_result =
  {flat =
    L.Ldot
     (L.Ldot
       (L.Lident
         {Location.txt = "M";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}},
       {Location.txt = "N";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}),
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}});
   spec =
    L.Ldot
     (L.Ldot
       (L.Lident
         {Location.txt = "M";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 1};
            loc_ghost = false}},
       {Location.txt = "N";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 2};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 3};
          loc_ghost = false}}),
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 4};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 7};
        loc_ghost = false}});
   any_is_correct = true}
|}]
let parse_complex = test  Parse.type_ident "M.F(M.N).N.foo"
(* the result below is a known misbehavior of Longident.parse
   which does not handle applications properly. *)
[%%expect {|
val parse_complex : parse_result =
  {flat =
    L.Ldot
     (L.Ldot
       (L.Ldot
         (L.Ldot
           (L.Lident
             {Location.txt = "M";
              loc =
               {Location.loc_start =
                 {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                  pos_cnum = -1};
                loc_end =
                 {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                  pos_cnum = -1};
                loc_ghost = true}},
           {Location.txt = "F(M";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}}),
         {Location.txt = "N)";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}}),
       {Location.txt = "N";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}),
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}});
   spec =
    L.Ldot
     (L.Ldot
       (L.Lapply
         (L.Ldot
           (L.Lident
             {Location.txt = "M";
              loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 0};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 1};
                loc_ghost = false}},
           {Location.txt = "F";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                pos_cnum = 2};
              loc_end =
               {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                pos_cnum = 3};
              loc_ghost = false}}),
         L.Ldot
          (L.Lident
            {Location.txt = "M";
             loc =
              {Location.loc_start =
                {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                 pos_cnum = 4};
               loc_end =
                {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                 pos_cnum = 5};
               loc_ghost = false}},
          {Location.txt = "N";
           loc =
            {Location.loc_start =
              {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
               pos_cnum = 6};
             loc_end =
              {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
               pos_cnum = 7};
             loc_ghost = false}})),
       {Location.txt = "N";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 9};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 10};
          loc_ghost = false}}),
     {Location.txt = "foo";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 11};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 14};
        loc_ghost = false}});
   any_is_correct = true}
|}]

let parse_op = test Parse.val_ident "M.(.%.()<-)"
(* the result below is another known misbehavior of Longident.parse. *)
[%%expect {|
val parse_op : parse_result =
  {flat =
    L.Ldot
     (L.Ldot
       (L.Ldot
         (L.Lident
           {Location.txt = "M";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}},
         {Location.txt = "(";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}}),
       {Location.txt = "%";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}),
     {Location.txt = "()<-)";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}});
   spec =
    L.Ldot
     (L.Lident
       {Location.txt = "M";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 1};
          loc_ghost = false}},
     {Location.txt = ".%.()<-";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 2};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 11};
        loc_ghost = false}});
   any_is_correct = true}
|}]


let parse_let_op = test Parse.val_ident "M.(let+*!)"
[%%expect {|
val parse_let_op : parse_result =
  {flat =
    L.Ldot
     (L.Lident
       {Location.txt = "M";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}},
     {Location.txt = "(let+*!)";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}});
   spec =
    L.Ldot
     (L.Lident
       {Location.txt = "M";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 1};
          loc_ghost = false}},
     {Location.txt = "let+*!";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 2};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 10};
        loc_ghost = false}});
   any_is_correct = true}
|}]

let constr = test Parse.constr_ident "true"
[%%expect{|
val constr : parse_result =
  {flat =
    L.Lident
     {Location.txt = "true";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   spec =
    L.Lident
     {Location.txt = "true";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 4};
        loc_ghost = false}};
   any_is_correct = true}
|}]

let prefix_constr = test Parse.constr_ident "A.B.C.(::)"
[%%expect{|
val prefix_constr : parse_result =
  {flat =
    L.Ldot
     (L.Ldot
       (L.Ldot
         (L.Lident
           {Location.txt = "A";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}},
         {Location.txt = "B";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}}),
       {Location.txt = "C";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}),
     {Location.txt = "(::)";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}});
   spec =
    L.Ldot
     (L.Ldot
       (L.Ldot
         (L.Lident
           {Location.txt = "A";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                pos_cnum = 0};
              loc_end =
               {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                pos_cnum = 1};
              loc_ghost = false}},
         {Location.txt = "B";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 2};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 3};
            loc_ghost = false}}),
       {Location.txt = "C";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 4};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 5};
          loc_ghost = false}}),
     {Location.txt = "::";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 6};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 10};
        loc_ghost = false}});
   any_is_correct = true}
|}]



let mod_ext = test Parse.extended_module_path "A.F(B.C(X)).G(Y).D"
[%%expect{|
val mod_ext : parse_result =
  {flat =
    L.Ldot
     (L.Ldot
       (L.Ldot
         (L.Ldot
           (L.Lident
             {Location.txt = "A";
              loc =
               {Location.loc_start =
                 {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                  pos_cnum = -1};
                loc_end =
                 {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                  pos_cnum = -1};
                loc_ghost = true}},
           {Location.txt = "F(B";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}}),
         {Location.txt = "C(X))";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}}),
       {Location.txt = "G(Y)";
        loc =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}),
     {Location.txt = "D";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}});
   spec =
    L.Ldot
     (L.Lapply
       (L.Ldot
         (L.Lapply
           (L.Ldot
             (L.Lident
               {Location.txt = "A";
                loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                    pos_cnum = 0};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                    pos_cnum = 1};
                  loc_ghost = false}},
             {Location.txt = "F";
              loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 2};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 3};
                loc_ghost = false}}),
           L.Lapply
            (L.Ldot
              (L.Lident
                {Location.txt = "B";
                 loc =
                  {Location.loc_start =
                    {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                     pos_cnum = 4};
                   loc_end =
                    {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                     pos_cnum = 5};
                   loc_ghost = false}},
              {Location.txt = "C";
               loc =
                {Location.loc_start =
                  {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                   pos_cnum = 6};
                 loc_end =
                  {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                   pos_cnum = 7};
                 loc_ghost = false}}),
            L.Lident
             {Location.txt = "X";
              loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 8};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 9};
                loc_ghost = false}})),
         {Location.txt = "G";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
              pos_cnum = 12};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
              pos_cnum = 13};
            loc_ghost = false}}),
       L.Lident
        {Location.txt = "Y";
         loc =
          {Location.loc_start =
            {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 14};
           loc_end =
            {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 15};
           loc_ghost = false}}),
     {Location.txt = "D";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 17};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 18};
        loc_ghost = false}});
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
   (let (&.) p word = L.Ldot(p, mknoloc word) in
    L.Lapply(L.Lident (mknoloc "M") &. "F", L.Lident (mknoloc "M") &. "N") &. "N" &. "foo")
[%%expect{|
val str_complex : string = "M.F(M.N).N.foo"
|}]
