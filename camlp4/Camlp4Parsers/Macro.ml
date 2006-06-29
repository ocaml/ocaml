open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module Id = struct
  value name = "Camlp4Parsers.Macro";
  value version = "$Id$";
end;

(*
Added statements:

  At toplevel (structure item):

     DEFINE <uident>
     DEFINE <uident> = <expression>
     DEFINE <uident> (<parameters>) = <expression>
     IFDEF <uident> THEN <structure_items> (END | ENDIF)
     IFDEF <uident> THEN <structure_items> ELSE <structure_items> (END | ENDIF)
     IFNDEF <uident> THEN <structure_items> (END | ENDIF)
     IFNDEF <uident> THEN <structure_items> ELSE <structure_items> (END | ENDIF)
     INCLUDE <string>

  In expressions:

     IFDEF <uident> THEN <expression> ELSE <expression> (END | ENDIF)
     IFNDEF <uident> THEN <expression> ELSE <expression> (END | ENDIF)
     __FILE__
     __LOCATION__

  In patterns:

     IFDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)
     IFNDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)

  As Camlp4 options:

     -D<uident>                      define <uident>
     -U<uident>                      undefine it
     -I<dir>                         add <dir> to the search path for INCLUDE'd files

  After having used a DEFINE <uident> followed by "= <expression>", you
  can use it in expressions *and* in patterns. If the expression defining
  the macro cannot be used as a pattern, there is an error message if
  it is used in a pattern.



  The toplevel statement INCLUDE <string> can be used to include a
  file containing macro definitions; note that files included in such
  a way can not have any non-macro toplevel items.  The included files
  are looked up in directories passed in via the -I option, falling
  back to the current directory.

  The expression __FILE__ returns the current compiled file name.
  The expression __LOCATION__ returns the current location of itself.

*)

open Camlp4;

module Make (Syntax : Sig.Camlp4Syntax.S) = struct
  open Sig.Camlp4Token;
  include Syntax;

  type item_or_def 'a =
    [ SdStr of 'a
    | SdDef of string and option (list string * Ast.expr)
    | SdUnd of string
    | SdITE of string and list (item_or_def 'a) and list (item_or_def 'a)
    | SdInc of string ];

  value rec list_remove x =
    fun
    [ [(y, _) :: l] when y = x -> l
    | [d :: l] -> [d :: list_remove x l]
    | [] -> [] ];

  value defined = ref [];

  value is_defined i = List.mem_assoc i defined.val;

  class reloc _loc = object
    inherit Ast.map as super;
    method _Loc_t _ = _loc;
  end;

  class subst _loc env = object
    inherit reloc _loc as super;
    method expr =
      fun
      [ <:expr< $lid:x$ >> | <:expr< $uid:x$ >> as e ->
          try List.assoc x env with
          [ Not_found -> e ]
      | e -> super#expr e ];
  end;

  value bad_patt _loc = 
    Loc.raise _loc
      (Failure
         "this macro cannot be used in a pattern (see its definition)");
  value substp _loc env =
    loop where rec loop =
      fun
      [ <:expr< $e1$ $e2$ >> -> <:patt< $loop e1$ $loop e2$ >>
      | <:expr< $lid:x$ >> ->
          try List.assoc x env with
          [ Not_found -> <:patt< $lid:x$ >> ]
      | <:expr< $uid:x$ >> ->
          try List.assoc x env with
          [ Not_found -> <:patt< $uid:x$ >> ]
      | <:expr< $int:x$ >> -> <:patt< $int:x$ >>
      | <:expr< $str:s$ >> -> <:patt< $str:s$ >>
      | <:expr< ($tup:x$) >> -> <:patt< ($tup:loop x$) >>
      | <:expr< $x1$, $x2$ >> -> <:patt< $loop x1$, $loop x2$ >>
      | <:expr< { $bi$ } >> ->
          let rec substbi = fun
            [ <:binding< $b1$; $b2$ >> -> <:patt< $substbi b1$; $substbi b2$ >>
            | <:binding< $p$ = $e$ >> -> <:patt< $p$ = $loop e$ >>
            | _ -> bad_patt _loc ]
          in <:patt< { $substbi bi$ } >>
      | _ -> bad_patt _loc ];

  value incorrect_number loc l1 l2 =
    Loc.raise loc
      (Failure
        (Printf.sprintf "expected %d parameters; found %d"
            (List.length l2) (List.length l1)));

  value define eo x =
    do {
      match eo with
      [ Some ([], e) ->
          EXTEND Gram
            expr: LEVEL "simple"
              [ [ UIDENT $x$ -> (new reloc _loc)#expr e ]]
            ;
            patt: LEVEL "simple"
              [ [ UIDENT $x$ ->
                    let p = substp _loc [] e
                    in (new reloc _loc)#patt p ]]
            ;
          END
      | Some (sl, e) ->
          EXTEND Gram
            expr: LEVEL "apply"
              [ [ UIDENT $x$; param = SELF ->
                    let el =
                      match param with
                      [ <:expr< ($tup:e$) >> -> Ast.list_of_expr e []
                      | e -> [e] ]
                    in
                    if List.length el = List.length sl then
                      let env = List.combine sl el in
                      (new subst _loc env)#expr e
                    else
                      incorrect_number _loc el sl ] ]
            ;
            patt: LEVEL "simple"
              [ [ UIDENT $x$; param = SELF ->
                    let pl =
                      match param with
                      [ <:patt< ($tup:p$) >> -> Ast.list_of_patt p []
                      | p -> [p] ]
                    in
                    if List.length pl = List.length sl then
                      let env = List.combine sl pl in
                      let p = substp _loc env e in
                      (new reloc _loc)#patt p
                    else
                      incorrect_number _loc pl sl ] ]
            ;
          END
      | None -> () ];
      defined.val := [(x, eo) :: defined.val];
    };

  value undef x =
    try
      do {
        let eo = List.assoc x defined.val in
        match eo with
        [ Some ([], _) ->
            do {
              DELETE_RULE Gram expr: UIDENT $x$ END;
              DELETE_RULE Gram patt: UIDENT $x$ END;
            }
        | Some (_, _) ->
            do {
              DELETE_RULE Gram expr: UIDENT $x$; SELF END;
              DELETE_RULE Gram patt: UIDENT $x$; SELF END;
            }
        | None -> () ];
        defined.val := list_remove x defined.val;
      }
    with
    [ Not_found -> () ];

  (* This is a list of directories to search for INCLUDE statements. *)
  value include_dirs = ref [];

  (* Add something to the above, make sure it ends with a slash. *)
  value add_include_dir str =
    if str <> "" then
      let str =
        if String.get str ((String.length str)-1) = '/'
        then str else str ^ "/"
      in include_dirs.val := include_dirs.val @ [str]
    else ();

  value smlist = Gram.Entry.mk "smlist";

  value parse_include_file =
    let dir_ok file dir = Sys.file_exists (dir ^ file) in
    fun file ->
      let file =
        try (List.find (dir_ok file) (include_dirs.val @ ["./"])) ^ file
        with [ Not_found -> file ]
      in
      let ch = open_in file in
      let st = Stream.of_channel ch in
        Gram.parse smlist (Loc.mk file) st;

  value rec execute_macro = fun
  [ SdStr i -> [i]
  | SdDef x eo -> do { define eo x; [] }
  | SdUnd x -> do { undef x; [] }
  | SdITE i l1 l2 ->
    execute_macro_list (if is_defined i then l1 else l2)
  | SdInc f -> execute_macro_list (parse_include_file f) ]

  and execute_macro_list = fun
  [ [] -> []
  | [hd::tl] -> (* The evaluation order is important here *)
    let il1 = execute_macro hd in
    let il2 = execute_macro_list tl in
      il1 @ il2 ]; 

  EXTEND Gram
    GLOBAL: expr patt str_item sig_item smlist;
    str_item: FIRST
      [ [ x = macro_def ->
            match execute_macro x with
            [ [] -> <:str_item<>>
            | [st] -> st
            | [st::stl] ->
                List.fold_right
                  (fun st acc -> <:str_item< $st$; $acc$ >>) stl st ]
      ] ]
    ;
    macro_def:
      [ [ "DEFINE"; i = uident; def = opt_macro_value -> SdDef i def
        | "UNDEF"; i = uident -> SdUnd i
        | "IFDEF"; i = uident; "THEN"; dl = smlist; _ = endif ->
            SdITE i dl []
        | "IFDEF"; i = uident; "THEN"; dl1 = smlist; "ELSE";
          dl2 = smlist; _ = endif ->
            SdITE i dl1 dl2
        | "IFNDEF"; i = uident; "THEN"; dl = smlist; _ = endif ->
            SdITE i [] dl
        | "IFNDEF"; i = uident; "THEN"; dl1 = smlist; "ELSE";
          dl2 = smlist; _ = endif ->
            SdITE i dl2 dl1
        | "INCLUDE"; fname = STRING -> SdInc fname ] ]
    ;
    smlist:
      [ [ sml = LIST1 str_item_or_macro -> sml ] ]
    ;
    endif:
      [ [ "END" -> ()
        | "ENDIF" -> () ] ]
    ;
    str_item_or_macro:
      [ [ d = macro_def -> d
        | si = str_item -> SdStr si ] ]
    ;
    opt_macro_value:
      [ [ "("; pl = LIST1 [ x = LIDENT -> x ] SEP ","; ")"; "="; e = expr -> Some (pl, e)
        | "="; e = expr -> Some ([], e)
        | -> None ] ]
    ;
    expr: LEVEL "top"
      [ [ "IFDEF"; i = uident; "THEN"; e1 = expr; "ELSE"; e2 = expr; _ = endif ->
            if is_defined i then e1 else e2
        | "IFNDEF"; i = uident; "THEN"; e1 = expr; "ELSE"; e2 = expr; _ = endif ->
            if is_defined i then e2 else e1 ] ]
    ;
    expr: LEVEL "simple"
      [ [ LIDENT "__FILE__" -> <:expr< $`str:Loc.file_name _loc$ >>
        | LIDENT "__LOCATION__" ->
            let (a, b, c, d, e, f, g, h) = Loc.to_tuple _loc in
            <:expr< Loc.of_tuple
                ($`str:a$, $`int:b$, $`int:c$, $`int:d$,
                 $`int:e$, $`int:f$, $`int:g$,
                 $if h then <:expr< True >> else <:expr< False >> $) >> ] ]
    ;
    patt:
      [ [ "IFDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; _ = endif ->
            if is_defined i then p1 else p2
        | "IFNDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; _ = endif ->
            if is_defined i then p2 else p1 ] ]
    ;
    uident:
      [ [ i = UIDENT -> i ] ]
    ;
  END;

  Options.add "-D" (Arg.String (define None))
    "<string> Define for IFDEF instruction.";
  Options.add "-U" (Arg.String undef)
    "<string> Undefine for IFDEF instruction.";
  Options.add "-I" (Arg.String add_include_dir)
    "<string> Add a directory to INCLUDE search path.";

end;

let module M = Register.OCamlSyntaxExtension Id Make in ();
