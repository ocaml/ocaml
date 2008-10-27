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
 * - Aleksey Nogin: extra features and bug fixes.
 * - Christopher Conway: extra feature (-D<uident>=)
 * - Jean-vincent Loddo: definitions inside IFs.
 *)

module Id = struct
  value name = "Camlp4MacroParser";
  value version = Sys.ocaml_version;
end;

(*
Added statements:

  At toplevel (structure item):

     DEFINE <uident>
     DEFINE <uident> = <expression>
     DEFINE <uident> (<parameters>) = <expression>
     IFDEF <uident> THEN <structure_items> [ ELSE <structure_items> ] (END | ENDIF)
     IFNDEF <uident> THEN <structure_items> [ ELSE <structure_items> ] (END | ENDIF)
     INCLUDE <string>

  At toplevel (signature item):

     DEFINE <uident>
     IFDEF <uident> THEN <signature_items> [ ELSE <signature_items> ] (END | ENDIF)
     IFNDEF <uident> THEN <signature_items> [ ELSE <signature_items> ] (END | ENDIF)
     INCLUDE <string>

  In expressions:

     IFDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     IFNDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     DEFINE <lident> = <expression> IN <expression>
     __FILE__
     __LOCATION__

  In patterns:

     IFDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)
     IFNDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)

  As Camlp4 options:

     -D<uident> or -D<uident>=expr   define <uident> with optional value <expr>
     -U<uident>                      undefine it
     -I<dir>                         add <dir> to the search path for INCLUDE'd files

  After having used a DEFINE <uident> followed by "= <expression>", you
  can use it in expressions *and* in patterns. If the expression defining
  the macro cannot be used as a pattern, there is an error message if
  it is used in a pattern.

  You can also define a local macro in an expression usigng the DEFINE ... IN form.
  Note that local macros have lowercase names and can not take parameters.

  If a macro is defined to = NOTHING, and then used as an argument to a function,
  this will be equivalent to function taking one less argument. Similarly,
  passing NOTHING as an argument to a macro is equivalent to "erasing" the
  corresponding parameter from the macro body.

  The toplevel statement INCLUDE <string> can be used to include a
  file containing macro definitions and also any other toplevel items.
  The included files are looked up in directories passed in via the -I
  option, falling back to the current directory.

  The expression __FILE__ returns the current compiled file name.
  The expression __LOCATION__ returns the current location of itself.

*)

open Camlp4;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  type item_or_def 'a =
    [ SdStr of 'a
    | SdDef of string and option (list string * Ast.expr)
    | SdUnd of string
    | SdITE of bool and list (item_or_def 'a) and list (item_or_def 'a)
    | SdLazy of Lazy.t 'a ];

  value rec list_remove x =
    fun
    [ [(y, _) :: l] when y = x -> l
    | [d :: l] -> [d :: list_remove x l]
    | [] -> [] ];

  value defined = ref [];

  value is_defined i = List.mem_assoc i defined.val;

  value bad_patt _loc =
    Loc.raise _loc
      (Failure
         "this macro cannot be used in a pattern (see its definition)");

  value substp _loc env =
    loop where rec loop =
      fun
      [ <:expr< $e1$ $e2$ >> -> <:patt< $loop e1$ $loop e2$ >>
      | <:expr< >> -> <:patt< >>
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
            [ <:rec_binding< $b1$; $b2$ >> -> <:patt< $substbi b1$; $substbi b2$ >>
            | <:rec_binding< $i$ = $e$ >> -> <:patt< $i$ = $loop e$ >>
            | _ -> bad_patt _loc ]
          in <:patt< { $substbi bi$ } >>
      | _ -> bad_patt _loc ];

  class reloc _loc = object
    inherit Ast.map as super;
    method loc _ = _loc;
    (* method _Loc_t _ = _loc; *)
  end;

  class subst _loc env = object
    inherit reloc _loc as super;
    method expr =
      fun
      [ <:expr< $lid:x$ >> | <:expr< $uid:x$ >> as e ->
          try List.assoc x env with
          [ Not_found -> super#expr e ]
      | e -> super#expr e ];

    method patt =
      fun
      [ <:patt< $lid:x$ >> | <:patt< $uid:x$ >> as p ->
         try substp _loc [] (List.assoc x env) with
         [ Not_found -> super#patt p ]
      | p -> super#patt p ];
  end;

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

  value parse_def s =
    match Gram.parse_string expr (Loc.mk "<command line>") s with
    [ <:expr< $uid:n$ >> -> define None n
    | <:expr< $uid:n$ = $e$ >> -> define (Some ([],e)) n
    | _ -> invalid_arg s ];

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

  value parse_include_file rule =
    let dir_ok file dir = Sys.file_exists (dir ^ file) in
    fun file ->
      let file =
        try (List.find (dir_ok file) (include_dirs.val @ ["./"])) ^ file
        with [ Not_found -> file ]
      in
      let ch = open_in file in
      let st = Stream.of_channel ch in
        Gram.parse rule (Loc.mk file) st;

  value rec execute_macro nil cons =
    fun
    [ SdStr i -> i
    | SdDef x eo -> do { define eo x; nil }
    | SdUnd x -> do { undef x; nil }
    | SdITE b l1 l2 -> execute_macro_list nil cons (if b then l1 else l2)
    | SdLazy l -> Lazy.force l ]

  and execute_macro_list nil cons = fun
  [ [] -> nil
  | [hd::tl] -> (* The evaluation order is important here *)
    let il1 = execute_macro nil cons hd in
    let il2 = execute_macro_list nil cons tl in
    cons il1 il2 ]
  ;

  (* Stack of conditionals. *)
  value stack = Stack.create () ;

  (* Make an SdITE value by extracting the result of the test from the stack. *)
  value make_SdITE_result st1 st2 =
   let test = Stack.pop stack in
   SdITE test st1 st2 ;

  type branch = [ Then | Else ];

  (* Execute macro only if it belongs to the currently active branch. *)
  value execute_macro_if_active_branch _loc nil cons branch macro_def =
   let test = Stack.top stack in
   let item =
     if (test && branch=Then) || ((not test) && branch=Else) then
      execute_macro nil cons macro_def
     else (* ignore the macro *)
      nil
   in SdStr(item)
   ;

  EXTEND Gram
    GLOBAL: expr patt str_item sig_item;
    str_item: FIRST
      [ [ x = macro_def ->
            execute_macro <:str_item<>> (fun a b -> <:str_item< $a$; $b$ >>) x ] ]
    ;
    sig_item: FIRST
      [ [ x = macro_def_sig ->
            execute_macro <:sig_item<>> (fun a b -> <:sig_item< $a$; $b$ >>) x ] ]
    ;
    macro_def:
      [ [ "DEFINE"; i = uident; def = opt_macro_value -> SdDef i def
        | "UNDEF";  i = uident -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; st1 = smlist_then; st2 = else_macro_def ->
	    make_SdITE_result st1 st2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; st1 = smlist_then; st2 = else_macro_def ->
	    make_SdITE_result st1 st2
        | "INCLUDE"; fname = STRING ->
            SdLazy (lazy (parse_include_file str_items fname)) ] ]
    ;
    macro_def_sig:
      [ [ "DEFINE"; i = uident -> SdDef i None
        | "UNDEF";  i = uident -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; sg1 = sglist_then; sg2 = else_macro_def_sig ->
            make_SdITE_result sg1 sg2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; sg1 = sglist_then; sg2 = else_macro_def_sig ->
            make_SdITE_result sg1 sg2
        | "INCLUDE"; fname = STRING ->
            SdLazy (lazy (parse_include_file sig_items fname)) ] ]
    ;
    uident_eval_ifdef:
      [ [ i = uident -> Stack.push (is_defined i) stack ]]
    ;
    uident_eval_ifndef:
      [ [ i = uident -> Stack.push (not (is_defined i)) stack ]]
    ;
    else_macro_def:
      [ [ "ELSE"; st = smlist_else; endif -> st
        | endif -> [] ] ]
    ;
    else_macro_def_sig:
      [ [ "ELSE"; st = sglist_else; endif -> st
        | endif -> [] ] ]
    ;
    else_expr:
      [ [ "ELSE"; e = expr; endif -> e
      | endif -> <:expr< () >> ] ]
    ;
    smlist_then:
      [ [ sml = LIST1 [ d = macro_def; semi ->
			  execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a$; $b$ >>) Then d
		      | si = str_item; semi -> SdStr si ] -> sml ] ]
    ;
    smlist_else:
      [ [ sml = LIST1 [ d = macro_def; semi ->
			  execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a$; $b$ >>) Else d
		      | si = str_item; semi -> SdStr si ] -> sml ] ]
    ;
    sglist_then:
      [ [ sgl = LIST1 [ d = macro_def_sig; semi ->
			  execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a$; $b$ >>) Then d
	              | si = sig_item; semi -> SdStr si ] -> sgl ] ]
    ;
    sglist_else:
      [ [ sgl = LIST1 [ d = macro_def_sig; semi ->
			  execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a$; $b$ >>) Else d
	              | si = sig_item; semi -> SdStr si ] -> sgl ] ]
    ;
    endif:
      [ [ "END" -> ()
        | "ENDIF" -> () ] ]
    ;
    opt_macro_value:
      [ [ "("; pl = LIST1 [ x = LIDENT -> x ] SEP ","; ")"; "="; e = expr -> Some (pl, e)
        | "="; e = expr -> Some ([], e)
        | -> None ] ]
    ;
    expr: LEVEL "top"
      [ [ "IFDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
            if is_defined i then e1 else e2
        | "IFNDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
            if is_defined i then e2 else e1
        | "DEFINE"; i = LIDENT; "="; def = expr; "IN"; body = expr ->
            (new subst _loc [(i, def)])#expr body ] ]
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
      [ [ "IFDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; endif ->
            if is_defined i then p1 else p2
        | "IFNDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; endif ->
            if is_defined i then p2 else p1 ] ]
    ;
    uident:
      [ [ i = UIDENT -> i ] ]
    ;
  END;

  Options.add "-D" (Arg.String parse_def)
    "<string> Define for IFDEF instruction.";
  Options.add "-U" (Arg.String undef)
    "<string> Undefine for IFDEF instruction.";
  Options.add "-I" (Arg.String add_include_dir)
    "<string> Add a directory to INCLUDE search path.";

end;

let module M = Register.OCamlSyntaxExtension Id Make in ();

module MakeNothing (AstFilters : Camlp4.Sig.AstFilters) = struct
 open AstFilters;
 open Ast;

 value remove_nothings =
   fun
   [ <:expr< $e$ NOTHING >> | <:expr< fun $ <:patt< NOTHING >> $ -> $e$ >> -> e
   | e -> e];

 register_str_item_filter (Ast.map_expr remove_nothings)#str_item;

end;

let module M = Camlp4.Register.AstFilter Id MakeNothing in ();
