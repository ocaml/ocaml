/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The parser definition */

%{
module Pervasives = Stdlib
(* In 4.08+dev, 'Pervasives' is deprecated in favor of Stdlib. We need
   to disable the deprecation warning not because of any OCaml code
   below, but because Menhir generates code using Pervasives (in the
   interpretation of $symbolstartpos). Yes, this is ugly, but right now
   we don't see an easier way.  *)

open Asttypes
open Longident
open Parsetree
open Ast_helper
open Docstrings
open Docstrings.WithMenhir

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let ghost_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = true;
}

let mktyp ~loc d = Typ.mk ~loc:(make_loc loc) d
let mkpat ~loc d = Pat.mk ~loc:(make_loc loc) d
let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d
let mkmty ~loc ?attrs d = Mty.mk ~loc:(make_loc loc) ?attrs d
let mksig ~loc d = Sig.mk ~loc:(make_loc loc) d
let mkmod ~loc ?attrs d = Mod.mk ~loc:(make_loc loc) ?attrs d
let mkstr ~loc d = Str.mk ~loc:(make_loc loc) d
let mkclass ~loc ?attrs d = Cl.mk ~loc:(make_loc loc) ?attrs d
let mkcty ~loc ?attrs d = Cty.mk ~loc:(make_loc loc) ?attrs d

let mkctf ~loc ?attrs ?docs d =
  Ctf.mk ~loc:(make_loc loc) ?attrs ?docs d
let mkcf ~loc ?attrs ?docs d =
  Cf.mk ~loc:(make_loc loc) ?attrs ?docs d

let mkrhs rhs loc = mkloc rhs (make_loc loc)
let ghrhs rhs loc = mkloc rhs (ghost_loc loc)

let reloc_pat ~loc x = { x with ppat_loc = make_loc loc };;
let reloc_exp ~loc x = { x with pexp_loc = make_loc loc };;
let reloc_typ ~loc x = { x with ptyp_loc = make_loc loc };;

let mkoperator ~loc name =
  mkexp ~loc (Pexp_ident(mkrhs (Lident name) loc))

let mkpatvar ~loc name =
  mkpat ~loc (Ppat_var (mkrhs name loc))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp ~loc d = Exp.mk ~loc:(ghost_loc loc) d
let ghpat ~loc d = Pat.mk ~loc:(ghost_loc loc) d
let ghtyp ~loc d = Typ.mk ~loc:(ghost_loc loc) d
let ghloc ~loc d = { txt = d; loc = ghost_loc loc }
let ghstr ~loc d = Str.mk ~loc:(ghost_loc loc) d
let ghsig ~loc d = Sig.mk ~loc:(ghost_loc loc) d

let mkinfix arg1 op arg2 =
  Pexp_apply(op, [Nolabel, arg1; Nolabel, arg2])

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus ~oploc name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Pconst_integer (n,m)) ->
      Pexp_constant(Pconst_integer(neg_string n,m))
  | ("-" | "-."), Pexp_constant(Pconst_float (f, m)) ->
      Pexp_constant(Pconst_float(neg_string f, m))
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc ("~" ^ name), [Nolabel, arg])

let mkuplus ~oploc name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Pconst_integer _)
  | ("+" | "+."), Pexp_constant(Pconst_float _) -> desc
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc ("~" ^ name), [Nolabel, arg])

(* TODO define an abstraction boundary between locations-as-pairs
   and locations-as-Location.t; it should be clear when we move from
   one world to the other *)

let mkexp_cons_desc consloc args =
  Pexp_construct(mkrhs (Lident "::") consloc, Some args)
let mkexp_cons ~loc consloc args =
  mkexp ~loc (mkexp_cons_desc consloc args)

let mkpat_cons_desc consloc args =
  Ppat_construct(mkrhs (Lident "::") consloc, Some args)
let mkpat_cons ~loc consloc args =
  mkpat ~loc (mkpat_cons_desc consloc args)

let ghexp_cons_desc consloc args =
  Pexp_construct(ghrhs (Lident "::") consloc, Some args)
let ghpat_cons_desc consloc args =
  Ppat_construct(ghrhs (Lident "::") consloc, Some args)

let rec mktailexp nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (Lident "[]") in
      Pexp_construct (nil, None), nilloc
  | e1 :: el ->
      let exp_el, el_loc = mktailexp nilloc el in
      let loc = (e1.pexp_loc.loc_start, snd el_loc) in
      let arg = ghexp ~loc (Pexp_tuple [e1; ghexp ~loc:el_loc exp_el]) in
      ghexp_cons_desc loc arg, loc

let rec mktailpat nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (Lident "[]") in
      Ppat_construct (nil, None), nilloc
  | p1 :: pl ->
      let pat_pl, el_loc = mktailpat nilloc pl in
      let loc = (p1.ppat_loc.loc_start, snd el_loc) in
      let arg = ghpat ~loc (Ppat_tuple [p1; ghpat ~loc:el_loc pat_pl]) in
      ghpat_cons_desc loc arg, loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint ~loc e (t1, t2) =
  match t1, t2 with
  | Some t, None -> ghexp ~loc (Pexp_constraint(e, t))
  | _, Some t -> ghexp ~loc (Pexp_coerce(e, t1, t))
  | None, None -> assert false

let mkexp_opt_constraint ~loc e = function
  | None -> e
  | Some constraint_ -> mkexp_constraint ~loc e constraint_

let mkpat_opt_constraint ~loc p = function
  | None -> p
  | Some typ -> mkpat ~loc (Ppat_constraint(p, typ))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_loc closing_name closing_loc =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(make_loc opening_loc, opening_name,
                                           make_loc closing_loc, closing_name)))

let expecting loc nonterm =
    raise Syntaxerr.(Error(Expecting(make_loc loc, nonterm)))

let not_expecting loc nonterm =
    raise Syntaxerr.(Error(Not_expecting(make_loc loc, nonterm)))

let dotop_fun ~loc dotop =
  (* We could use ghexp here, but sticking to mkexp for parser.mly
     compatibility. TODO improve parser.mly *)
  mkexp ~loc (Pexp_ident (ghloc ~loc dotop))

let array_function ~loc str name =
  ghloc ~loc (Ldot(Lident str,
                   (if !Clflags.unsafe then "unsafe_" ^ name else name)))

let array_get_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "Array" "get"))
let string_get_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "String" "get"))

let array_set_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "Array" "set"))
let string_set_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "String" "set"))

let index_get ~loc get_fun array index =
  let args = [Nolabel, array; Nolabel, index] in
   mkexp ~loc (Pexp_apply(get_fun, args))

let index_set ~loc set_fun array index value =
  let args = [Nolabel, array; Nolabel, index; Nolabel, value] in
   mkexp ~loc (Pexp_apply(set_fun, args))

let array_get ~loc = index_get ~loc (array_get_fun ~loc)
let string_get ~loc = index_get ~loc (string_get_fun ~loc)
let dotop_get ~loc dotop = index_get ~loc (dotop_fun ~loc dotop)

let array_set ~loc = index_set ~loc (array_set_fun ~loc)
let string_set ~loc = index_set ~loc (string_set_fun ~loc)
let dotop_set ~loc dotop = index_set ~loc (dotop_fun ~loc dotop)

let bigarray_function ~loc str name =
  ghloc ~loc (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get ~loc arr arg =
  let mkexp, ghexp = mkexp ~loc, ghexp ~loc in
  let bigarray_function = bigarray_function ~loc in
  let get = if !Clflags.unsafe then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       [Nolabel, arr; Nolabel, c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       [Nolabel, arr; Nolabel, ghexp(Pexp_array coords)]))

let bigarray_set ~loc arr arg newval =
  let mkexp, ghexp = mkexp ~loc, ghexp ~loc in
  let bigarray_function = bigarray_function ~loc in
  let set = if !Clflags.unsafe then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       [Nolabel, arr; Nolabel, c1;
                        Nolabel, c2; Nolabel, newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       [Nolabel, arr; Nolabel, c1;
                        Nolabel, c2; Nolabel, c3; Nolabel, newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       [Nolabel, arr;
                        Nolabel, ghexp(Pexp_array coords);
                        Nolabel, newval]))

let lapply ~loc p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(
                  Syntaxerr.Applicative_path (make_loc loc)))

let exp_of_longident ~loc lid =
  mkexp ~loc (Pexp_ident {lid with txt = Lident(Longident.last lid.txt)})

let exp_of_label ~loc lbl =
  mkexp ~loc (Pexp_ident lbl)

let pat_of_label ~loc lbl =
  mkpat ~loc (Ppat_var lbl)

let mk_newtypes ~loc newtypes exp =
  let mkexp = mkexp ~loc in
  List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
    newtypes exp

let wrap_type_annotation ~loc newtypes core_type body =
  let mkexp, ghtyp = mkexp ~loc, ghtyp ~loc in
  let mk_newtypes = mk_newtypes ~loc in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp = mk_newtypes newtypes exp in
  (exp, ghtyp(Ptyp_poly(newtypes, Typ.varify_constructors newtypes core_type)))

let wrap_exp_attrs ~loc body (ext, attrs) =
  let ghexp = ghexp ~loc in
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs ~loc d attrs =
  wrap_exp_attrs ~loc (mkexp ~loc d) attrs

let wrap_typ_attrs ~loc typ (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let typ = {typ with ptyp_attributes = attrs @ typ.ptyp_attributes} in
  match ext with
  | None -> typ
  | Some id -> ghtyp ~loc (Ptyp_extension (id, PTyp typ))

let wrap_pat_attrs ~loc pat (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let pat = {pat with ppat_attributes = attrs @ pat.ppat_attributes} in
  match ext with
  | None -> pat
  | Some id -> ghpat ~loc (Ppat_extension (id, PPat (pat, None)))

let mkpat_attrs ~loc d attrs =
  wrap_pat_attrs ~loc (mkpat ~loc d) attrs

let wrap_class_attrs ~loc:_ body attrs =
  {body with pcl_attributes = attrs @ body.pcl_attributes}
let wrap_mod_attrs ~loc:_ body attrs =
  {body with pmod_attributes = attrs @ body.pmod_attributes}
let wrap_mty_attrs ~loc:_ body attrs =
  {body with pmty_attributes = attrs @ body.pmty_attributes}

let wrap_str_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghstr ~loc (Pstr_extension ((id, PStr [body]), []))

let wrap_sig_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghsig ~loc (Psig_extension ((id, PSig [body]), []))

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)
let text_def pos = [Ptop_def (Str.text (rhs_text pos))]

let extra_text startpos endpos text items =
  match items with
  | [] ->
      let post = rhs_post_text endpos in
      let post_extras = rhs_post_extra_text endpos in
      text post @ text post_extras
  | _ :: _ ->
      let pre_extras = rhs_pre_extra_text startpos in
      let post_extras = rhs_post_extra_text endpos in
        text pre_extras @ items @ text post_extras

let extra_str p1 p2 items = extra_text p1 p2 Str.text items
let extra_sig p1 p2 items = extra_text p1 p2 Sig.text items
let extra_cstr p1 p2 items = extra_text p1 p2 Cf.text items
let extra_csig p1 p2 items = extra_text p1 p2 Ctf.text  items
let extra_def p1 p2 items =
  extra_text p1 p2 (fun txt -> [Ptop_def (Str.text txt)]) items

let extra_rhs_core_type ct ~pos =
  let docs = rhs_info pos in
  { ct with ptyp_attributes = add_info_attrs docs ct.ptyp_attributes }

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option;
    lbs_loc: Location.t }

let mklb first ~loc (p, e) attrs =
  {
    lb_pattern = p;
    lb_expression = e;
    lb_attributes = attrs;
    lb_docs = symbol_docs_lazy loc;
    lb_text = (if first then empty_text_lazy
               else symbol_text_lazy (fst loc));
    lb_loc = make_loc loc;
  }

let mklbs ~loc ext rf lb =
  {
    lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = ext ;
    lbs_loc = make_loc loc;
  }

let addlb lbs lb =
  { lbs with lbs_bindings = lb :: lbs.lbs_bindings }

let val_of_let_bindings ~loc lbs =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
  let str = mkstr ~loc (Pstr_value(lbs.lbs_rec, List.rev bindings)) in
  match lbs.lbs_extension with
  | None -> str
  | Some id -> ghstr ~loc (Pstr_extension((id, PStr [str]), []))

let expr_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    mkexp_attrs ~loc (Pexp_let(lbs.lbs_rec, List.rev bindings, body))
      (lbs.lbs_extension, [])

let class_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    if lbs.lbs_extension <> None then
      raise Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "extension")));
    mkclass ~loc (Pcl_let (lbs.lbs_rec, List.rev bindings, body))

(* Alternatively, we could keep the generic module type in the Parsetree
   and extract the package type during type-checking. In that case,
   the assertions below should be turned into explicit checks. *)
let package_type_of_module_type pmty =
  let err loc s =
    raise (Syntaxerr.Error (Syntaxerr.Invalid_package_type (loc, s)))
  in
  let map_cstr = function
    | Pwith_type (lid, ptyp) ->
        let loc = ptyp.ptype_loc in
        if ptyp.ptype_params <> [] then
          err loc "parametrized types are not supported";
        if ptyp.ptype_cstrs <> [] then
          err loc "constrained types are not supported";
        if ptyp.ptype_private <> Public then
          err loc "private types are not supported";

        (* restrictions below are checked by the 'with_constraint' rule *)
        assert (ptyp.ptype_kind = Ptype_abstract);
        assert (ptyp.ptype_attributes = []);
        let ty =
          match ptyp.ptype_manifest with
          | Some ty -> ty
          | None -> assert false
        in
        (lid, ty)
    | _ ->
        err pmty.pmty_loc "only 'with type t =' constraints are supported"
  in
  match pmty with
  | {pmty_desc = Pmty_ident lid} -> (lid, [])
  | {pmty_desc = Pmty_with({pmty_desc = Pmty_ident lid}, cstrs)} ->
      (lid, List.map map_cstr cstrs)
  | _ ->
      err pmty.pmty_loc
        "only module type identifier and 'with type' constraints are supported"

let mk_directive_arg ~loc k =
  { pdira_desc = k;
    pdira_loc = make_loc loc;
  }

let mk_directive ~loc name arg =
  Ptop_dir {
      pdir_name = name;
      pdir_arg = arg;
      pdir_loc = make_loc loc;
    }

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string * char option> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <string> DOTOP
%token INHERIT
%token INITIALIZER
%token <string * char option> INT
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token NEW
%token NONREC
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token HASH
%token <string> HASHOP
%token SIG
%token STAR
%token <string * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT
%token <Docstrings.docstring> DOCSTRING

%token EOL

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_HASH
%nonassoc HASH                         /* simple_expr/toplevel_directive */
%left     HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTOP
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%%

/* macros */
%inline extra_str(symb): symb { extra_str $startpos $endpos $1 };
%inline extra_sig(symb): symb { extra_sig $startpos $endpos $1 };
%inline extra_cstr(symb): symb { extra_cstr $startpos $endpos $1 };
%inline extra_csig(symb): symb { extra_csig $startpos $endpos $1 };
%inline extra_def(symb): symb { extra_def $startpos $endpos $1 };
%inline extra_text(symb): symb { extra_text $startpos $endpos $1 };

%inline mkrhs(symb): symb
    {
      (* Semantically we could use $symbolstartpos instead of $startpos
         here, but the code comes from calls to (Parsing.rhs_loc p) for
         some position p, which rather corresponds to
         $startpos, so we kept it for compatibility.

         I do not know if mkrhs is ever used in a situation where $startpos
         and $symbolpos do not coincide.  *)
      mkrhs $1 $loc }
;

%inline op(symb): symb
   { (* see the mkrhs comment above
        for the choice of $startpos over $symbolstartpos *)
     mkoperator ~loc:$loc $1 }

%inline mkloc(symb): symb
    { mkloc $1 (make_loc $sloc) }

%inline mkexp(symb): symb
    { mkexp ~loc:$sloc $1 }
%inline mkpat(symb): symb
    { mkpat ~loc:$sloc $1 }
%inline mktyp(symb): symb
    { mktyp ~loc:$sloc $1 }
%inline mksig(symb): symb
    { mksig ~loc:$sloc $1 }
%inline mkmod(symb): symb
    { mkmod ~loc:$sloc $1 }
%inline mkmty(symb): symb
    { mkmty ~loc:$sloc $1 }
%inline mkcty(symb): symb
    { mkcty ~loc:$sloc $1 }
%inline mkctf(symb): symb
    { mkctf ~loc:$sloc $1 }
%inline mkcf(symb): symb
    { mkcf ~loc:$sloc $1 }
%inline mkclass(symb): symb
    { mkclass ~loc:$sloc $1 }

/* Generic definitions */

(* [reversed_separated_nonempty_list(separator, X)] recognizes a nonempty list
   of [X]s, separated with [separator]s, and produces an OCaml list in reverse
   order -- that is, the last element in the input text appears first in this
   OCaml list. *)

(* [inline_reversed_separated_nonempty_list(separator, X)] is semantically
   equivalent to [reversed_separated_nonempty_list(separator, X)], but is
   marked %inline, which means that the case of a list of length one and
   the case of a list of length more than one will be distinguished at the
   use site, and will give rise there to two productions. This can be used
   to avoid certain conflicts. *)

%inline inline_reversed_separated_nonempty_list(separator, X):
  x = X
    { [ x ] }
| xs = reversed_separated_nonempty_list(separator, X)
  separator
  x = X
    { x :: xs }

reversed_separated_nonempty_list(separator, X):
  xs = inline_reversed_separated_nonempty_list(separator, X)
    { xs }

/* Entry points */

implementation:
    structure EOF                        { $1 }
;

interface:
    signature EOF                        { $1 }
;

toplevel_phrase:
    top_structure SEMISEMI               { Ptop_def ($1) }
  | toplevel_directive SEMISEMI          { $1 }
  | EOF                                  { raise End_of_file }
;
top_structure: extra_str(top_structure_nodoc) { $1 }
top_structure_nodoc:
    seq_expr post_item_attributes
      { text_str $startpos($1) @ [mkstrexp $1 $2] }
  | top_structure_tail_nodoc
      { $1 }
;
top_structure_tail_nodoc:
    /* empty */
      { [] }
  | structure_item top_structure_tail_nodoc
      { text_str $startpos($1) @ $1 :: $2 }
;

use_file:
   extra_def(use_file_body) EOF          { $1 }
;
use_file_body:
    use_file_tail                        { $1 }
  | seq_expr post_item_attributes use_file_tail
      { text_def $startpos($1) @ Ptop_def[mkstrexp $1 $2] :: $3 }
;
use_file_tail:
    /* empty */
      { [] }
  | SEMISEMI use_file_body
      { $2 }
  | structure_item use_file_tail
      { text_def $startpos($1) @ Ptop_def[$1] :: $2 }
  | toplevel_directive use_file_tail
      { mark_rhs_docs $startpos($1) $endpos($1);
        text_def $startpos($1) @ $1 :: $2 }
;

parse_core_type:
    core_type EOF { $1 }
;

parse_expression:
    seq_expr EOF { $1 }
;

parse_pattern:
    pattern EOF { $1 }
;

/* Module expressions */

functor_arg:
    mkrhs(LPAREN RPAREN {"*"})
      { $1, None }
  | LPAREN mkrhs(functor_arg_name) COLON module_type RPAREN
      { $2, Some $4 }
;

functor_arg_name:
    UIDENT     { $1 }
  | UNDERSCORE { "_" }
;

functor_args:
    functor_args functor_arg
      { $2 :: $1 }
  | functor_arg
      { [ $1 ] }
;

module_expr:
  | STRUCT attributes structure END
      { mkmod ~loc:$sloc ~attrs:$2 (Pmod_structure($3)) }
  | STRUCT attributes structure error
      { unclosed "struct" $loc($1) "end" $loc($4) }
  | FUNCTOR attributes functor_args MINUSGREATER module_expr
      { let modexp =
          List.fold_left
            (fun acc (n, t) -> mkmod ~loc:$sloc (Pmod_functor(n, t, acc)))
            $5 $3
        in wrap_mod_attrs ~loc:$sloc modexp $2 }
  | paren_module_expr
      { $1 }
  | module_expr attribute
      { Mod.attr $1 $2 }
  | mkmod(module_expr_)
      { $1 }
;
%inline module_expr_:
  | mkrhs(mod_longident)
    { Pmod_ident $1 }
  | module_expr paren_module_expr
    { Pmod_apply($1, $2) }
  | module_expr LPAREN RPAREN
    { (* TODO review mkmod location *)
      Pmod_apply($1, mkmod ~loc:$sloc (Pmod_structure [])) }
  | extension
    { Pmod_extension $1 }
;

paren_module_expr:
    mkmod(LPAREN module_expr COLON module_type RPAREN
      { Pmod_constraint($2, $4) })
      { $1 }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN module_expr RPAREN
      { $2 (* TODO consider reloc *) }
  | LPAREN module_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN VAL attributes expr RPAREN
      { mkmod ~loc:$sloc ~attrs:$3 (Pmod_unpack $4)}
  | LPAREN VAL attributes expr COLON package_type RPAREN
      { let constr_loc = ($startpos($4), $endpos($6)) in
        mkmod ~loc:$sloc ~attrs:$3
          (Pmod_unpack(
               ghexp ~loc:constr_loc (Pexp_constraint($4, $6)))) }
  | LPAREN VAL attributes expr COLON package_type COLONGREATER package_type
    RPAREN
      { let constr_loc = ($startpos($4), $endpos($8)) in
        mkmod ~loc:$sloc ~attrs:$3
          (Pmod_unpack(
               ghexp ~loc:constr_loc (Pexp_coerce($4, Some $6, $8)))) }
  | LPAREN VAL attributes expr COLONGREATER package_type RPAREN
      { let constr_loc = ($startpos($4), $endpos($6)) in
        mkmod ~loc:$sloc ~attrs:$3
          (Pmod_unpack(
               ghexp ~loc:constr_loc (Pexp_coerce($4, None, $6)))) }
  | LPAREN VAL attributes expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr COLONGREATER error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr error
      { unclosed "(" $loc($1) ")" $loc($5) }
;

structure: extra_str(structure_nodoc) { $1 }
structure_nodoc:
    seq_expr post_item_attributes structure_tail_nodoc
      { mark_rhs_docs $startpos($1) $endpos($2);
        text_str $startpos($1) @ mkstrexp $1 $2 :: $3 }
  | structure_tail_nodoc { $1 }
;
structure_tail_nodoc:
    /* empty */                         { [] }
  | SEMISEMI structure_nodoc            { text_str $startpos($1) @ $2 }
  | structure_item structure_tail_nodoc { text_str $startpos($1) @ $1 :: $2 }
;

structure_item:
    let_bindings
      { val_of_let_bindings ~loc:$sloc $1 }
  | structure_item_with_ext
      { let item, ext = $1 in
        wrap_str_ext ~loc:$loc (mkstr ~loc:$loc item) ext }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mkstr ~loc:$sloc (Pstr_extension ($1, (add_docs_attrs docs $2))) }
  | floating_attribute
      { mkstr ~loc:$sloc (Pstr_attribute $1) }
;
structure_item_with_ext:
  | primitive_declaration
      { let (body, ext) = $1 in (Pstr_primitive body, ext) }
  | value_description
      { let (body, ext) = $1 in (Pstr_primitive body, ext) }
  | type_declarations
      { let (nr, l, ext ) = $1 in (Pstr_type (nr, List.rev l), ext) }
  | str_type_extension
      { let (l, ext) = $1 in (Pstr_typext l, ext) }
  | str_exception_declaration
      { let (l, ext) = $1 in (Pstr_exception l, ext) }
  | module_binding
      { let (body, ext) = $1 in (Pstr_module body, ext) }
  | rec_module_bindings
      { let (l, ext) = $1 in (Pstr_recmodule (List.rev l), ext) }
  | module_type_declaration
      { let (body, ext) = $1 in (Pstr_modtype body, ext) }
  | open_statement
      { let (body, ext) = $1 in (Pstr_open body, ext) }
  | class_declarations
      { let (l, ext) = $1 in (Pstr_class (List.rev l), ext) }
  | class_type_declarations
      { let (l, ext) = $1 in (Pstr_class_type (List.rev l), ext) }
  | str_include_statement
      { let (body, ext) = $1 in (Pstr_include body, ext) }
;

str_include_statement:
    INCLUDE ext_attributes module_expr post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Incl.mk $3 ~attrs:(attrs@$4) ~loc:(make_loc $sloc) ~docs, ext }
;
module_binding_body:
    EQUAL module_expr
      { $2 }
  | mkmod(
      COLON module_type EQUAL module_expr
        { Pmod_constraint($4, $2) }
    | functor_arg module_binding_body
        { Pmod_functor(fst $1, snd $1, $2) }
  ) { $1 }
;
module_binding:
    MODULE ext_attributes mkrhs(UIDENT) module_binding_body post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Mb.mk $3 $4 ~attrs:(attrs@$5) ~loc:(make_loc $sloc) ~docs, ext }
;
rec_module_bindings:
    rec_module_binding
      { let (b, ext) = $1 in ([b], ext) }
  | rec_module_bindings and_module_binding
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
rec_module_binding:
    MODULE ext_attributes REC mkrhs(UIDENT)
    module_binding_body post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Mb.mk $4 $5 ~attrs:(attrs@$6) ~loc:(make_loc $sloc) ~docs, ext }
;
and_module_binding:
    AND attributes mkrhs(UIDENT) module_binding_body post_item_attributes
      { let docs = symbol_docs $sloc in
        let text = symbol_text $symbolstartpos in
        Mb.mk $3 $4 ~attrs:($2@$5) ~loc:(make_loc $sloc) ~text ~docs }
;

/* Module types */

module_type:
  | SIG attributes signature END
      { mkmty ~loc:$sloc ~attrs:$2 (Pmty_signature ($3)) }
  | SIG attributes signature error
      { unclosed "sig" $loc($1) "end" $loc($4) }
  | FUNCTOR attributes functor_args MINUSGREATER module_type
      %prec below_WITH
      { let mty =
          List.fold_left
            (fun acc (n, t) -> mkmty ~loc:$sloc (Pmty_functor(n, t, acc)))
            $5 $3
        in wrap_mty_attrs ~loc:$sloc mty $2 }
  | MODULE TYPE OF attributes module_expr %prec below_LBRACKETAT
      { mkmty ~loc:$sloc ~attrs:$4 (Pmty_typeof $5) }
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | module_type attribute
      { Mty.attr $1 $2 }
  | mkmty(module_type_)
      { $1 }
;
%inline module_type_:
  | mkrhs(mty_longident)
      { Pmty_ident $1 }
  | module_type MINUSGREATER module_type
      %prec below_WITH
      { Pmty_functor(mknoloc "_", Some $1, $3) }
  | module_type WITH with_constraints
      { Pmty_with($1, List.rev $3) }
/*  | LPAREN MODULE mkrhs(mod_longident) RPAREN
      { Pmty_alias $3 } */
  | extension
      { Pmty_extension $1 }
;

signature: extra_sig(signature_nodoc) { $1 }
signature_nodoc:
    /* empty */                    { [] }
  | SEMISEMI signature_nodoc       { text_sig $startpos($1) @ $2 }
  | signature_item signature_nodoc { text_sig $startpos($1) @ $1 :: $2 }
;
signature_item:
  | signature_item_with_ext
      { let item, ext = $1 in
        wrap_sig_ext ~loc:$loc (mksig ~loc:$sloc item) ext }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mksig ~loc:$sloc (Psig_extension ($1, (add_docs_attrs docs $2))) }
  | mksig(floating_attribute
      { Psig_attribute $1 })
      { $1 }
;
signature_item_with_ext:
    value_description
      { let (body, ext) = $1 in (Psig_value body, ext) }
  | primitive_declaration
      { let (body, ext) = $1 in (Psig_value body, ext) }
  | type_declarations
      { let (nr, l, ext) = $1 in (Psig_type (nr, List.rev l), ext) }
  | sig_type_extension
      { let (l, ext) = $1 in (Psig_typext l, ext) }
  | sig_exception_declaration
      { let (l, ext) = $1 in (Psig_exception l, ext) }
  | module_declaration
      { let (body, ext) = $1 in (Psig_module body, ext) }
  | module_alias
      { let (body, ext) = $1 in (Psig_module body, ext) }
  | rec_module_declarations
      { let (l, ext) = $1 in (Psig_recmodule (List.rev l), ext) }
  | module_type_declaration
      { let (body, ext) = $1 in (Psig_modtype body, ext) }
  | open_statement
      { let (body, ext) = $1 in (Psig_open body, ext) }
  | sig_include_statement
      { let (body, ext) = $1 in (Psig_include body, ext) }
  | class_descriptions
      { let (l, ext) = $1 in (Psig_class (List.rev l), ext) }
  | class_type_declarations
      { let (l, ext) = $1 in (Psig_class_type (List.rev l), ext) }
;
open_statement:
  | OPEN override_flag ext_attributes mkrhs(mod_longident) post_item_attributes
      { let (ext, attrs) = $3 in
        let docs = symbol_docs $sloc in
        Opn.mk $4 ~override:$2 ~attrs:(attrs@$5) ~loc:(make_loc $sloc) ~docs
        , ext }
;
sig_include_statement:
    INCLUDE ext_attributes module_type post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Incl.mk $3 ~attrs:(attrs@$4) ~loc:(make_loc $sloc) ~docs, ext }
;
module_declaration_body:
    COLON module_type
      { $2 }
  | mkmty(functor_arg module_declaration_body
      { let (name,typ) = $1 in
        Pmty_functor(name, typ, $2) })
      { $1 }
;
module_declaration:
    MODULE ext_attributes mkrhs(UIDENT)
    module_declaration_body post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Md.mk $3 $4 ~attrs:(attrs@$5) ~loc:(make_loc $sloc) ~docs, ext }
;
%inline module_expr_alias: mkrhs(mod_longident)
  { Mty.alias ~loc:(make_loc $sloc) $1 };
module_alias:
    MODULE ext_attributes mkrhs(UIDENT)
    EQUAL module_expr_alias post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Md.mk $3 $5 ~attrs:(attrs@$6) ~loc:(make_loc $sloc) ~docs, ext }
;
rec_module_declarations:
    rec_module_declaration
      { let (body, ext) = $1 in ([body], ext) }
  | rec_module_declarations and_module_declaration
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
rec_module_declaration:
    MODULE ext_attributes REC mkrhs(UIDENT)
    COLON module_type post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Md.mk $4 $6 ~attrs:(attrs@$7) ~loc:(make_loc $sloc) ~docs, ext }
;
and_module_declaration:
    AND attributes mkrhs(UIDENT) COLON module_type post_item_attributes
      { let docs = symbol_docs $sloc in
        let text = symbol_text $symbolstartpos in
        Md.mk $3 $5 ~attrs:($2@$6) ~loc:(make_loc $sloc) ~text ~docs }
;
module_type_declaration_body:
    /* empty */               { None }
  | EQUAL module_type         { Some $2 }
;
module_type_declaration:
    MODULE TYPE ext_attributes mkrhs(ident) module_type_declaration_body
    post_item_attributes
      { let (ext, attrs) = $3 in
        let docs = symbol_docs $sloc in
        Mtd.mk $4 ?typ:$5 ~attrs:(attrs@$6) ~loc:(make_loc $sloc) ~docs, ext }
;
/* Class expressions */

class_declarations:
    class_declaration
      { let (body, ext) = $1 in ([body], ext) }
  | class_declarations and_class_declaration
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
class_declaration:
    CLASS ext_attributes virtual_flag class_type_parameters mkrhs(LIDENT)
    class_fun_binding post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Ci.mk $5 $6 ~virt:$3 ~params:$4
                    ~attrs:(attrs@$7) ~loc:(make_loc $sloc) ~docs
        , ext }
;
and_class_declaration:
    AND attributes virtual_flag class_type_parameters mkrhs(LIDENT)
    class_fun_binding post_item_attributes
      { let docs = symbol_docs $sloc in
        let text = symbol_text $symbolstartpos in
        Ci.mk $5 $6 ~virt:$3 ~params:$4
                    ~attrs:($2@$7) ~loc:(make_loc $sloc) ~text ~docs }
;
class_fun_binding:
    EQUAL class_expr
      { $2 }
  | mkclass(
      COLON class_type EQUAL class_expr
        { Pcl_constraint($4, $2) }
    | labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in Pcl_fun(l, o, p, $2) }
    ) { $1 }
;
class_type_parameters:
    /*empty*/                                   { [] }
  | LBRACKET type_parameter_list RBRACKET       { List.rev $2 }
;

class_fun_def: mkclass(class_fun_def_desc) { $1 };
class_fun_def_desc:
    labeled_simple_pattern MINUSGREATER class_expr
      { let (l,o,p) = $1 in Pcl_fun(l, o, p, $3) }
  | labeled_simple_pattern class_fun_def
      { let (l,o,p) = $1 in Pcl_fun(l, o, p, $2) }
;
class_expr:
    class_simple_expr
      { $1 }
  | FUN attributes class_fun_def
      { wrap_class_attrs ~loc:$sloc $3 $2 }
  | let_bindings IN class_expr
      { class_of_let_bindings ~loc:$sloc $1 $3 }
  | LET OPEN override_flag attributes mkrhs(mod_longident) IN class_expr
      { mkclass ~loc:$sloc ~attrs:$4 (Pcl_open($3, $5, $7)) }
  | class_expr attribute
      { Cl.attr $1 $2 }
  | mkclass(
      class_simple_expr simple_labeled_expr_list
        { Pcl_apply($1, List.rev $2) }
    | extension
        { Pcl_extension $1 }
    ) { $1 }
;
class_simple_expr:
  | LPAREN class_expr RPAREN
      { $2 }
  | LPAREN class_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | mkclass(
      LBRACKET core_type_comma_list RBRACKET mkrhs(class_longident)
        { Pcl_constr($4, List.rev $2) }
    | mkrhs(class_longident)
        { Pcl_constr($1, []) }
    | OBJECT attributes class_structure error
        { unclosed "object" $loc($1) "end" $loc($4) }
    | LPAREN class_expr COLON class_type RPAREN
        { Pcl_constraint($2, $4) }
    | LPAREN class_expr COLON class_type error
        { unclosed "(" $loc($1) ")" $loc($5) }
    ) { $1 }
  | OBJECT attributes class_structure END
    { mkclass ~loc:$sloc ~attrs:$2 (Pcl_structure $3) }
;
class_structure:
  |  class_self_pattern extra_cstr(class_fields)
       { Cstr.mk $1 (List.rev $2) }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { reloc_pat ~loc:$sloc $2 }
  | mkpat(LPAREN pattern COLON core_type RPAREN
      { Ppat_constraint($2, $4) })
      { $1 }
  | /* empty */
      { ghpat ~loc:$sloc Ppat_any }
;
class_fields:
    /* empty */
      { [] }
  | class_fields class_field
      { $2 :: List.rev (text_cstr $startpos($2)) @ $1 }
;
class_field:
  | INHERIT override_flag attributes class_expr parent_binder
    post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_inherit ($2, $4, $5)) ~attrs:($3@$6) ~docs }
  | VAL value post_item_attributes
      { let v, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_val v) ~attrs:(attrs@$3) ~docs }
  | METHOD method_ post_item_attributes
      { let meth, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_method meth) ~attrs:(attrs@$3) ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_constraint $3) ~attrs:($2@$4) ~docs }
  | INITIALIZER attributes seq_expr post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_initializer $3) ~attrs:($2@$4) ~docs }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_extension $1) ~attrs:$2 ~docs }
  | mkcf(floating_attribute
      { Pcf_attribute $1 })
      { $1 }
;
parent_binder:
    AS mkrhs(LIDENT)
          { Some $2 }
  | /* empty */
          { None }
;
value:
/* TODO: factorize these rules (also with method): */
    override_flag attributes MUTABLE VIRTUAL mkrhs(label) COLON core_type
      { if $1 = Override then syntax_error ();
        ($5, Mutable, Cfk_virtual $7), $2 }
  | override_flag attributes VIRTUAL mutable_flag mkrhs(label) COLON core_type
      { if $1 = Override then syntax_error ();
        ($5, $4, Cfk_virtual $7), $2 }
  | override_flag attributes mutable_flag mkrhs(label) EQUAL seq_expr
      { ($4, $3, Cfk_concrete ($1, $6)), $2 }
  | override_flag attributes mutable_flag mkrhs(label) type_constraint
    EQUAL seq_expr
      { let e = mkexp_constraint ~loc:$sloc $7 $5 in
        ($4, $3, Cfk_concrete ($1, e)), $2
      }
;
method_:
/* TODO: factorize those rules... */
    override_flag attributes PRIVATE VIRTUAL mkrhs(label) COLON poly_type
      { if $1 = Override then syntax_error ();
        ($5, Private, Cfk_virtual $7), $2 }
  | override_flag attributes VIRTUAL private_flag mkrhs(label) COLON poly_type
      { if $1 = Override then syntax_error ();
        ($5, $4, Cfk_virtual $7), $2 }
  | override_flag attributes private_flag mkrhs(label) strict_binding
      { let e = $5 in
        let loc = Location.(e.pexp_loc.loc_start, e.pexp_loc.loc_end) in
        ($4, $3,
        Cfk_concrete ($1, ghexp ~loc (Pexp_poly (e, None)))), $2 }
  | override_flag attributes private_flag mkrhs(label)
    COLON poly_type EQUAL seq_expr
      { let poly_exp =
          let loc = ($startpos($6), $endpos($8)) in
          ghexp ~loc (Pexp_poly($8, Some $6)) in
        ($4, $3, Cfk_concrete ($1, poly_exp)), $2 }
  | override_flag attributes private_flag mkrhs(label) COLON TYPE lident_list
    DOT core_type EQUAL seq_expr
      { let poly_exp_loc = ($startpos($7), $endpos($11)) in
        let poly_exp =
          let exp, poly =
            (* it seems odd to use the global ~loc here while poly_exp_loc
               is tighter, but this is what ocamlyacc does;
               TODO improve parser.mly *)
            wrap_type_annotation ~loc:$sloc $7 $9 $11 in
          ghexp ~loc:poly_exp_loc (Pexp_poly(exp, Some poly)) in
        ($4, $3,
        Cfk_concrete ($1, poly_exp)), $2 }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | mkcty(
      QUESTION LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
        { Pcty_arrow(Optional $2 , $4, $6) }
    | OPTLABEL simple_core_type_or_tuple MINUSGREATER class_type
        { Pcty_arrow(Optional $1, $2, $4) }
    | LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
        { Pcty_arrow(Labelled $1, $3, $5) }
    | simple_core_type_or_tuple MINUSGREATER class_type
        { Pcty_arrow(Nolabel, $1, $3) }
    ) { $1 }
 ;
class_signature:
    mkcty(
      LBRACKET core_type_comma_list RBRACKET mkrhs(clty_longident)
        { Pcty_constr ($4, List.rev $2) }
    | mkrhs(clty_longident)
        { Pcty_constr ($1, []) }
    | extension
        { Pcty_extension $1 }
    ) { $1 }
  | OBJECT attributes class_sig_body END
      { mkcty ~loc:$sloc ~attrs:$2 (Pcty_signature $3) }
  | OBJECT attributes class_sig_body error
      { unclosed "object" $loc($1) "end" $loc($4) }
  | class_signature attribute
      { Cty.attr $1 $2 }
  | LET OPEN override_flag attributes mkrhs(mod_longident) IN class_signature
      { mkcty ~loc:$sloc ~attrs:$4 (Pcty_open($3, $5, $7)) }
;
class_sig_body:
    class_self_type extra_csig(class_sig_fields)
      { Csig.mk $1 (List.rev $2) }
;
class_self_type:
    LPAREN core_type RPAREN
      { $2 }
  | mktyp(/* empty */ { Ptyp_any })
      { $1 }
;
class_sig_fields:
    /* empty */
    { [] }
| class_sig_fields class_sig_field
    { $2 :: List.rev (text_csig $startpos($2)) @ $1 }
;
class_sig_field:
    INHERIT attributes class_signature post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_inherit $3) ~attrs:($2@$4) ~docs }
  | VAL attributes value_type post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_val $3) ~attrs:($2@$4) ~docs }
  | METHOD attributes private_virtual_flags mkrhs(label) COLON poly_type
    post_item_attributes
      { let (p, v) = $3 in
        let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_method ($4, p, v, $6)) ~attrs:($2@$7) ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_constraint $3) ~attrs:($2@$4) ~docs }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_extension $1) ~attrs:$2 ~docs }
  | mkctf(floating_attribute
      { Pctf_attribute $1 })
      { $1 }
;
value_type:
    VIRTUAL mutable_flag mkrhs(label) COLON core_type
      { $3, $2, Virtual, $5 }
  | MUTABLE virtual_flag mkrhs(label) COLON core_type
      { $3, Mutable, $2, $5 }
  | mkrhs(label) COLON core_type
      { $1, Immutable, Concrete, $3 }
;
constrain:
    core_type EQUAL core_type
    { $1, $3, make_loc $sloc }
;
constrain_field:
  core_type EQUAL core_type
    { $1, $3 }
;
class_descriptions:
    class_description
      { let (body, ext) = $1 in ([body],ext) }
  | class_descriptions and_class_description
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
class_description:
    CLASS ext_attributes virtual_flag class_type_parameters mkrhs(LIDENT)
    COLON class_type post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Ci.mk $5 $7 ~virt:$3 ~params:$4
                    ~attrs:(attrs @ $8) ~loc:(make_loc $sloc) ~docs
        , ext }
;
and_class_description:
    AND attributes virtual_flag class_type_parameters mkrhs(LIDENT)
    COLON class_type post_item_attributes
      { let docs = symbol_docs $sloc in
        let text = symbol_text $symbolstartpos in
        Ci.mk $5 $7 ~virt:$3 ~params:$4
                    ~attrs:($2@$8) ~loc:(make_loc $sloc) ~text ~docs }
;
class_type_declarations:
    class_type_declaration
      { let (body, ext) = $1 in ([body],ext) }
  | class_type_declarations and_class_type_declaration
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
class_type_declaration:
    CLASS TYPE ext_attributes virtual_flag class_type_parameters mkrhs(LIDENT)
    EQUAL class_signature post_item_attributes
      { let (ext, attrs) = $3 in
        let docs = symbol_docs $sloc in
        Ci.mk $6 $8 ~virt:$4 ~params:$5
                    ~attrs:(attrs@$9) ~loc:(make_loc $sloc) ~docs
        , ext }
;
and_class_type_declaration:
    AND attributes virtual_flag class_type_parameters mkrhs(LIDENT) EQUAL
    class_signature post_item_attributes
      { let docs = symbol_docs $sloc in
        let text = symbol_text $symbolstartpos in
        Ci.mk $5 $7 ~virt:$3 ~params:$4
                    ~attrs:($2@$8) ~loc:(make_loc $sloc) ~text ~docs }
;

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { $1 }
  | mkexp(expr SEMI seq_expr
    { Pexp_sequence($1, $3) })
    { $1 }
  | expr SEMI PERCENT attr_id seq_expr
    { let seq = mkexp ~loc:$sloc (Pexp_sequence ($1, $5)) in
      let payload = PStr [mkstrexp seq []] in
      mkexp ~loc:$sloc (Pexp_extension ($4, payload)) }
;
labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern opt_default RPAREN
      { (Optional (fst $3), $4, snd $3) }
  | QUESTION label_var
      { (Optional (fst $2), None, snd $2) }
  | OPTLABEL LPAREN let_pattern opt_default RPAREN
      { (Optional $1, $4, $3) }
  | OPTLABEL pattern_var
      { (Optional $1, None, $2) }
  | TILDE LPAREN label_let_pattern RPAREN
      { (Labelled (fst $3), None, snd $3) }
  | TILDE label_var
      { (Labelled (fst $2), None, snd $2) }
  | LABEL simple_pattern
      { (Labelled $1, None, $2) }
  | simple_pattern
      { (Nolabel, None, $1) }
;

pattern_var:
    mkpat(pattern_var_)
      { $1 }
;
pattern_var_:
    mkrhs(LIDENT)     { Ppat_var $1 }
  | UNDERSCORE        { Ppat_any }
;

opt_default:
    /* empty */                         { None }
  | EQUAL seq_expr                      { Some $2 }
;
label_let_pattern:
    label_var
      { $1 }
  | label_var COLON core_type
      { let (lab, pat) = $1 in
        (lab, mkpat ~loc:$sloc (Ppat_constraint(pat, $3))) }
;
label_var:
    mkrhs(LIDENT)
      { ($1.Location.txt, mkpat ~loc:$sloc (Ppat_var $1)) }
;
let_pattern:
    pattern
      { $1 }
  | mkpat(pattern COLON core_type
      { Ppat_constraint($1, $3) })
      { $1 }
;

expr:
    simple_expr %prec below_HASH
      { $1 }
  | expr_attrs
    { let desc, attrs = $1 in
      mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(expr_)
    { $1 }
  | let_bindings IN seq_expr
    { expr_of_let_bindings ~loc:$sloc $1 $3 }
  | expr COLONCOLON expr
      { mkexp_cons ~loc:$sloc $loc($2) (ghexp ~loc:$sloc (Pexp_tuple[$1;$3])) }
  | mkrhs(label) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setinstvar($1, $3)) }
  | simple_expr DOT mkrhs(label_longident) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { array_set ~loc:$sloc $1 $4 $7 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { string_set ~loc:$sloc $1 $4 $7 }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { bigarray_set ~loc:$sloc $1 $4 $7 }
  | simple_expr DOTOP LBRACKET expr RBRACKET LESSMINUS expr
      { dotop_set ~loc:$sloc (Lident ("." ^ $2 ^ "[]<-")) $1 $4 $7 }
  | simple_expr DOTOP LPAREN expr RPAREN LESSMINUS expr
      { dotop_set ~loc:$sloc (Lident ("." ^ $2 ^ "()<-")) $1 $4 $7 }
  | simple_expr DOTOP LBRACE expr RBRACE LESSMINUS expr
      { dotop_set ~loc:$sloc (Lident ("." ^ $2 ^ "{}<-")) $1 $4 $7 }
  | simple_expr DOT mod_longident DOTOP LBRACKET expr RBRACKET LESSMINUS expr
      { dotop_set ~loc:$sloc (Ldot($3,"." ^ $4 ^ "[]<-")) $1 $6 $9 }
  | simple_expr DOT mod_longident DOTOP LPAREN expr RPAREN LESSMINUS expr
      { dotop_set ~loc:$sloc (Ldot($3, "." ^ $4 ^ "()<-")) $1 $6 $9 }
  | simple_expr DOT mod_longident DOTOP LBRACE expr RBRACE LESSMINUS expr
      { dotop_set ~loc:$sloc (Ldot($3, "." ^ $4 ^ "{}<-")) $1 $6 $9 }
  | expr attribute
      { Exp.attr $1 $2 }
  | UNDERSCORE
     { not_expecting $loc($1) "wildcard \"_\"" }
;
%inline expr_attrs:
  | LET MODULE ext_attributes mkrhs(UIDENT) module_binding_body IN seq_expr
      { Pexp_letmodule($4, $5, $7), $3 }
  | LET EXCEPTION ext_attributes let_exception_declaration IN seq_expr
      { Pexp_letexception($4, $6), $3 }
  | LET OPEN override_flag ext_attributes mkrhs(mod_longident) IN seq_expr
      { Pexp_open($3, $5, $7), $4 }
  | FUNCTION ext_attributes opt_bar match_cases
      { Pexp_function(List.rev $4), $2 }
  | FUN ext_attributes labeled_simple_pattern fun_def
      { let (l,o,p) = $3 in
        Pexp_fun(l, o, p, $4), $2 }
  | FUN ext_attributes LPAREN TYPE lident_list RPAREN fun_def
      { (mk_newtypes ~loc:$sloc $5 $7).pexp_desc, $2 }
  | MATCH ext_attributes seq_expr WITH opt_bar match_cases
      { Pexp_match($3, List.rev $6), $2 }
  | TRY ext_attributes seq_expr WITH opt_bar match_cases
      { Pexp_try($3, List.rev $6), $2 }
  | TRY ext_attributes seq_expr WITH error
      { syntax_error() }
  | IF ext_attributes seq_expr THEN expr ELSE expr
      { Pexp_ifthenelse($3, $5, Some $7), $2 }
  | IF ext_attributes seq_expr THEN expr
      { Pexp_ifthenelse($3, $5, None), $2 }
  | WHILE ext_attributes seq_expr DO seq_expr DONE
      { Pexp_while($3, $5), $2 }
  | FOR ext_attributes pattern EQUAL seq_expr direction_flag seq_expr DO
    seq_expr DONE
      { Pexp_for($3, $5, $7, $6, $9), $2 }
  | ASSERT ext_attributes simple_expr %prec below_HASH
      { Pexp_assert $3, $2 }
  | LAZY ext_attributes simple_expr %prec below_HASH
      { Pexp_lazy $3, $2 }
  | OBJECT ext_attributes class_structure END
      { Pexp_object $3, $2 }
  | OBJECT ext_attributes class_structure error
      { unclosed "object" $loc($1) "end" $loc($4) }
;
%inline expr_:
  | expr_op { $1 }
  | simple_expr simple_labeled_expr_list
    { Pexp_apply($1, List.rev $2) }
  | expr_comma_list %prec below_COMMA
    { Pexp_tuple(List.rev $1) }
  | mkrhs(constr_longident) simple_expr %prec below_HASH
    { Pexp_construct($1, Some $2) }
  | name_tag simple_expr %prec below_HASH
    { Pexp_variant($1, Some $2) }
;
%inline expr_op:
  | expr op(INFIXOP0) expr
      { mkinfix $1 $2 $3 }
  | expr op(INFIXOP1) expr
      { mkinfix $1 $2 $3 }
  | expr op(INFIXOP2) expr
      { mkinfix $1 $2 $3 }
  | expr op(INFIXOP3) expr
      { mkinfix $1 $2 $3 }
  | expr op(INFIXOP4) expr
      { mkinfix $1 $2 $3 }
  | expr op(PLUS {"+"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(PLUSDOT {"+."}) expr
      { mkinfix $1 $2 $3 }
  | expr op(PLUSEQ {"+="}) expr
      { mkinfix $1 $2 $3 }
  | expr op(MINUS {"-"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(MINUSDOT {"-."}) expr
      { mkinfix $1 $2 $3 }
  | expr op(STAR {"*"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(PERCENT {"%"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(EQUAL {"="}) expr
      { mkinfix $1 $2 $3 }
  | expr op(LESS {"<"}) expr
    { mkinfix $1 $2 $3 }
  | expr op(GREATER {">"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(OR {"or"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(BARBAR {"||"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(AMPERSAND {"&"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(AMPERAMPER {"&&"}) expr
      { mkinfix $1 $2 $3 }
  | expr op(COLONEQUAL {":="}) expr
      { mkinfix $1 $2 $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus ~oploc:$loc($1) $1 $2 }
  | additive expr %prec prec_unary_plus
      { mkuplus ~oploc:$loc($1) $1 $2 }
;

simple_expr:
  | LPAREN seq_expr RPAREN
      { reloc_exp ~loc:$sloc $2 }
  | LPAREN seq_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN seq_expr type_constraint RPAREN
      { mkexp_constraint ~loc:$sloc $2 $3 }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { array_get ~loc:$sloc $1 $4 }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { string_get ~loc:$sloc $1 $4 }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | simple_expr DOTOP LBRACKET expr RBRACKET
      { dotop_get ~loc:$sloc (Lident ("." ^ $2 ^ "[]")) $1 $4 }
  | simple_expr DOTOP LBRACKET expr error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | simple_expr DOTOP LPAREN expr RPAREN
      { dotop_get ~loc:$sloc (Lident ("." ^ $2 ^ "()")) $1 $4  }
  | simple_expr DOTOP LPAREN expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | simple_expr DOTOP LBRACE expr RBRACE
      { dotop_get ~loc:$sloc (Lident ("." ^ $2 ^ "{}")) $1 $4 }
  | simple_expr DOTOP LBRACE expr error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | simple_expr DOT mod_longident DOTOP LBRACKET expr RBRACKET
      { dotop_get ~loc:$sloc (Ldot($3, "." ^ $4 ^ "[]")) $1 $6  }
  | simple_expr DOT
    mod_longident DOTOP LBRACKET expr error
      { unclosed "[" $loc($5) "]" $loc($7) }
  | simple_expr DOT mod_longident DOTOP LPAREN expr RPAREN
      { dotop_get ~loc:$sloc (Ldot($3, "." ^ $4 ^ "()")) $1 $6 }
  | simple_expr DOT
    mod_longident DOTOP LPAREN expr error
      { unclosed "(" $loc($5) ")" $loc($7) }
  | simple_expr DOT mod_longident DOTOP LBRACE expr RBRACE
      { dotop_get ~loc:$sloc (Ldot($3, "." ^ $4 ^ "{}")) $1 $6  }
  | simple_expr DOT
    mod_longident DOTOP LBRACE expr error
      { unclosed "{" $loc($5) "}" $loc($7) }
  | simple_expr DOT LBRACE expr RBRACE
      { bigarray_get ~loc:$sloc $1 $4 }
  | simple_expr DOT LBRACE expr_comma_list error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | simple_expr_attrs
    { let desc, attrs = $1 in
      mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(simple_expr_)
      { $1 }
;
%inline simple_expr_attrs:
  | BEGIN ext_attributes seq_expr END
      { let (ext, attrs) = $2 in
        $3.pexp_desc, (ext, attrs @ $3.pexp_attributes) }
  | BEGIN ext_attributes END
      { Pexp_construct (mkloc (Lident "()") (make_loc $sloc), None), $2 }
  | BEGIN ext_attributes seq_expr error
      { unclosed "begin" $loc($1) "end" $loc($4) }
  | NEW ext_attributes mkrhs(class_longident)
      { Pexp_new($3), $2 }
  | LPAREN MODULE ext_attributes module_expr RPAREN
      { Pexp_pack $4, $3 }
  | LPAREN MODULE ext_attributes module_expr COLON package_type RPAREN
      { Pexp_constraint (ghexp ~loc:$sloc (Pexp_pack $4), $6), $3 }
  | LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
;
%inline simple_expr_:
  | mkrhs(val_longident)
      { Pexp_ident ($1) }
  | constant
      { Pexp_constant $1 }
  | mkrhs(constr_longident) %prec prec_constant_constructor
      { Pexp_construct($1, None) }
  | name_tag %prec prec_constant_constructor
      { Pexp_variant($1, None) }
  | op(PREFIXOP) simple_expr
      { Pexp_apply($1, [Nolabel,$2]) }
  | op(BANG {"!"}) simple_expr
      { Pexp_apply($1, [Nolabel,$2]) }
  | LBRACELESS field_expr_list GREATERRBRACE
      { Pexp_override $2 }
  | LBRACELESS field_expr_list error
      { unclosed "{<" $loc($1) ">}" $loc($3) }
  | LBRACELESS GREATERRBRACE
      { Pexp_override [] }
  | simple_expr DOT mkrhs(label_longident)
      { Pexp_field($1, $3) }
  | mkrhs(mod_longident) DOT LPAREN seq_expr RPAREN
      { Pexp_open(Fresh, $1, $4) }
  | mkrhs(mod_longident) DOT LBRACELESS field_expr_list GREATERRBRACE
      { (* TODO: review the location of Pexp_override *)
        Pexp_open(Fresh, $1, mkexp ~loc:$sloc (Pexp_override $4)) }
  | mod_longident DOT LBRACELESS field_expr_list error
      { unclosed "{<" $loc($3) ">}" $loc($5) }
  | simple_expr HASH mkrhs(label)
      { Pexp_send($1, $3) }
  | simple_expr op(HASHOP) simple_expr
      { mkinfix $1 $2 $3 }
  | extension
      { Pexp_extension $1 }
  | mkrhs(mod_longident) DOT mkrhs(LPAREN RPAREN {Lident "()"})
      { (* TODO: review the location of Pexp_construct *)
        Pexp_open(Fresh, $1, mkexp ~loc:$sloc (Pexp_construct($3, None))) }
  | mod_longident DOT LPAREN seq_expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | LBRACE record_expr RBRACE
      { let (exten, fields) = $2 in
        Pexp_record(fields, exten) }
  | LBRACE record_expr error
      { unclosed "{" $loc($1) "}" $loc($3) }
  | mkrhs(mod_longident) DOT LBRACE record_expr RBRACE
      { let (exten, fields) = $4 in
        (* TODO: review the location of Pexp_construct *)
        Pexp_open(Fresh, $1, mkexp ~loc:$sloc (Pexp_record(fields, exten))) }
  | mod_longident DOT LBRACE record_expr error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { Pexp_array(List.rev $2) }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" $loc($1) "|]" $loc($4) }
  | LBRACKETBAR BARRBRACKET
      { Pexp_array [] }
  | mkrhs(mod_longident) DOT LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { (* TODO: review the location of Pexp_array *)
        Pexp_open(Fresh, $1, mkexp ~loc:$sloc (Pexp_array(List.rev $4))) }
  | mkrhs(mod_longident) DOT LBRACKETBAR BARRBRACKET
      { (* TODO: review the location of Pexp_array *)
        Pexp_open(Fresh, $1, mkexp ~loc:$sloc (Pexp_array [])) }
  | mod_longident DOT
    LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" $loc($3) "|]" $loc($6) }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { fst (mktailexp $loc($4) (List.rev $2)) }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" $loc($1) "]" $loc($4) }
  | mkrhs(mod_longident) DOT LBRACKET expr_semi_list opt_semi RBRACKET
      { let list_exp =
          (* TODO: review the location of list_exp *)
          let tail_exp, _tail_loc = mktailexp $loc($6) (List.rev $4) in
          mkexp ~loc:$sloc tail_exp in
        Pexp_open(Fresh, $1, list_exp) }
  | mkrhs(mod_longident) DOT mkrhs(LBRACKET RBRACKET {Lident "[]"})
      { (* TODO: review the location of Pexp_construct *)
        Pexp_open(Fresh, $1, mkexp ~loc:$sloc (Pexp_construct($3, None))) }
  | mod_longident DOT
    LBRACKET expr_semi_list opt_semi error
      { unclosed "[" $loc($3) "]" $loc($6) }
  | mkrhs(mod_longident) DOT LPAREN MODULE ext_attributes module_expr COLON
    package_type RPAREN
      { (* TODO: review the location of Pexp_constraint *)
        let modexp =
          mkexp_attrs ~loc:$sloc
            (Pexp_constraint (ghexp ~loc:$sloc (Pexp_pack $6), $8)) $5 in
        Pexp_open(Fresh, $1, modexp) }
  | mod_longident DOT
    LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($3) ")" $loc($8) }
;

simple_labeled_expr_list:
    labeled_simple_expr
      { [$1] }
  | simple_labeled_expr_list labeled_simple_expr
      { $2 :: $1 }
;
labeled_simple_expr:
    simple_expr %prec below_HASH
      { (Nolabel, $1) }
  | label_expr
      { $1 }
;
label_expr:
    LABEL simple_expr %prec below_HASH
      { (Labelled $1, $2) }
  | TILDE label_ident
      { (Labelled (fst $2), snd $2) }
  | QUESTION label_ident
      { (Optional (fst $2), snd $2) }
  | OPTLABEL simple_expr %prec below_HASH
      { (Optional $1, $2) }
;
label_ident:
    LIDENT
      { ($1, mkexp ~loc:$sloc (Pexp_ident(mkrhs (Lident $1) $sloc))) }
;
lident_list:
    mkrhs(LIDENT)                     { [$1] }
  | mkrhs(LIDENT) lident_list         { $1 :: $2 }
;
%inline let_ident:
    val_ident { mkpatvar ~loc:$loc $1 };
let_binding_body:
    let_ident strict_binding
      { ($1, $2) }
  | let_ident type_constraint EQUAL seq_expr
      { let v = $1 in (* PR#7344 *)
        let t =
          match $2 with
            Some t, None -> t
          | _, Some t -> t
          | _ -> assert false
        in
        let loc = Location.(t.ptyp_loc.loc_start, t.ptyp_loc.loc_end) in
        let typ = ghtyp ~loc (Ptyp_poly([],t)) in
        let patloc = ($startpos($1), $endpos($2)) in
        (ghpat ~loc:patloc (Ppat_constraint(v, typ)),
         mkexp_constraint ~loc:$sloc $4 $2) }
  | let_ident COLON typevar_list DOT core_type EQUAL seq_expr
      { let typloc = ($startpos($3), $endpos($5)) in
        let patloc = ($startpos($1), $endpos($5)) in
        (ghpat ~loc:patloc
           (Ppat_constraint($1, ghtyp ~loc:typloc (Ptyp_poly(List.rev $3,$5)))),
         $7) }
  | let_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
      { let exp, poly =
          wrap_type_annotation ~loc:$sloc $4 $6 $8 in
        let loc = ($startpos($1), $endpos($6)) in
        (ghpat ~loc (Ppat_constraint($1, poly)), exp) }
  | pattern_no_exn EQUAL seq_expr
      { ($1, $3) }
  | simple_pattern_not_ident COLON core_type EQUAL seq_expr
      { let loc = ($startpos($1), $endpos($3)) in
        (ghpat ~loc (Ppat_constraint($1, $3)), $5) }
;
let_bindings:
    let_binding                                 { $1 }
  | let_bindings and_let_binding                { addlb $1 $2 }
;
let_binding:
    LET ext_attributes rec_flag let_binding_body post_item_attributes
      { let (ext, attr) = $2 in
        mklbs ~loc:$sloc ext $3 (mklb ~loc:$sloc true $4 (attr@$5)) }
;
and_let_binding:
    AND attributes let_binding_body post_item_attributes
      { mklb ~loc:$sloc false $3 ($2@$4) }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { mkexp_constraint ~loc:$sloc $3 $1 }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | labeled_simple_pattern fun_binding
      { let (l, o, p) = $1 in ghexp ~loc:$sloc (Pexp_fun(l, o, p, $2)) }
  | LPAREN TYPE lident_list RPAREN fun_binding
      { mk_newtypes ~loc:$sloc $3 $5 }
;
match_cases:
    match_case { [$1] }
  | match_cases BAR match_case { $3 :: $1 }
;
match_case:
    pattern MINUSGREATER seq_expr
      { Exp.case $1 $3 }
  | pattern WHEN seq_expr MINUSGREATER seq_expr
      { Exp.case $1 ~guard:$3 $5 }
  | pattern MINUSGREATER DOT
      { Exp.case $1 (Exp.unreachable ~loc:(make_loc $loc($3)) ()) }
;
fun_def:
    MINUSGREATER seq_expr
      { $2 }
  | mkexp(COLON simple_core_type MINUSGREATER seq_expr
      { Pexp_constraint ($4, $2) })
      { $1 }
/* Cf #5939: we used to accept (fun p when e0 -> e) */
  | labeled_simple_pattern fun_def
      {
       let (l,o,p) = $1 in
       ghexp ~loc:$sloc (Pexp_fun(l, o, p, $2))
      }
  | LPAREN TYPE lident_list RPAREN fun_def
      { mk_newtypes ~loc:$sloc $3 $5 }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $3 :: $1 }
  | expr COMMA expr                             { [$3; $1] }
;
record_expr:
    simple_expr WITH lbl_expr_list              { (Some $1, $3) }
  | lbl_expr_list                               { (None, $1) }
;
lbl_expr_list:
     lbl_expr { [$1] }
  |  lbl_expr SEMI lbl_expr_list { $1 :: $3 }
  |  lbl_expr SEMI { [$1] }
;
lbl_expr:
    mkrhs(label_longident) opt_type_constraint EQUAL expr
      { ($1, mkexp_opt_constraint ~loc:$sloc $4 $2) }
  | mkrhs(label_longident) opt_type_constraint
      { ($1,
         mkexp_opt_constraint ~loc:$sloc
           (exp_of_longident ~loc:$sloc $1) $2) }
;
field_expr_list:
    field_expr opt_semi { [$1] }
  | field_expr SEMI field_expr_list { $1 :: $3 }
;
field_expr:
    mkrhs(label) EQUAL expr
      { ($1, $3) }
  | mkrhs(label)
      { ($1, exp_of_label ~loc:$sloc {$1 with txt = Lident $1.txt}) }
;
expr_semi_list:
    expr                                        { [$1] }
  | expr_semi_list SEMI expr                    { $3 :: $1 }
;
type_constraint:
    COLON core_type                             { (Some $2, None) }
  | COLON core_type COLONGREATER core_type      { (Some $2, Some $4) }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;
opt_type_constraint:
    type_constraint { Some $1 }
  | /* empty */ { None }
;

/* Patterns */

pattern:
  | pattern COLONCOLON pattern
      { mkpat_cons ~loc:$sloc $loc($2) (ghpat ~loc:$sloc (Ppat_tuple[$1;$3])) }
  | EXCEPTION ext_attributes pattern %prec prec_constr_appl
      { mkpat_attrs ~loc:$sloc (Ppat_exception $3) $2}
  | pattern attribute
      { Pat.attr $1 $2 }
  | pattern_gen
      { $1 }
  | mkpat(pattern_)
      { $1 }
;
%inline pattern_:
  | pattern AS mkrhs(val_ident)
      { Ppat_alias($1, $3) }
  | pattern AS error
      { expecting $loc($3) "identifier" }
  | pattern_comma_list  %prec below_COMMA
      { Ppat_tuple(List.rev $1) }
  | pattern COLONCOLON error
      { expecting $loc($3) "pattern" }
  | pattern BAR pattern
      { Ppat_or($1, $3) }
  | pattern BAR error
      { expecting $loc($3) "pattern" }
;

pattern_no_exn:
  | pattern_no_exn COLONCOLON pattern
      { mkpat_cons ~loc:$sloc $loc($2) (ghpat ~loc:$sloc (Ppat_tuple[$1;$3])) }
  | pattern_no_exn attribute
      { Pat.attr $1 $2 }
  | pattern_gen
      { $1 }
  | mkpat(pattern_no_exn_)
      { $1 }
;
%inline pattern_no_exn_:
  | pattern_no_exn AS mkrhs(val_ident)
      { Ppat_alias($1, $3) }
  | pattern_no_exn AS error
      { expecting $loc($3) "identifier" }
  | pattern_no_exn_comma_list  %prec below_COMMA
      { Ppat_tuple(List.rev $1) }
  | pattern_no_exn COLONCOLON error
      { expecting $loc($3) "pattern" }
  | pattern_no_exn BAR pattern
      { Ppat_or($1, $3) }
  | pattern_no_exn BAR error
      { expecting $loc($3) "pattern" }
;

pattern_gen:
    simple_pattern
      { $1 }
  | mkpat(
      mkrhs(constr_longident) pattern %prec prec_constr_appl
        { Ppat_construct($1, Some $2) }
    | name_tag pattern %prec prec_constr_appl
        { Ppat_variant($1, Some $2) }
    ) { $1 }
  | LAZY ext_attributes simple_pattern
      { mkpat_attrs ~loc:$sloc (Ppat_lazy $3) $2}
;
simple_pattern:
    mkpat(mkrhs(val_ident) %prec below_EQUAL
      { Ppat_var ($1) })
      { $1 }
  | simple_pattern_not_ident { $1 }
;

simple_pattern_not_ident:
  | LPAREN pattern RPAREN
      { reloc_pat ~loc:$sloc $2 }
  | simple_delimited_pattern
      { $1 }
  | LPAREN MODULE ext_attributes mkrhs(UIDENT) RPAREN
      { mkpat_attrs ~loc:$sloc (Ppat_unpack $4) $3 }
  | LPAREN MODULE ext_attributes mkrhs(UIDENT) COLON package_type RPAREN
      { mkpat_attrs ~loc:$sloc
          (Ppat_constraint(mkpat ~loc:$sloc (Ppat_unpack $4), $6))
          $3 }
  | mkpat(simple_pattern_not_ident_)
      { $1 }
;
%inline simple_pattern_not_ident_:
  | UNDERSCORE
      { Ppat_any }
  | signed_constant
      { Ppat_constant $1 }
  | signed_constant DOTDOT signed_constant
      { Ppat_interval ($1, $3) }
  | mkrhs(constr_longident)
      { Ppat_construct($1, None) }
  | name_tag
      { Ppat_variant($1, None) }
  | HASH mkrhs(type_longident)
      { Ppat_type ($2) }
  | mkrhs(mod_longident) DOT simple_delimited_pattern
      { Ppat_open($1, $3) }
  | mkrhs(mod_longident) DOT mkrhs(LBRACKET RBRACKET {Lident "[]"})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT mkrhs(LPAREN RPAREN {Lident "()"})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT LPAREN pattern RPAREN
      { Ppat_open ($1, $4) }
  | mod_longident DOT LPAREN pattern error
      { unclosed "(" $loc($3) ")" $loc($5)  }
  | mod_longident DOT LPAREN error
      { expecting $loc($4) "pattern" }
  | LPAREN pattern error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN pattern COLON core_type RPAREN
      { Ppat_constraint($2, $4) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN pattern COLON error
      { expecting $loc($4) "type" }
  | LPAREN MODULE ext_attributes UIDENT COLON package_type
    error
      { unclosed "(" $loc($1) ")" $loc($7) }
  | extension
      { Ppat_extension $1 }
;

simple_delimited_pattern:
  mkpat(
      LBRACE lbl_pattern_list RBRACE
      { let (fields, closed) = $2 in
        Ppat_record(fields, closed) }
    | LBRACE lbl_pattern_list error
      { unclosed "{" $loc($1) "}" $loc($3) }
    | LBRACKET pattern_semi_list opt_semi RBRACKET
      { fst (mktailpat $loc($4) (List.rev $2)) }
    | LBRACKET pattern_semi_list opt_semi error
      { unclosed "[" $loc($1) "]" $loc($4) }
    | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { Ppat_array(List.rev $2) }
    | LBRACKETBAR BARRBRACKET
      { Ppat_array [] }
    | LBRACKETBAR pattern_semi_list opt_semi error
      { unclosed "[|" $loc($1) "|]" $loc($4) }
  ) { $1 }

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
  | pattern COMMA error                         { expecting $loc($3) "pattern" }
;
pattern_no_exn_comma_list:
    pattern_no_exn_comma_list COMMA pattern     { $3 :: $1 }
  | pattern_no_exn COMMA pattern                { [$3; $1] }
  | pattern_no_exn COMMA error                  { expecting $loc($3) "pattern" }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    lbl_pattern { [$1], Closed }
  | lbl_pattern SEMI { [$1], Closed }
  | lbl_pattern SEMI UNDERSCORE opt_semi { [$1], Open }
  | lbl_pattern SEMI lbl_pattern_list
      { let (fields, closed) = $3 in $1 :: fields, closed }
;
lbl_pattern:
    mkrhs(label_longident) opt_pattern_type_constraint EQUAL pattern
     { ($1, mkpat_opt_constraint ~loc:$sloc $4 $2) }
  | mkrhs(label_longident) opt_pattern_type_constraint
     { let label = {$1 with txt = Longident.last $1.txt} in
       ($1, mkpat_opt_constraint ~loc:$sloc
              (pat_of_label ~loc:$sloc label) $2) }
;
opt_pattern_type_constraint:
    COLON core_type { Some $2 }
  | /* empty */ { None }
;

/* Value descriptions */

value_description:
    VAL ext_attributes mkrhs(val_ident) COLON core_type post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Val.mk $3 $5 ~attrs:(attrs@$6) ~loc:(make_loc $sloc) ~docs, ext }
;

/* Primitive declarations */

primitive_declaration_body:
    STRING                                      { [fst $1] }
  | STRING primitive_declaration_body           { fst $1 :: $2 }
;
primitive_declaration:
    EXTERNAL ext_attributes mkrhs(val_ident) COLON core_type EQUAL
    primitive_declaration_body post_item_attributes
      { let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        Val.mk $3 $5 ~prim:$7 ~attrs:(attrs@$8) ~loc:(make_loc $sloc) ~docs
        , ext }
;

/* Type declarations */

type_declarations:
    type_declaration
      { let (nonrec_flag, ty, ext) = $1 in (nonrec_flag, [ty], ext) }
  | type_declarations and_type_declaration
      { let (nonrec_flag, tys, ext) = $1 in (nonrec_flag, $2 :: tys, ext) }
;

type_declaration:
    TYPE ext_attributes nonrec_flag optional_type_parameters mkrhs(LIDENT)
    type_kind constraints post_item_attributes
      { let (kind, priv, manifest) = $6 in
        let (ext, attrs) = $2 in
        let docs = symbol_docs $sloc in
        let ty =
          Type.mk $5 ~params:$4 ~cstrs:(List.rev $7) ~kind
            ~priv ?manifest ~attrs:(attrs@$8) ~loc:(make_loc $sloc) ~docs
        in
        ($3, ty, ext) }
;
and_type_declaration:
    AND attributes optional_type_parameters mkrhs(LIDENT) type_kind constraints
    post_item_attributes
      { let (kind, priv, manifest) = $5 in
        let docs = symbol_docs $sloc in
        let text = symbol_text $symbolstartpos in
        Type.mk $4 ~params:$3 ~cstrs:(List.rev $6)
          ~kind ~priv ?manifest
          ~attrs:($2@$7) ~loc:(make_loc $sloc) ~docs ~text }
;
constraints:
        constraints CONSTRAINT constrain        { $3 :: $1 }
      | /* empty */                             { [] }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, Public, None) }
  | EQUAL core_type
      { (Ptype_abstract, Public, Some $2) }
  | EQUAL PRIVATE core_type
      { (Ptype_abstract, Private, Some $3) }
  | EQUAL constructor_declarations
      { (Ptype_variant(List.rev $2), Public, None) }
  | EQUAL PRIVATE constructor_declarations
      { (Ptype_variant(List.rev $3), Private, None) }
  | EQUAL DOTDOT
      { (Ptype_open, Public, None) }
  | EQUAL PRIVATE DOTDOT
      { (Ptype_open, Private, None) }
  | EQUAL private_flag LBRACE label_declarations RBRACE
      { (Ptype_record $4, $2, None) }
  | EQUAL core_type EQUAL private_flag constructor_declarations
      { (Ptype_variant(List.rev $5), $4, Some $2) }
  | EQUAL core_type EQUAL private_flag DOTDOT
      { (Ptype_open, $4, Some $2) }
  | EQUAL core_type EQUAL private_flag LBRACE label_declarations RBRACE
      { (Ptype_record $6, $4, Some $2) }
;
optional_type_parameters:
    /*empty*/                                   { [] }
  | optional_type_parameter                     { [$1] }
  | LPAREN optional_type_parameter_list RPAREN  { List.rev $2 }
;
optional_type_parameter:
    type_variance optional_type_variable        { $2, $1 }
;
optional_type_parameter_list:
    optional_type_parameter                              { [$1] }
  | optional_type_parameter_list COMMA optional_type_parameter    { $3 :: $1 }
;
optional_type_variable:
    mktyp(
        QUOTE ident { Ptyp_var $2 }
      | UNDERSCORE  { Ptyp_any }
    ) { $1 }
;

type_parameter:
    type_variance type_variable                   { $2, $1 }
;
type_variance:
    /* empty */                                 { Invariant }
  | PLUS                                        { Covariant }
  | MINUS                                       { Contravariant }
;
type_variable:
    mktyp(QUOTE ident { Ptyp_var $2 }) { $1 }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $3 :: $1 }
;
constructor_declarations:
  | BAR                                                  { [  ] }
  | constructor_declaration                              { [$1] }
  | bar_constructor_declaration                          { [$1] }
  | constructor_declarations bar_constructor_declaration { $2 :: $1 }
;
constructor_declaration:
  | mkrhs(constr_ident) generalized_constructor_arguments attributes
    { let args,res = $2 in
      let info = symbol_info $endpos in
      Type.constructor $1 ~args ?res ~attrs:$3 ~loc:(make_loc $sloc) ~info }
;
bar_constructor_declaration:
  | BAR mkrhs(constr_ident) generalized_constructor_arguments attributes
    { let args,res = $3 in
      let info = symbol_info $endpos in
      Type.constructor $2 ~args ?res ~attrs:$4 ~loc:(make_loc $sloc) ~info }
;
str_exception_declaration:
  | sig_exception_declaration                    { $1 }
  | EXCEPTION ext_attributes mkrhs(constr_ident)
    EQUAL mkrhs(constr_longident) attributes post_item_attributes
    { let (ext,attrs) = $2 in
      let docs = symbol_docs $sloc in
      Te.mk_exception ~attrs:$7
        (Te.rebind $3 $5 ~attrs:(attrs @ $6) ~loc:(make_loc $sloc) ~docs)
      , ext }
;
sig_exception_declaration:
  | EXCEPTION ext_attributes mkrhs(constr_ident)
    generalized_constructor_arguments attributes post_item_attributes
      { let args, res = $4 in
        let (ext,attrs) = $2 in
        let docs = symbol_docs $sloc in
        Te.mk_exception ~attrs:$6
          (Te.decl $3 ~args ?res
                      ~attrs:(attrs @ $5) ~loc:(make_loc $sloc) ~docs)
        , ext }
;
let_exception_declaration:
    mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let args, res = $2 in
        Te.decl $1 ~args ?res ~attrs:$3 ~loc:(make_loc $sloc) }
;
generalized_constructor_arguments:
    /*empty*/                     { (Pcstr_tuple [],None) }
  | OF constructor_arguments      { ($2,None) }
  | COLON constructor_arguments MINUSGREATER simple_core_type
                                  { ($2,Some $4) }
  | COLON simple_core_type
                                  { (Pcstr_tuple [],Some $2) }
;

constructor_arguments:
  | core_type_list                   { Pcstr_tuple (List.rev $1) }
  | LBRACE label_declarations RBRACE { Pcstr_record $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declaration_semi                      { [$1] }
  | label_declaration_semi label_declarations   { $1 :: $2 }
;
label_declaration:
    mutable_flag mkrhs(label) COLON poly_type_no_attr attributes
      { let info = symbol_info $endpos in
        Type.field $2 $4 ~mut:$1 ~attrs:$5 ~loc:(make_loc $sloc) ~info }
;
label_declaration_semi:
    mutable_flag mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
      { let info =
          match rhs_info $endpos($5) with
          | Some _ as info_before_semi -> info_before_semi
          | None -> symbol_info $endpos
       in
       Type.field $2 $4 ~mut:$1 ~attrs:($5 @ $7) ~loc:(make_loc $sloc) ~info }
;

/* Type Extensions */

str_type_extension:
  TYPE ext_attributes
  nonrec_flag optional_type_parameters mkrhs(type_longident)
  PLUSEQ private_flag str_extension_constructors post_item_attributes
      { let (ext, attrs) = $2 in
        if $3 <> Recursive then not_expecting $loc($3) "nonrec flag";
        let docs = symbol_docs $sloc in
        Te.mk $5 (List.rev $8) ~params:$4 ~priv:$7 ~attrs:(attrs@$9) ~docs
        , ext }
;
sig_type_extension:
  TYPE ext_attributes
  nonrec_flag optional_type_parameters mkrhs(type_longident)
  PLUSEQ private_flag sig_extension_constructors post_item_attributes
      { let (ext, attrs) = $2 in
        if $3 <> Recursive then not_expecting $loc($3) "nonrec flag";
        let docs = symbol_docs $sloc in
        Te.mk $5 (List.rev $8) ~params:$4 ~priv:$7 ~attrs:(attrs@$9) ~docs
        , ext }
;
str_extension_constructors:
    extension_constructor_declaration                     { [$1] }
  | bar_extension_constructor_declaration                 { [$1] }
  | extension_constructor_rebind                          { [$1] }
  | bar_extension_constructor_rebind                      { [$1] }
  | str_extension_constructors bar_extension_constructor_declaration
      { $2 :: $1 }
  | str_extension_constructors bar_extension_constructor_rebind
      { $2 :: $1 }
;
sig_extension_constructors:
    extension_constructor_declaration                     { [$1] }
  | bar_extension_constructor_declaration                 { [$1] }
  | sig_extension_constructors bar_extension_constructor_declaration
      { $2 :: $1 }
;
extension_constructor_declaration:
  | mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let args, res = $2 in
        let info = symbol_info $endpos in
        Te.decl $1 ~args ?res ~attrs:$3 ~loc:(make_loc $sloc) ~info }
;
bar_extension_constructor_declaration:
  | BAR mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let args, res = $3 in
        let info = symbol_info $endpos in
        Te.decl $2 ~args ?res ~attrs:$4 ~loc:(make_loc $sloc) ~info }
;
extension_constructor_rebind:
  | mkrhs(constr_ident) EQUAL mkrhs(constr_longident) attributes
      { let info = symbol_info $endpos in
        Te.rebind $1 $3 ~attrs:$4 ~loc:(make_loc $sloc) ~info }
;
bar_extension_constructor_rebind:
  | BAR mkrhs(constr_ident) EQUAL mkrhs(constr_longident) attributes
      { let info = symbol_info $endpos in
        Te.rebind $2 $4 ~attrs:$5 ~loc:(make_loc $sloc) ~info }
;

/* "with" constraints (additional type equations over signature components) */

with_constraints:
    with_constraint                             { [$1] }
  | with_constraints AND with_constraint        { $3 :: $1 }
;
with_constraint:
    TYPE optional_type_parameters mkrhs(label_longident) with_type_binder
    core_type_no_attr constraints
      { let lident = Location.{ $3 with txt = Longident.last $3.txt } in
        Pwith_type
          ($3,
           (Type.mk lident
              ~params:$2
              ~cstrs:(List.rev $6)
              ~manifest:$5
              ~priv:$4
              ~loc:(make_loc $sloc))) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE optional_type_parameters mkrhs(label_longident)
    COLONEQUAL core_type_no_attr
      { let lident = Location.{ $3 with txt = Longident.last $3.txt } in
        Pwith_typesubst
         ($3,
           (Type.mk lident
              ~params:$2
              ~manifest:$5
              ~loc:(make_loc $sloc))) }
  | MODULE mkrhs(mod_longident) EQUAL mkrhs(mod_ext_longident)
      { Pwith_module ($2, $4) }
  | MODULE mkrhs(mod_longident) COLONEQUAL mkrhs(mod_ext_longident)
      { Pwith_modsubst ($2, $4) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private }
;

/* Polymorphic types */

typevar_list:
        QUOTE mkrhs(ident)                      { [$2] }
      | typevar_list QUOTE mkrhs(ident)         { $3 :: $1 }
;
poly_type:
        core_type
          { $1 }
      | mktyp(typevar_list DOT core_type
          { Ptyp_poly(List.rev $1, $3) })
          { $1 }
;
poly_type_no_attr:
        core_type_no_attr
          { $1 }
      | mktyp(typevar_list DOT core_type_no_attr
          { Ptyp_poly(List.rev $1, $3) })
          { $1 }
;

/* Core types */

core_type:
    core_type_no_attr
      { $1 }
  | core_type attribute
      { Typ.attr $1 $2 }
;
core_type_no_attr:
    core_type2 %prec MINUSGREATER
      { $1 }
  | mktyp(core_type2 AS QUOTE ident
      { Ptyp_alias($1, $4) })
      { $1 }
;
core_type2:
    simple_core_type_or_tuple
      { $1 }
  | mktyp(core_type2_)
      { $1 }
;
core_type2_:
    QUESTION LIDENT COLON extra_core_type2 MINUSGREATER core_type2
      { Ptyp_arrow(Optional $2, $4, $6) }
  | OPTLABEL extra_core_type2 MINUSGREATER core_type2
      { Ptyp_arrow(Optional $1 , $2, $4) }
  | LIDENT COLON extra_core_type2 MINUSGREATER core_type2
      { Ptyp_arrow(Labelled $1, $3, $5) }
  | extra_core_type2 MINUSGREATER core_type2
      { Ptyp_arrow(Nolabel, $1, $3) }
;
%inline extra_core_type2: core_type2
  { extra_rhs_core_type $1 ~pos:$endpos($1) };

simple_core_type:
    simple_core_type2  %prec below_HASH
      { $1 }
  | LPAREN core_type RPAREN %prec below_HASH
      { $2 }
;
simple_core_type2:
  | LPAREN MODULE ext_attributes package_type RPAREN
      { wrap_typ_attrs ~loc:$sloc (reloc_typ ~loc:$sloc $4) $3 }
  | mktyp(simple_core_type2_)
      { $1 }
;
simple_core_type2_:
    QUOTE ident
      { Ptyp_var $2 }
  | UNDERSCORE
      { Ptyp_any }
  | mkrhs(type_longident)
      { Ptyp_constr($1, []) }
  | simple_core_type2 mkrhs(type_longident)
      { Ptyp_constr($2, [$1]) }
  | LPAREN inline_core_type_comma_list RPAREN mkrhs(type_longident)
      { Ptyp_constr($4, List.rev $2) }
  | LESS meth_list GREATER
      { let (f, c) = $2 in Ptyp_object (f, c) }
  | LESS GREATER
      { Ptyp_object ([], Closed) }
  | HASH mkrhs(class_longident)
      { Ptyp_class($2, []) }
  | simple_core_type2 HASH mkrhs(class_longident)
      { Ptyp_class($3, [$1]) }
  | LPAREN inline_core_type_comma_list RPAREN HASH mkrhs(class_longident)
      { Ptyp_class($5, List.rev $2) }
  | LBRACKET tag_field RBRACKET
      { Ptyp_variant([$2], Closed, None) }
/* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type RBRACKET
      { Ptyp_variant([$2], Closed, None) }
*/
  | LBRACKET BAR row_field_list RBRACKET
      { Ptyp_variant(List.rev $3, Closed, None) }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { Ptyp_variant($2 :: List.rev $4, Closed, None) }
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { Ptyp_variant(List.rev $3, Open, None) }
  | LBRACKETGREATER RBRACKET
      { Ptyp_variant([], Open, None) }
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { Ptyp_variant(List.rev $3, Closed, Some []) }
  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { Ptyp_variant(List.rev $3, Closed, Some (List.rev $5)) }
  | extension
      { Ptyp_extension $1 }
;

package_type:
    mktyp(module_type
      { Ptyp_package (package_type_of_module_type $1) })
      { $1 }
;
row_field_list:
    row_field                                   { [$1] }
  | row_field_list BAR row_field                { $3 :: $1 }
;
row_field:
    tag_field          { $1 }
  | simple_core_type   { Rf.inherit_ ~loc:(make_loc $sloc) $1 }
;
tag_field:
    mkrhs(name_tag) OF opt_ampersand amper_type_list attributes
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $5 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 $3 (List.rev $4) }
  | mkrhs(name_tag) attributes
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $2 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 true [] }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
amper_type_list:
    core_type_no_attr                           { [$1] }
  | amper_type_list AMPERSAND core_type_no_attr { $3 :: $1 }
;
name_tag_list:
    name_tag                                    { [$1] }
  | name_tag_list name_tag                      { $2 :: $1 }
;
simple_core_type_or_tuple:
    simple_core_type { $1 }
  | mktyp(simple_core_type STAR core_type_list
      { Ptyp_tuple($1 :: List.rev $3) })
      { $1 }
;
(* A [core_type_comma_list] is a nonempty, comma-separated list of types. *)
%inline core_type_comma_list:
  tys = reversed_separated_nonempty_list(COMMA, core_type)
    { tys }
;
(* [inline_core_type_comma_list] is semantically equivalent to
   [core_type_comma_list], that is, it recognizes the same language.
   It is used in some places to avoid a conflict between the normal use of
   parentheses as a disambiguation device, e.g.
     (foo -> bar) -> baz
   and the use of parentheses in parameterized types, e.g.
     (foo -> bar) list
   Inlining allows the parser to shift the closing parenthesis without (yet)
   deciding which of the above two situations we have. *)
%inline inline_core_type_comma_list:
  tys = inline_reversed_separated_nonempty_list(COMMA, core_type)
    { tys }
;
core_type_list:
    simple_core_type                       { [$1] }
  | core_type_list STAR simple_core_type   { $3 :: $1 }
;
meth_list:
    field_semi meth_list
      { let (f, c) = $2 in ($1 :: f, c) }
  | inherit_field_semi meth_list
      { let (f, c) = $2 in ($1 :: f, c) }
  | field_semi           { [$1], Closed }
  | field                { [$1], Closed }
  | inherit_field_semi   { [$1], Closed }
  | simple_core_type     { [Of.inherit_ ~loc:(make_loc $sloc) $1], Closed }
  | DOTDOT               { [], Open }
;
field:
  mkrhs(label) COLON poly_type_no_attr attributes
    { let info = symbol_info $endpos in
      let attrs = add_info_attrs info $4 in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

field_semi:
  mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
    { let info =
        match rhs_info $endpos($4) with
        | Some _ as info_before_semi -> info_before_semi
        | None -> symbol_info $endpos
      in
      let attrs = add_info_attrs info ($4 @ $6) in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

inherit_field_semi:
  simple_core_type SEMI
    { Of.inherit_ ~loc:(make_loc $sloc) $1 }

label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
  | INT          { let (n, m) = $1 in Pconst_integer (n, m) }
  | CHAR         { Pconst_char $1 }
  | STRING       { let (s, d) = $1 in Pconst_string (s, d) }
  | FLOAT        { let (f, m) = $1 in Pconst_float (f, m) }
;
signed_constant:
    constant     { $1 }
  | MINUS INT    { let (n, m) = $2 in Pconst_integer("-" ^ n, m) }
  | MINUS FLOAT  { let (f, m) = $2 in Pconst_float("-" ^ f, m) }
  | PLUS INT     { let (n, m) = $2 in Pconst_integer (n, m) }
  | PLUS FLOAT   { let (f, m) = $2 in Pconst_float(f, m) }
;

/* Identifiers and long identifiers */

ident:
    UIDENT                    { $1 }
  | LIDENT                    { $1 }
;
val_ident:
    LIDENT                    { $1 }
  | LPAREN operator RPAREN    { $2 }
  | LPAREN operator error     { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN error              { expecting $loc($2) "operator" }
  | LPAREN MODULE error       { expecting $loc($3) "module-expr" }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | DOTOP LPAREN RPAREN                         { "."^ $1 ^"()" }
  | DOTOP LPAREN RPAREN LESSMINUS               { "."^ $1 ^ "()<-" }
  | DOTOP LBRACKET RBRACKET                     { "."^ $1 ^"[]" }
  | DOTOP LBRACKET RBRACKET LESSMINUS           { "."^ $1 ^ "[]<-" }
  | DOTOP LBRACE RBRACE                         { "."^ $1 ^"{}" }
  | DOTOP LBRACE RBRACE LESSMINUS               { "."^ $1 ^ "{}<-" }
  | HASHOP                                      { $1 }
  | BANG                                        { "!" }
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
  | PLUSEQ                                      { "+=" }
  | PERCENT                                     { "%" }
;
constr_ident:
    UIDENT                                      { $1 }
  | LBRACKET RBRACKET                           { "[]" }
  | LPAREN RPAREN                               { "()" }
  | LPAREN COLONCOLON RPAREN                    { "::" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | mod_longident DOT LPAREN COLONCOLON RPAREN  { Ldot($1,"::") }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | LPAREN COLONCOLON RPAREN                    { Lident "::" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN
      { lapply ~loc:$sloc $1 $3 }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
clty_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
class_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;

/* Toplevel directives */

toplevel_directive:
  toplevel_directive_
    { let (dir, arg) = $1 in
      mk_directive ~loc:$sloc dir arg }
;
toplevel_directive_:
    HASH mkrhs(ident)
    { $2, None }
  | HASH mkrhs(ident) toplevel_directive_argument
    { $2, Some $3 }
;

toplevel_directive_argument:
  toplevel_directive_argument_
    { mk_directive_arg ~loc:$sloc $1 }
;
toplevel_directive_argument_:
  | STRING        { let (s, _) = $1 in Pdir_string s }
  | INT           { let (n, m) = $1 in Pdir_int (n ,m) }
  | val_longident { Pdir_ident $1 }
  | mod_longident { Pdir_ident $1 }
  | FALSE         { Pdir_bool false }
  | TRUE          { Pdir_bool true }
;

/* Miscellaneous */

name_tag:
    BACKQUOTE ident                             { $2 }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
nonrec_flag:
    /* empty */                                 { Recursive }
  | NONREC                                      { Nonrecursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
private_virtual_flags:
    /* empty */  { Public, Concrete }
  | PRIVATE { Private, Concrete }
  | VIRTUAL { Public, Virtual }
  | PRIVATE VIRTUAL { Private, Virtual }
  | VIRTUAL PRIVATE { Private, Virtual }
;
override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;

/* Attributes and extensions */

single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | MATCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | NONREC { "nonrec" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
  mkloc(
      single_attr_id { $1 }
    | single_attr_id DOT attr_id { $1 ^ "." ^ $3.txt }
  ) { $1 }
;
attribute:
  LBRACKETAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
post_item_attribute:
  LBRACKETATAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
floating_attribute:
  LBRACKETATATAT attr_id payload RBRACKET
    { mark_symbol_docs $sloc;
      Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
post_item_attributes:
    /* empty */  { [] }
  | post_item_attribute post_item_attributes { $1 :: $2 }
;
attributes:
    /* empty */{ [] }
  | attribute attributes { $1 :: $2 }
;
ext_attributes:
    /* empty */  { None, [] }
  | attribute attributes { None, $1 :: $2 }
  | PERCENT attr_id attributes { Some $2, $3 }
;
extension:
  LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
;
item_extension:
  LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
;
payload:
    structure { PStr $1 }
  | COLON signature { PSig $2 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, None) }
  | QUESTION pattern WHEN seq_expr { PPat ($2, Some $4) }
;
%%
