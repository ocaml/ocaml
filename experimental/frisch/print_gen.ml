(* Generate code to print values of a certain type as OCaml constants.
   This illustrates how to build fragments of Parsetree through
   Ast_helper and more local helper functions. *)

open Location
open Types
open Asttypes
open Ast_helper

let env = Env.initial

let clean s =
  let s = String.copy s in
  for i = 0 to String.length s - 1 do
    if s.[i] = '.' then s.[i] <- '_'
  done;
  s

let print_fun s =
  "variantize_" ^ clean s

let printed = Hashtbl.create 16
let funs = ref []

let ignored_types =
  [
   "Location.t";
  ]

let lid s = mknoloc (Longident.parse s)
let constr s x = Exp.construct (lid s) (Some x) true
let constr0 s = Exp.construct (lid s) None true
let pconstr s x = Pat.construct (lid s) (Some x) true
let pconstr0 s = Pat.construct (lid s) None true
let nil = constr0 "[]"
let cons hd tl = constr "::" (Exp.tuple [hd; tl])
let list l = List.fold_right cons l nil
let lam pat exp = Exp.function_ "" None [pat, exp]
let func l = Exp.function_ "" None l
let str s = Exp.constant (Const_string (s, None))
let app f l = Exp.apply f (List.map (fun a -> "", a) l)
let pvar s = Pat.var (mknoloc s)
let evar s = Exp.ident (lid s)
let pair x y = Exp.tuple [x; y]
let let_in b body = Exp.let_ Nonrecursive b body

let rec gen ty =
  if Hashtbl.mem printed ty || List.mem ty ignored_types then ()
  else let (_, td) =
    try Env.lookup_type (Longident.parse ty) env
    with Not_found ->
      failwith (Printf.sprintf "Cannot resolve type %s" ty)
  in
  Hashtbl.add printed ty ();
  let params =
    List.mapi
      (fun i t ->
        let s = Printf.sprintf "f%i" i in
        (t.id, evar s), pvar s
      )
      td.type_params
  in
  let env = List.map fst params in
  let e = match td.type_kind, td.type_manifest with
  | Type_record (l, _), _ ->
      let field (s, _, t) =
        let s = Ident.name s in
        (lid s, pvar s),
        pair (str s) (tyexpr env t (evar s))
      in
      let l = List.map field l in
      lam
        (Pat.record (List.map fst l) Closed)
        (constr "Record" (list (List.map snd l)))
  | Type_variant l, _ ->
      let case (c, tyl, _) =
        let c = Ident.name c in
        let pat, args =
          match tyl with
          | [] ->
              pconstr0 c, []
          | [t] ->
              pconstr c (pvar "x"),
              [ tyexpr env t (evar "x") ]
          | l ->
              let p, e = tuple env l in
              pconstr c p, e
        in
        pat, constr "Constructor" (pair (str c) (list args))
      in
      func (List.map case l)
  | Type_abstract, Some t ->
      tyexpr_fun env t
  | _ ->
      constr "Abstract" (str ty)
  in
  let e = List.fold_right lam (List.map snd params) e in
  funs := (Pat.var (mknoloc (print_fun ty)), e) :: !funs


and tuple env tl =
  let arg i t =
    let x = Printf.sprintf "x%i" i in
    pvar x, tyexpr env t (evar x)
  in
  let l = List.mapi arg tl in
  Pat.tuple (List.map fst l), List.map snd l


and tyexpr env ty x =
  match ty.desc with
  | Tvar _ ->
      let f =
        try List.assoc ty.id env
        with Not_found -> assert false
      in
      app f [x]
  | Ttuple tl ->
      let p, e = tuple env tl in
      let_in [p, x] (list e)
  | Tconstr (path, [t], _) when Path.same path Predef.path_list ->
      constr "List" (app (evar "List.map") [tyexpr_fun env t; x])
  | Tconstr (path, [t], _) when Path.same path Predef.path_option ->
      constr "Option" (app (evar "option_map") [tyexpr_fun env t; x])
  | Tconstr (path, [], _) when Path.same path Predef.path_string ->
      constr "String" x
  | Tconstr (path, [], _) when Path.same path Predef.path_int ->
      constr "Int" x
  | Tconstr (path, tl, _) ->
      let ty = Path.name path in
      gen ty;
      app (evar (print_fun ty)) (List.map (tyexpr_fun env) tl @ [x])
   | _ ->
       Printtyp.type_expr Format.str_formatter ty;
       constr "UNKNOWN" (str (Format.flush_str_formatter ()))

and tyexpr_fun env ty =
  lam (pvar "x") (tyexpr env ty (evar "x"))


let () =
  Config.load_path := ["../../parsing"];
  gen "Parsetree.expression";
  let i = Str.value Recursive !funs in
  Format.printf "%a@." Pprintast.structure [i]

