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

let print_fun s = "lift_" ^ clean s

let printed = Hashtbl.create 16
let meths = ref []

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
let selfcall m args = app (Exp.send (evar "this") m) args

let rec gen ty =
  if Hashtbl.mem printed ty then ()
  else let (_, td) =
    try Env.lookup_type (Longident.parse ty) env
    with Not_found ->
      failwith (Printf.sprintf "Cannot resolve type %s" ty)
  in
  Hashtbl.add printed ty ();
  let params = List.mapi (fun i _ -> Printf.sprintf "f%i" i) td.type_params in
  let env = List.map2 (fun s t -> t.id, evar s) params td.type_params in
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
        (selfcall "mk_record" [list (List.map snd l)])
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
        pat, selfcall "mk_constr" [pair (str c) (list args)]
      in
      func (List.map case l)
  | Type_abstract, Some t ->
      tyexpr_fun env t
  | _ ->
      assert false
(*      lam (Pat.any ()) (constr "Abstract" (str ty)) *)
  in
  let e = List.fold_right lam (List.map pvar params) e in
  let tyargs = List.map Typ.var params in
  let t = Typ.(arrow "" (constr (lid ty) tyargs) (var "res")) in
  let t =
    List.fold_right
      (fun s t ->
        Typ.(arrow "" (arrow "" (var s) (var "res")) t))
      params t
  in
  let t = Typ.poly params t in
  let body = Exp.poly e (Some t) in
  meths := Cf.meth (mknoloc (print_fun ty)) Public Fresh body :: !meths


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
      let_in [p, x] (selfcall "mk_tuple" [list e])
  | Tconstr (path, [t], _) when Path.same path Predef.path_list ->
      selfcall "mk_list" [app (evar "List.map") [tyexpr_fun env t; x]]
  | Tconstr (path, [], _) when Path.same path Predef.path_string ->
      selfcall "mk_string" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_int ->
      selfcall "mk_int" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_char ->
      selfcall "mk_char" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_int32 ->
      selfcall "mk_int32" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_int64 ->
      selfcall "mk_int64" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_nativeint ->
      selfcall "mk_nativeint" [x]
  | Tconstr (path, [], _) when Path.name path = "Location.t" ->
      selfcall "mk_location" [x]
  | Tconstr (path, tl, _) ->
      let ty = Path.name path in
      gen ty;
      selfcall (print_fun ty) (List.map (tyexpr_fun env) tl @ [x])
   | _ ->
       assert false
(*
       Printtyp.type_expr Format.str_formatter ty;
       constr "UNKNOWN" (str (Format.flush_str_formatter ()))
*)

and tyexpr_fun env ty =
  lam (pvar "x") (tyexpr env ty (evar "x"))

let simplify =
  object
    inherit Ast_mapper.mapper as super
    method! expr e =
      let e = super # expr e in
      let open Longident in
      let open Parsetree in
      match e.pexp_desc with
      | Pexp_function ("", None, [{ppat_desc = Ppat_var{txt=id}},
                                  {pexp_desc = Pexp_apply(f,
                                                          ["",{pexp_desc=Pexp_ident{txt=Lident id2}}])}]) when id = id2 -> f
      | _ -> e
  end

let () =
  Config.load_path := ["../../parsing"];
  gen "Parsetree.expression";
  let cl = {Parsetree.pcstr_pat = pvar "this"; pcstr_fields = !meths} in
  let params = [mknoloc "res", Invariant], Location.none in
  let cl = Ci.mk ~virt:Virtual ~params (mknoloc "lifter") (Cl.structure cl) in
  let s = Str.([
               open_ (lid "Asttypes");
               open_ (lid "Longident");
               open_ (lid "Parsetree");
               Str.class_ [cl]
        ])
  in
  Format.printf "%a@." Pprintast.structure (simplify # structure s)
