(* Generate code to lift values of a certain type.
   This illustrates how to build fragments of Parsetree through
   Ast_helper and more local helper functions. *)

module Main : sig end = struct

open Location
open Types
open Asttypes
open Ast_helper
open Ast_helper.Convenience

let selfcall ?(this = "this") m args = app (Exp.send (evar this) m) args

(*************************************************************************)


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

let rec gen ty =
  if Hashtbl.mem printed ty then ()
  else let tylid = Longident.parse ty in
  let (_, td) =
    try Env.lookup_type tylid env
    with Not_found ->
      Format.eprintf "** Cannot resolve type %s" ty;
      exit 2
  in
  let prefix =
    let open Longident in
    match tylid with
    | Ldot (m, _) -> String.concat "." (Longident.flatten m) ^ "."
    | Lident _ -> ""
    | Lapply _ -> assert false
  in
  Hashtbl.add printed ty ();
  let params = List.mapi (fun i _ -> Printf.sprintf "f%i" i) td.type_params in
  let env = List.map2 (fun s t -> t.id, evar s) params td.type_params in
  let tyargs = List.map Typ.var params in
  let t = Typ.(arrow "" (constr (lid ty) tyargs) (var "res")) in
  let t =
    List.fold_right
      (fun s t ->
        Typ.(arrow "" (arrow "" (var s) (var "res")) t))
      params t
  in
  let t = Typ.poly params t in
  let concrete e =
    let e = List.fold_right lam (List.map pvar params) e in
    let body = Exp.poly e (Some t) in
    meths := Cf.(method_ (mknoloc (print_fun ty)) Public (concrete Fresh body)) :: !meths
  in
  match td.type_kind, td.type_manifest with
  | Type_record (l, _), _ ->
      let field (s, _, t) =
        let s = Ident.name s in
        (lid (prefix ^ s), pvar s),
        tuple[str s; tyexpr env t (evar s)]
      in
      let l = List.map field l in
      concrete
        (lam
           (Pat.record (List.map fst l) Closed)
           (selfcall "record" [str ty; list (List.map snd l)]))
  | Type_variant l, _ ->
      let case (c, tyl, _) =
        let c = Ident.name c in
        let qc = prefix ^ c in
        let p, args = gentuple env tyl in
        pconstr qc p, selfcall "constr" [str ty; tuple[str c; list args]]
      in
      concrete (func (List.map case l))
  | Type_abstract, Some t ->
      concrete (tyexpr_fun env t)
  | Type_abstract, None ->
      (* Generate an abstract method to lift abstract types *)
      meths := Cf.(method_ (mknoloc (print_fun ty)) Public (virtual_ t)) :: !meths

and gentuple env tl =
  let arg i t =
    let x = Printf.sprintf "x%i" i in
    pvar x, tyexpr env t (evar x)
  in
  List.split (List.mapi arg tl)

and tyexpr env ty x =
  match ty.desc with
  | Tvar _ ->
      let f =
        try List.assoc ty.id env
        with Not_found -> assert false
      in
      app f [x]
  | Ttuple tl ->
      let p, e = gentuple env tl in
      let_in [Vb.mk (Pat.tuple p) x] (selfcall "tuple" [list e])
  | Tconstr (path, [t], _) when Path.same path Predef.path_list ->
      selfcall "list" [app (evar "List.map") [tyexpr_fun env t; x]]
  | Tconstr (path, [t], _) when Path.same path Predef.path_array ->
      selfcall "array" [app (evar "Array.map") [tyexpr_fun env t; x]]
  | Tconstr (path, [], _) when Path.same path Predef.path_string ->
      selfcall "string" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_int ->
      selfcall "int" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_char ->
      selfcall "char" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_int32 ->
      selfcall "int32" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_int64 ->
      selfcall "int64" [x]
  | Tconstr (path, [], _) when Path.same path Predef.path_nativeint ->
      selfcall "nativeint" [x]
  | Tconstr (path, tl, _) ->
      let ty = Path.name path in
      gen ty;
      selfcall (print_fun ty) (List.map (tyexpr_fun env) tl @ [x])
  | _ ->
      Format.eprintf "** Cannot deal with type %a@." Printtyp.type_expr ty;
      exit 2

and tyexpr_fun env ty =
  lam (pvar "x") (tyexpr env ty (evar "x"))

let simplify =
  (* (fun x -> <expr> x) ====> <expr> *)
  object
    inherit Ast_mapper.mapper as super
    method! expr e =
      let e = super # expr e in
      let open Longident in
      let open Parsetree in
      match e.pexp_desc with
      | Pexp_fun
          ("", None,
           {ppat_desc = Ppat_var{txt=id;_};_},
           {pexp_desc =
            Pexp_apply
              (f,
               ["",{pexp_desc=
                    Pexp_ident{txt=Lident id2;_};_}]);_}) when id = id2 -> f
      | _ -> e
  end

let args =
  let open Arg in
  [
   "-I", String (fun s -> Config.load_path := s :: !Config.load_path),
   "<dir> Add <dir> to the list of include directories";
  ]

let usage =
  Printf.sprintf "%s [options] <type names>\n" Sys.argv.(0)

let () =
  Config.load_path := [];
  Arg.parse (Arg.align args) gen usage;
  let cl = Cstr.mk (pvar "this") !meths in
  let params = [mknoloc "res", Invariant] in
  let cl = Ci.mk ~virt:Virtual ~params (mknoloc "lifter") (Cl.structure cl) in
  let s = [Str.class_ [cl]] in
  Format.printf "%a@." Pprintast.structure (simplify # structure s)

end
