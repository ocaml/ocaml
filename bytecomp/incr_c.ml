(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                                and C++                                 *)
(*                                                                        *)
(*                                                                        *)
(*   All rights reserved, except those that aren't, which is all of them. *)
(*                                                                        *)
(**************************************************************************)

type cpp_type = Ty_int | Ty_bool | Ty_pe | Ty_template

type defn =
  | Const of cpp_type * expr
  | Struct of
      { params: (cpp_type * Ident.t) list;
        special: expr list;
        body: bindings option }

and bindings = (binder_name * defn) list

and binder_name = Local of Ident.t | Export of field_name

and expr =
  | Var of Ident.t
  | Const_int of int
  | Const_bool of bool
  | Tapp of expr * expr list
  | Proj of expr * cpp_type * field_name
  | Binop of expr * string * expr

and field_name =
  | Tag
  | Val
  | Res
  | App
  | Nonzero
  | Print
  | Field of int

module Pp = struct
  open Format

  module StringSet = Set.Make (String)
  type env = { names: string Ident.Map.t; used: StringSet.t }

  let field_name = function
    | Tag -> "tag"
    | Val -> "val"
    | Res -> "res"
    | App -> "app"
    | Nonzero -> "nonzero"
    | Print -> "print"
    | Field n -> "f" ^ string_of_int n

  let cpp_reserved =
    ["alignas"; "alignof"; "and"; "and_eq"; "asm"; "atomic_cancel";
     "atomic_commit"; "atomic_noexcept"; "auto"; "bitand"; "bitor"; "bool";
     "break"; "case"; "catch"; "char"; "char8_t"; "char16_t"; "char32_t";
     "class"; "compl"; "concept"; "const"; "consteval"; "constexpr";
     "constinit"; "const_cast"; "continue"; "contract_assert"; "co_await";
     "co_return"; "co_yield"; "decltype"; "default"; "delete"; "do";
     "double"; "dynamic_cast"; "else"; "enum"; "explicit"; "export";
     "extern"; "false"; "float"; "for"; "friend"; "goto"; "if"; "inline";
     "int"; "long"; "mutable"; "namespace"; "new"; "noexcept"; "not";
     "not_eq"; "nullptr"; "operator"; "or"; "or_eq"; "private";
     "protected"; "public"; "reflexpr"; "register"; "reinterpret_cast";
     "requires"; "return"; "short"; "signed"; "sizeof"; "static";
     "static_assert"; "static_cast"; "struct"; "switch"; "synchronized";
     "template"; "this"; "thread_local"; "throw"; "true"; "try"; "typedef";
     "typeid"; "typename"; "union"; "unsigned"; "using"; "virtual"; "void";
     "volatile"; "wchar_t"; "while"; "xor"; "xor_eq"]

  let reserved =
    let fields =
      Tag :: Val :: Res :: App :: Nonzero :: Print ::
        List.init 1000 (fun i -> Field i)
    in
    List.map field_name fields @ cpp_reserved

  let reserve env names =
    { env with used = StringSet.union env.used (StringSet.of_list names) }

  let fully_empty = { names = Ident.Map.empty; used = StringSet.empty }

  let empty_env = reserve fully_empty reserved

  let pp = fprintf

  let commas f ppf xs =
    pp_print_list ~pp_sep:(fun ppf () -> pp ppf ",@ ") f ppf xs

  let ident ~env ppf id =
    match Ident.Map.find id env.names with
    | s -> pp ppf "%s" s
    | exception Not_found ->
       failwith (Printf.sprintf "Ident %s not bound" (Ident.name id))

  let bind env id =
    let rec freshen k name =
      let name' =
        match k with
        | 0 -> name
        | 1 -> name ^ "_"
        | n -> name ^ "_" ^ string_of_int n
      in
      if not (StringSet.mem name' env.used) then name'
      else freshen (k+1) name
    in
    let name = Ident.name id in
    let fix_char = function
      | '0'..'9' | 'a'..'z' | 'A'..'Z' | '_' as c -> c
      | _ -> '_'
    in
    let name = String.map fix_char name in
    let name = freshen 0 name in
    { names = Ident.Map.add id name env.names;
      used = StringSet.add name env.used }

  let bind env id =
    if Ident.Map.mem id env.names then env else bind env id

  let binder ~env ppf = function
    | Local id -> ident ~env ppf id
    | Export s -> pp ppf "%s" (field_name s)

  let cpp_type = function
    | Ty_int -> "int"
    | Ty_bool -> "bool"
    | Ty_pe -> "typename"
    | Ty_template -> "template"

  let rec expr' ~env ppf = function
    | Var id -> ident ~env ppf id
    | Const_int n -> pp ppf "%d" n
    | Const_bool b -> pp ppf "%b" b
    | Tapp (fn, es) ->
       pp ppf "%a<%a>" (expr' ~env) fn (commas (expr ~env)) es
    | Proj (e, Ty_template, s) ->
       pp ppf "%a::template %s" (expr' ~env) e (field_name s)
    | Proj (e, _, s) ->
       pp ppf "%a::%s" (expr' ~env) e (field_name s)
    | Binop (e1, op, e2) ->
       pp ppf "((%a) %s (%a))" (expr ~env) e1 op (expr ~env) e2

  and expr ~env ppf = function
    | Proj (_, Ty_pe, _) as e -> pp ppf "typename %a" (expr' ~env) e
    | e -> expr' ~env ppf e

  let expr ~env ppf e = pp ppf "@[%a@]" (expr ~env) e

  let template_params ~show_empty ~env ppf = function
    | [] when not show_empty -> ()
    | ps ->
       pp ppf "@[<h>template@ <%a>@]@ "
         (commas (fun ppf (k, id) ->
              pp ppf "%s %a" (cpp_type k) (ident ~env) id)) ps

  let rec defn ~env id ppf = function
    | Const (Ty_pe, e) ->
       pp ppf "@[<h>typedef %a %a@];@ " (expr ~env) e (binder ~env) id
    | Const (ty, e) ->
       pp ppf "@[<h>static constexpr %s %a = %a@];@ "
         (cpp_type ty) (binder ~env) id (expr ~env) e
    | Struct { params; special; body } ->
       let opt_body ~env ppf = function
         | None -> ()
         | Some body -> pp ppf "{@ %a}" (defns ~env) body
       in
       let pp_special ~env ppf = function
         | [] -> ()
         | args -> pp ppf "<%a>" (commas (expr ~env)) args
       in
       let env' = List.fold_left bind env (List.map snd params) in
       pp ppf "@[<v>%astruct @[<h>%a%a@]%a@];@ "
         (template_params ~show_empty:(special <> []) ~env:env') params
         (binder ~env) id
         (pp_special ~env:env') special
         (opt_body ~env:env') body

  and defns ~env ppf defns =
    List.fold_left (fun env (id, d) ->
      let env =
        match id with Local id -> bind env id | Export _ -> env in
      defn ~env id ppf d;
      env) env defns |> ignore
end


let var = Ident.create_local

let to_channel oc (program : Lambda.program) =
  let mk_raise = var "EXCEPTION" in
  let mk_int = var "I" in
  let mk_block0 = var "Cons" in
  let mk_blockT = var "Cons_" in
  let block_arities = Hashtbl.create 10 in
  let mk_block ~tag args =
    Hashtbl.replace block_arities (List.length args) ();
    if tag = 0 then
      Tapp (Var mk_block0, args)
    else
      Tapp (Var mk_blockT, Const_int tag :: args)
  in
  let basic_defns () =
    let blocks =
      Hashtbl.to_seq block_arities |> List.of_seq |> List.sort compare in
    let block_struct (n, ()) =
      [(Local mk_block0,
       let fs = List.init n (fun n -> var ("f" ^ string_of_int n)) in
       Struct
         { params = (List.map (fun v -> Ty_pe, v) fs);
           special = List.map (fun v -> Var v) fs;
           body = Some (
            (Export Tag, Const (Ty_int, Const_int 0)) ::
            (Export Nonzero, Const (Ty_bool, Const_bool true)) ::
            (Export Val, Const (Ty_int, Const_int (-1))) ::
            List.mapi (fun i f -> Export (Field i),
                                  Const (Ty_pe, Var f)) fs)});
       (Local mk_blockT,
       let tag_ = var "tag" in
       let fs = List.init n (fun n -> var ("f" ^ string_of_int n)) in
       Struct
         { params = ([Ty_int, tag_] @ (List.map (fun v -> Ty_pe, v) fs));
           special = [Var tag_] @ List.map (fun v -> Var v) fs;
           body = Some (
             (Export Tag, Const (Ty_int, Var tag_)) ::
             (Export Nonzero, Const (Ty_bool, Const_bool true)) ::
             (Export Val, Const (Ty_int, Const_int (-1))) ::
               List.mapi (fun i f -> Export (Field i),
                                     Const (Ty_pe, Var f)) fs) })]
    in
    let ints =
      (Local mk_int,
       let n = var "n" in
       Struct
         { params = [Ty_int, n];
           special = [];
           body = Some [
             Export Tag, Const (Ty_int, Const_int 1000);
             Export Nonzero, Const (Ty_bool, Binop (Var n, "!=", Const_int 0));
             Export Val, Const (Ty_int, Var n)]})
    in
    let exn =
      [Local mk_raise,
       Struct
         { params = [];
           special = [];
           body = Some [] }]
    in
    ints :: exn @ List.concat_map block_struct blocks
  in
  let hoisted = ref [] in
  let catchers = ref [] in
  let main = ref None in

  let rec compile : Lambda.lambda -> bindings * expr = function
    | Lvar id -> [], Var id
    | Lfunction fn ->
       let name = Debuginfo.Scoped_location.string_of_scoped_location fn.loc in
       let id = Ident.create_local name in
       let fn = compile_lfunction fn in
       [Local id, fn], Var id
    | Llet (_, _, id, Lfunction fn, e2) ->
       if Ident.name id = "main" then begin
         let ps = List.map (fun (i,_) -> Ident.name i) fn.params in
         main := Some (id, ps)
       end;
       let fn = compile_lfunction fn in
       let b2, e2 = compile e2 in
       [Local id, fn] @ b2, e2
    | Llet (_, _, id, e1, e2) ->
       let b1, e1 = compile e1 in
       let b2, e2 = compile e2 in
       b1 @ [Local id, Const (Ty_pe, e1)] @ b2, e2
    | Lsequence (e1, e2) ->
       let b1, e1 = compile e1 in
       let unused = var "unit" in
       let b2, e2 = compile e2 in
       b1 @ [Local unused, Const (Ty_pe, e1)] @ b2, e2
    | Lletrec (fns,body) ->
       let decls =
         List.map (fun Lambda.{id;_} ->
             Local id, Struct {params=[]; special=[]; body=None}) fns
       in
       let fns = List.map (fun Lambda.{id;def} ->
         Local id, compile_lfunction def) fns in
       let b, e = compile body in
       decls@fns@b, e
    | Lapply {ap_func=fn; ap_args=args;_} ->
       let b_func, e_func = compile fn in
       let args = List.map compile args in
       let e = compile_application e_func (List.map snd args) in
       b_func @ List.concat_map fst args, e
    | Lifthenelse(cond,ifso,ifnot) ->
       let id = var "ifthenelse" in
       let condv = var "cond" in
       let fvs =
         hoist_conditional ~id ~params:[Ty_bool, condv] ~default:None
           [[Const_bool true], ifso;
            [Const_bool false], ifnot]
       in
       let b_cond, e_cond = compile cond in
       let args = fvs @ [Proj(e_cond,Ty_bool,Nonzero)] in
       b_cond, Proj (Tapp (Var id, args), Ty_pe, Res)
    | Lswitch (e, sw, _) ->
       let id = var "switch" in
       let default = sw.sw_failaction in
       let b_val, e_val = compile e in
       let fvs, args =
         match sw.sw_consts, sw.sw_blocks with
         | [], blks ->
            let tag = var "tag" in
            hoist_conditional ~id ~params:[Ty_int, tag] ~default
              (List.map (fun (k, e) -> [Const_int k], e) blks),
            [Proj (e_val, Ty_int, Tag)]
         | ints, [] ->
            let n = var "n" in
            hoist_conditional ~id ~params:[Ty_int, n] ~default
              (List.map (fun (k, e) -> [Const_int k], e) ints),
            [Proj (e_val, Ty_int, Val)]
         | ints, blks ->
            let tag = var "tag" in
            let n = var "n" in
            hoist_conditional ~id ~params:[Ty_int, tag; Ty_int, n] ~default
              ( List.map (fun (k, e) -> [Const_int 1000; Const_int k], e) ints
              @ List.map (fun (k, e) -> [Const_int k; Const_int (-1)], e) blks),
            [Proj (e_val, Ty_int, Tag); Proj (e_val, Ty_int, Val)]
       in
       b_val, Proj (Tapp (Var id, fvs @ args), Ty_pe, Res)
    | Lstaticcatch (e, (n, params), handler) ->
       let old = !catchers in
       catchers := (n,(params,handler)) :: !catchers;
       let b, e = compile e in
       catchers := old;
       b, e
    | Lstaticraise (n, xs) ->
       let params, handler = List.assoc n !catchers in
       List.fold_left2 (fun acc (id,k) e ->
           Lambda.Llet (Strict,k,id,e,acc)) handler params xs
       |> compile
    | Lprim (Pintcomp cmp, [e1; e2], _) ->
       let op =
         match cmp with
         | Ceq -> "=="
         | Cne -> "!="
         | Clt -> "<"
         | Cgt -> ">"
         | Cle -> "<="
         | Cge -> ">="
       in
       intop e1 op e2
    | Lprim (Pnegint, [e], _) ->
       intop (Lconst (Const_int 0)) "-" e
    | Lprim (Paddint, [e1; e2], _) ->
       intop e1 "+" e2
    | Lprim (Psubint, [e1; e2], _) ->
       intop e1 "-" e2
    | Lprim (Pmulint, [e1; e2], _) ->
       intop e1 "*" e2
    | Lprim (Pdivint _, [e1; e2], _) ->
       intop e1 "/" e2
    | Lprim (Pmodint _, [e1; e2], _) ->
       intop e1 "%" e2
    | Lprim (Pandint, [e1; e2], _) ->
       intop e1 "&" e2
    | Lprim (Porint, [e1; e2], _) ->
       intop e1 "|" e2
    | Lprim (Pxorint, [e1; e2], _) ->
       intop e1 "^" e2
    | Lconst (Const_int n) ->
       [], Tapp (Var mk_int, [Const_int n])
    | Lconst (Const_block (tag, args)) ->
       let args = List.map (fun v -> compile (Lconst v)) args in
       List.concat_map fst args, mk_block ~tag (List.map snd args)
    | Lprim (Pmakeblock (tag, _, _), args, _) ->
       let args = List.map compile args in
       List.concat_map fst args, mk_block ~tag (List.map snd args)
    | Lprim (Pfield (n,_,_), [e], _) ->
       let b, e = compile e in
       b, Proj (e, Ty_pe, Field n)
    | Lprim (Psequand, [e1;e2], _) ->
       compile (Lifthenelse (e1, e2, Lconst (Const_int 0)))
    | Lprim (Psequor, [e1;e2], _) ->
       compile (Lifthenelse (e1, Lconst (Const_int 1), e2))
    | Lprim (Praise _, [_e], _) ->
       [], Var mk_raise
    | e ->
       let blank = Ident.create_persistent "_" in
       let e = Lambda.shallow_map (fun _ -> Lvar blank) e in
       failwith (Format.asprintf "Unsupported: %a" Printlambda.lambda e)

  and compile_application e_func args =
    List.fold_left (fun fn arg ->
        Proj (Tapp (Proj (fn, Ty_template, App), [arg]), Ty_pe, Res))
      e_func
      args

  and compile_res e =
    let b, e = compile e in
    b @ [Export Res, Const (Ty_pe, e)]

  and hoist_conditional ~id ~params ~default conds =
    let fvs =
      List.fold_left
        (fun acc (_, e) -> Ident.Set.union acc (Lambda.free_variables e))
        (match default with
         | None -> Ident.Set.empty
         | Some e -> Lambda.free_variables e)
        conds
      |> Ident.Set.to_list
    in
    let conds = conds |> List.map (fun (ps, e) ->
      Struct { params = List.map (fun v -> Ty_pe, v) fvs;
               special = List.map (fun v -> Var v) fvs @ ps;
               body = Some (compile_res e) })
    in
    let default =
      let body = Option.map compile_res default in
      Struct { params = List.map (fun v -> Ty_pe, v) fvs @ params;
               special = [];
               body }
    in
    hoisted := ((Local id, default) :: !hoisted);
    conds |> List.iter (fun s -> hoisted := ((Local id, s) :: !hoisted));
    List.map (fun v -> Var v) fvs

  and compile_lfunction (fn : Lambda.lfunction) =
    match fn with
    | { kind = Curried; params = []; _ } -> assert false
    | { kind = Curried; params = [p, _]; body; _ } ->
       let body =
         match body with
         | Lfunction fn ->
            let fn = compile_lfunction fn in
            [Export Res, fn]
         | body ->
            let b, e = compile body in
            b @ [Export Res, Const (Ty_pe, e)]
       in
       Struct
         {params=[];
          special=[];
          body=Some [
            Export App,
            Struct
              { params=[Ty_pe,p];
                special=[];
                body=Some body}]}
    | { kind = Curried; params = p::params; _ } as fn ->
       compile_lfunction
         (Lambda.lfunction' ~kind:Curried ~params:[p]
            ~return:Pgenval ~attr:fn.attr ~loc:fn.loc ~body:(
              Lambda.lfunction ~kind:Curried ~params
                ~return:fn.return ~attr:fn.attr ~loc:fn.loc
                ~body:fn.body))
    | { kind = Tupled; _ } ->
       (* should never happen in bytecode builds *)
       assert false

  and intop (e1 : Lambda.lambda) op (e2 : Lambda.lambda) =
    let b1, e1 = compile e1 in
    let b2, e2 = compile e2 in
    b1 @ b2,
    Tapp (Var mk_int,
          [Binop (Proj(e1, Ty_int, Val), op, Proj(e2, Ty_int, Val))])
  in
  let ppf = Format.formatter_of_out_channel oc in
  match program.code with
  | Lprim (Psetglobal id, [e], _) when Ident.equal id program.module_ident ->
     let e = Simplif.simplify_lambda e in
     let b, e = compile e in
     let prog = basic_defns () @ List.rev !hoisted @ b in
     let env, prog, param_names =
       match !main with
       | Some (main, param_names) ->
          let params = List.map var param_names in
          let main_app =
            compile_application (Var main)
              (List.map (fun x -> Tapp (Var mk_int, [Var x])) params)
          in
          let output = var "output" in
          List.fold_left Pp.bind Pp.empty_env params,
          prog @ [Local output, Const (Ty_pe, main_app);
                  Export Print, Const (Ty_pe, Proj (Var output, Ty_pe, Print))],
          param_names
       | None ->
          Pp.empty_env, prog @ [Export Res, Const (Ty_pe, e)], []
     in
     param_names |> List.iter (fun p ->
       Format.fprintf ppf "#ifndef %s@." p;
       Format.fprintf ppf "#error \"Parameter %s missing\"@." p;
       Format.fprintf ppf "#include <stop_after_error>@.";
       Format.fprintf ppf "#endif@.");
     Format.fprintf ppf "template <typename...> struct Cons;@.";
     Format.fprintf ppf "template <int tag, typename...> struct Cons_;@.";
     Format.fprintf ppf "@[<v>%a@]@." (Pp.defns ~env) prog
  | l ->
     failwith (Format.asprintf "unexpected lambda: %a@." Printlambda.lambda l)
