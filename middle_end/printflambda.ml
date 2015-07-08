(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let fprintf = Format.fprintf
module Int = Ext_types.Int

let rec lam ppf (flam : _ Flambda.t) =
  match flam with
  | Var (id,_) ->
      Variable.print ppf id
  | Apply({func; args; kind},_) ->
    let direct = match kind with Indirect -> "" | Direct _ -> "*" in
    fprintf ppf "@[<2>(apply%s@ %a%a)@]" direct Variable.print func
      Variable.print_list args
  | Assign(id, expr,_) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Variable.print id lam expr
  | Send (k, met, obj, largs, _,_) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Lambda.Self then "self" else if k = Lambda.Cached then "cache" else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs
  | Unreachable _ ->
      fprintf ppf "unreachable"
  | Let(_str, id, arg, body,_) ->
      let rec letbody (ul : _ Flambda.t) =
        match ul with
        | Let(str, id, arg, body,_) ->
            let str = match str with
              | Mutable -> "*"
              | Immutable -> ""
            in
            fprintf ppf "@ @[<2>%a%s@ %a@]" Variable.print id str lam_named arg;
            letbody body
        | _ -> ul
      in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]"
        Variable.print id lam_named arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Let_rec(id_arg_list, body,_) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<2>%a@ %a@]" Variable.print id lam_named l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Switch(larg, sw,_) ->
      let switch ppf (sw : _ Flambda.switch) =
        let spc = ref false in
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
          sw.consts;
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.blocks ;
        begin match sw.failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
        "@[<1>(%s(%i,%i) %a@ @[<v 0>%a@])@]"
        (match sw.failaction with None -> "switch*" | _ -> "switch")
        (Int.Set.cardinal sw.numconsts)
        (Int.Set.cardinal sw.numblocks)
        lam larg switch sw
  | String_switch(arg, cases, default, _) ->
      let switch ppf cases =
        let spc = ref false in
        List.iter
         (fun (s, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
          cases;
        begin match default with
        | Some default ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam default
        | None -> ()
        end in
      fprintf ppf
       "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" lam arg switch cases
  | Static_raise (i, ls,_)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %a%a)@]" Static_exception.print i lams ls;
  | Static_catch(i, vars, lbody, lhandler,_) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%a%a)@ %a)@]"
        lam lbody Static_exception.print i
        (fun ppf vars -> match vars with
           | [] -> ()
           | _ ->
               List.iter
                 (fun x -> fprintf ppf " %a" Variable.print x)
                 vars)
        vars
        lam lhandler
  | Try_with(lbody, param, lhandler,_) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Variable.print param lam lhandler
  | If_then_else(lcond, lif, lelse,_) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | While(lcond, lbody,_) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | For(param, lo, hi, dir, body,_) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
        Variable.print param lam lo
        (match dir with Asttypes.Upto -> "to" | Asttypes.Downto -> "downto")
        lam hi lam body
and lam_named ppf (named : _ Flambda.named) =
  match named with
  | Symbol (symbol,_) -> Symbol.print ppf symbol
  | Const (cst,_) -> const ppf cst
  | Project_closure (project_closure, _) ->
    print_project_closure ppf project_closure
  | Project_var (project_var, _) -> print_project_var ppf project_var
  | Move_within_set_of_closures (move_within_set_of_closures, _) ->
    print_move_within_set_of_closures ppf move_within_set_of_closures
  | Set_of_closures (set_of_closures, _) ->
    print_set_of_closures ppf set_of_closures
  | Prim(prim, args, _,_) ->
    fprintf ppf "@[<2>(%a%a)@]" Printlambda.primitive prim
      Variable.print_list args
  | Expr expr -> lam ppf expr

and print_set_of_closures ppf (set_of_closures : _ Flambda.set_of_closures) =
  match set_of_closures with
  | { function_decls; free_vars; specialised_args} ->
    let idents ppf =
      List.iter (fprintf ppf "@ %a" Variable.print) in
    let funs ppf =
      Variable.Map.iter (fun var (f : _ Flambda.function_declaration) ->
          fprintf ppf "@ (fun %a@[<2>%a@]@ @[<2>%a@])"
            Variable.print var idents f.params lam f.body) in
    let vars ppf =
      Variable.Map.iter (fun id v -> fprintf ppf "@ %a = %a"
                      Variable.print id Variable.print v) in
    let spec ppf spec_args =
      if not (Variable.Map.is_empty spec_args)
      then begin
        fprintf ppf "@ with";
        Variable.Map.iter (fun id id' -> fprintf ppf "@ %a <- %a"
                        Variable.print id Variable.print id')
          spec_args
      end
    in
    fprintf ppf "@[<2>(set_of_closures%a %a%a)@]" funs function_decls.funs
      vars free_vars spec specialised_args

and print_project_closure ppf (project_closure : Flambda.project_closure) =
  fprintf ppf "@[<2>(project_closure@ %a@ %a)@]"
    Closure_id.print project_closure.closure_id
    Variable.print project_closure.set_of_closures

and print_move_within_set_of_closures ppf
      (move_within_set_of_closures : Flambda.move_within_set_of_closures) =
  fprintf ppf "@[<2>(move_within_set_of_closures@ %a - %a@ %a)@]"
    Closure_id.print move_within_set_of_closures.move_to
    Closure_id.print move_within_set_of_closures.start_from
    Variable.print move_within_set_of_closures.closure

and print_project_var ppf (project_var : Flambda.project_var) =
  fprintf ppf "@[<2>(project_var@ %a@ %a@ %a)@]"
    Var_within_closure.print project_var.var
    Closure_id.print project_var.closure_id
    Variable.print project_var.closure

and const ppf (c : Flambda.const) =
  match c with
  | Const_base(Const_int n) -> fprintf ppf "%i" n
  | Const_base(Const_char c) -> fprintf ppf "%C" c
  | Const_base(Const_string (s,_)) -> fprintf ppf "%S" s
  | Const_immstring s -> fprintf ppf "#%S" s
  | Const_base(Const_float f) -> fprintf ppf "%s" f
  | Const_base(Const_int32 n) -> fprintf ppf "%lil" n
  | Const_base(Const_int64 n) -> fprintf ppf "%LiL" n
  | Const_base(Const_nativeint n) -> fprintf ppf "%nin" n
  | Const_pointer n -> fprintf ppf "%ia" n
  | Const_float f -> fprintf ppf "%f" f
  | Const_float_array [] ->
      fprintf ppf "[| |]"
  | Const_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl

let function_declarations ppf (fd : _ Flambda.function_declarations) =
  let idents ppf =
    List.iter (fprintf ppf "@ %a" Variable.print) in
  let funs ppf =
    Variable.Map.iter (fun var (f : _ Flambda.function_declaration) ->
        fprintf ppf "@ (fun@ %a@[<2>%a@]@ @[<2>%a@])"
          Variable.print var idents f.params lam f.body) in
  fprintf ppf "@[<2>(%a)@]" funs fd.funs

let flambda ppf flam =
  fprintf ppf "%a@." lam flam

let project_closure = print_project_closure
let move_within_set_of_closures = print_move_within_set_of_closures
let project_var = print_project_var
