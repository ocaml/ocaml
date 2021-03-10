(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Err = Errormod

module FunctorDiff = struct
  (* Simplication for printing *)

  let shortname side pos =
    match side with
    | `Got -> Format.sprintf "$S%d" pos
    | `Expected -> Format.sprintf "$T%d" pos
    | `Unneeded -> "..."

  let to_shortnames ctx patch =
    let to_shortname side pos mty =
      {Err.Short_name. name = (shortname side pos); item = mty; from=None }
    in
    let elide_if_app s = match ctx with
      | `App -> `Unneeded
      | `Sig -> s
    in
    let aux i d =
      let pos = i + 1 in
      let d = match d with
        | Diffing.Insert mty ->
            Diffing.Insert (to_shortname `Expected pos mty)
        | Diffing.Delete mty ->
            Diffing.Delete (to_shortname (elide_if_app `Got) pos mty)
        | Diffing.Change (g, e, p) ->
            Diffing.Change
              (to_shortname `Got pos g,
               to_shortname `Expected pos e, p)
        | Diffing.Keep (g, e, p) ->
            Diffing.Keep (to_shortname `Got pos g,
                       to_shortname (elide_if_app `Expected) pos e, p)
      in
      pos, d
    in
    List.mapi aux patch

  let drop_inserted_suffix patch =
    let rec drop = function
      | Diffing.Insert _ :: q -> drop q
      | rest -> List.rev rest in
    drop (List.rev patch)

  let prepare_patch ~drop ~ctx patch =
    let drop_suffix x = if drop then drop_inserted_suffix x else x in
    patch |> drop_suffix |> to_shortnames ctx

end



module Linearize = struct
  (** Construct a linear presentation of the error tree *)
  open Err

  let with_context ?loc ctx printer diff =
    Location.msg ?loc "%a%a" Context.pp (List.rev ctx)
      printer diff

  let dwith_context ?loc ctx printer =
    Location.msg ?loc "%a%t" Context.pp (List.rev ctx) printer

  let dwith_context_and_elision ?loc ctx printer diff =
    if Err.Pp.is_big (diff.got,diff.expected) then
      Location.msg ?loc "..."
    else
      dwith_context ?loc ctx (printer diff)

  type ('a,'b) patch =
    ( 'a Short_name.item, 'b Short_name.item,
      Typedtree.module_coercion, arg_functor_param_symptom
    ) Diffing.change
  type ('a,'b) t = {
    msgs: Location.msg list;
    post:
      (Env.t * (int * ('a, 'b) patch) list) option
  }

  let rec module_type ~expansion_token ~eqmode ~env ~before ~ctx diff =
    match diff.symptom with
    | Invalid_module_alias _ (* the difference is non-informative here *)
    | After_alias_expansion _ (* we print only the expanded module types *) ->
        module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
          diff.symptom
    | _ ->
        let inner = if eqmode then Pp.eq_module_types else Pp.module_types in
        let before = match diff.symptom with
          | Functor Params _ -> before
          | _ ->
              let next = dwith_context_and_elision ctx inner diff in
              next :: before in
        module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
          diff.symptom

  and module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx = function
    | Mt_core core ->
        begin match Pp.core_module_type_symptom core with
        | None -> { msgs = before; post = None }
        | Some msg ->
            { msgs = Location.msg "%t" msg :: before; post = None }
        end
    | Signature s -> signature ~expansion_token ~env ~before ~ctx s
    | Functor f -> functor_symptom ~expansion_token ~env ~before ~ctx f
    | After_alias_expansion diff ->
        module_type ~eqmode ~expansion_token ~env ~before ~ctx diff
    | Invalid_module_alias path ->
        let printer =
          Format.dprintf "Module %a cannot be aliased" Printtyp.path path
        in
        let msgs = dwith_context ctx printer :: before in
        { msgs; post = None }


  and functor_symptom ~expansion_token ~env ~before ~ctx = function
    | Result res ->
        module_type ~expansion_token ~eqmode:false ~env ~before ~ctx res
    | Params {got; expected; symptom=()} ->
        let d =
          Includemod.FunctorDiff.arg env ctx got expected
          |> FunctorDiff.prepare_patch ~drop:false ~ctx:`Sig
        in
        let actual = Pp.(params_diff space got_app functor_param d) in
        let expected =
          Pp.(params_diff space expected functor_param d)
        in
        let main =
          Format.dprintf
            "@[<hv 2>Modules do not match:@ \
             @[functor@ %t@ -> ...@]@;<1 -2>is not included in@ \
             @[functor@ %t@ -> ...@]@]"
            actual expected
        in
        let post = if expansion_token then Some (env,d) else None in
        { msgs = dwith_context ctx main :: before; post }

  and signature ~expansion_token ~env:_ ~before ~ctx sgs =
    Printtyp.wrap_printing_env ~error:true sgs.env (fun () ->
    match sgs.missings, sgs.incompatibles with
    | a :: l , _ ->
        let more =
          if expansion_token then
            with_context ctx Pp.missing_field a
            :: List.map (Location.msg "%a" Pp.missing_field) l
          else
            []
        in
        let msgs = more @ before in
        { msgs; post = None }
    | [], a :: _ -> sigitem ~expansion_token ~env:sgs.env ~before ~ctx a
    | [], [] -> assert false
      )
  and sigitem ~expansion_token ~env ~before ~ctx (name,s) = match s with
    | Core c ->
        { msgs = dwith_context ctx (Pp.core name c):: before; post = None }
    | Module_type diff ->
        module_type ~expansion_token ~eqmode:false ~env ~before
          ~ctx:(Context.Module name :: ctx) diff
    | Module_type_declaration diff ->
        module_type_decl ~expansion_token ~env ~before ~ctx name diff
  and module_type_decl ~expansion_token ~env ~before ~ctx id diff =
    let next =
      dwith_context_and_elision ctx (Pp.module_type_declarations id) diff in
    let before = next :: before in
    match diff.symptom with
    | Not_less_than mts ->
        let before =
          Location.msg "The first module type is not included in the second"
          :: before in
        module_type ~expansion_token ~eqmode:true ~before ~env
          ~ctx:(Context.Modtype id :: ctx) mts
    | Not_greater_than mts ->
        let before =
          Location.msg "The second module type is not included in the first"
          :: before in
        module_type ~expansion_token ~eqmode:true ~before ~env
          ~ctx:(Context.Modtype id :: ctx) mts
    | Incomparable mts ->
        module_type ~expansion_token ~eqmode:true ~env ~before
          ~ctx:(Context.Modtype id :: ctx) mts.less_than
    | Illegal_permutation c ->
        begin match diff.got.Types.mtd_type with
        | None -> assert false
        | Some mty ->
            let main =
            with_context (Modtype id::ctx)
              (Illegal_permutation.pp Context.alt_pp env) (mty,c) in
            { msgs = main :: before; post = None }
        end

  let module_type_subst ~env id diff =
    match diff.symptom with
    | Not_less_than mts ->
        module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
          ~ctx:[Modtype id] mts
    | Not_greater_than mts ->
        module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
          ~ctx:[Modtype id] mts
    | Incomparable mts ->
        module_type ~expansion_token:true ~eqmode:true ~env ~before:[]
          ~ctx:[Modtype id] mts.less_than
    | Illegal_permutation c ->
        let mty = diff.got in
        let main =
          with_context [Modtype id]
            (Illegal_permutation.pp Context.alt_pp env) (mty,c) in
        { msgs = [main]; post = None }

  let insert_suberror mty =
    Format.dprintf
      "An argument appears to be missing with module type@;<1 2>@[%t@]"
      (Pp.definition_of_functor_param mty)

  let delete_suberror mty =
    Format.dprintf
      "An extra argument is provided of module type@;<1 2>@[%t@]"
      (Pp.definition_of_functor_param mty)

  let delete_suberror_app mty =
    Format.dprintf
      "The following extra argument is provided@;<1 2>@[%t@]"
      (Pp.definition_of_argument mty)

  let ok_suberror x y =
    Format.dprintf
      "Module types %t and %t match"
      (Pp.short_functor_param x)
      (Pp.short_functor_param y)

  let ok_suberror_app x y =
    let pp_orig_name = match Short_name.functor_param y with
      | Short_name.Named (_, Original mty) ->
          Format.dprintf " %t" (Pp.dmodtype mty)
      | _ -> ignore
    in
    Format.dprintf
      "Module %t matches the expected module type%t"
      (Pp.short_argument x)
      pp_orig_name

  let diff_arg g e more =
    let g = Pp.definition_of_functor_param g in
    let e = Pp.definition_of_functor_param e in
    Format.dprintf
      "Module types do not match:@ @[%t@]@;<1 -2>does not include@ \
       @[%t@]%t"
      g e (more ())

  let diff_app g e more =
    let g = Pp.definition_of_argument g in
    let e = Pp.definition_of_functor_param e in
    Format.dprintf
      "Modules do not match:@ @[%t@]@;<1 -2>\
       is not included in@ @[%t@]%t"
      g e (more ())

  let param_subcase sub ~expansion_token env (pos, diff) =
    Location.msg "%a%a%a %a@[<hv 2>%t@]%a"
      Format.pp_print_tab ()
      Format.pp_open_tbox ()
      Pp.prefix (pos, diff)
      Format.pp_set_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )
     Format.pp_close_tbox ()

  let param_onlycase sub ~expansion_token env (_, diff) =
    Location.msg "%a@[<hv 2>%t@]"
      Format.pp_print_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )

  let param_suberrors sub ~expansion_token env l =
    let rec aux = function
      | [] -> []
      | (_, Diffing.Keep _) as a :: q ->
          param_subcase sub ~expansion_token env a
          :: aux q
      | a :: q ->
          param_subcase sub ~expansion_token env a
          :: List.map (param_subcase sub ~expansion_token:false env) q
    in
    match l with
    | [a] -> [param_onlycase sub ~expansion_token env a]
    | l -> aux l

  let arg_incompatible = function
    | Types.Unit ->
        Format.dprintf
          "The functor was expected to be applicative at this position"
    | Types.Named _ ->
        Format.dprintf
          "The functor was expected to be generative at this position"

  let app_incompatible = function
    | Unit_arg ->
        Format.dprintf
          "The functor was expected to be applicative at this position"
    | Named_arg _ | Anonymous _ ->
        Format.dprintf
          "The functor was expected to be generative at this position"


  let rec diff_suberror:
    'a 'b 'c 'd. ('c -> _) -> ('a -> 'b -> _) -> expansion_token:_ -> _ ->
    'a -> 'b -> ('c,'d) Err.functor_param_symptom -> _
    = fun incompatible msg ~expansion_token env g e diff -> match diff with
    | Err.Incompatible_params (i,_) -> incompatible i
    | Err.Mismatch mty_diff ->
        let more () =
          let r =
            module_type_symptom ~eqmode:false ~expansion_token ~env ~before:[]
              ~ctx:[] mty_diff.symptom
          in
          let list l ppf = match l with
            | [] -> ()
            | _ :: _ ->
                Format.fprintf ppf "@;<1 -2>@[%a@]"
                  (Format.pp_print_list ~pp_sep:Pp.space
                     (fun ppf f -> f.Location.txt ppf)
                  )
                  l
          in
          let post = match r.post with
            | None -> []
            | Some (env, patch) ->
                param_suberrors arg ~expansion_token env patch in
          list (List.rev_append r.msgs post) in
        msg g e more

  and arg ~expansion_token env = function
    | Diffing.Insert mty -> insert_suberror mty
    | Diffing.Delete mty -> delete_suberror mty
    | Diffing.Change (g, e, d) ->
        diff_suberror arg_incompatible diff_arg ~expansion_token env g e d
    | Diffing.Keep (x, y, _) -> ok_suberror x y

  let app ~expansion_token env = function
    | Diffing.Insert mty -> insert_suberror mty
    | Diffing.Delete mty -> delete_suberror_app mty
    | Diffing.Change (g, e, d) ->
        diff_suberror app_incompatible diff_app ~expansion_token env g e d
    | Diffing.Keep (x, y, _) -> ok_suberror_app x y

  let all env = function
    | In_Compilation_unit diff ->
      let first = Location.msg "%a" Pp.interface_mismatch diff in
      signature ~expansion_token:true ~env ~before:[first] ~ctx:[] diff.symptom
    | In_Type_declaration (id,reason) ->
        let main = Location.msg "%t" (Pp.core id reason) in
        { msgs = [main]; post = None }
    | In_Module_type diff ->
        module_type ~expansion_token:true ~eqmode:false ~before:[] ~env ~ctx:[]
          diff
    | In_Module_type_substitution (id,diff) ->
        module_type_subst ~env id diff
    | In_Signature diff ->
        signature ~expansion_token:true ~before:[] ~env ~ctx:[] diff
    | In_Expansion cmts ->
        match Pp.core_module_type_symptom cmts with
        | None -> assert false
        | Some main ->
            { msgs = [Location.msg "%t" main]; post = None }

  let coalesce { msgs; _ } =
    match List.rev msgs with
    | [] -> ignore
    | before ->
        let ctx ppf =
          Format.pp_print_list ~pp_sep:Pp.space
            (fun ppf x -> x.Location.txt ppf)
            ppf before in
        ctx
end



let err_msgs (env, err) =
  Printtyp.Conflicts.reset();
  Printtyp.wrap_printing_env ~error:true env (fun () ->
      let l = Linearize.all env err in
      let main = Linearize.coalesce l in
      let sub = match l.Linearize.post with
        | None -> []
        | Some (env,post) ->
            Linearize.(param_suberrors arg) ~expansion_token:true env post in
      sub, main
    )

let report_error err =
  let sub, main = err_msgs err in
  Location.errorf ~loc:Location.(in_file !input_name) ~sub "%t" main

let report_apply_error ~loc env (lid_app, mty_f, args) =
  let may_print_app ppf = match lid_app with
    | None -> ()
    | Some lid -> Format.fprintf ppf "%a " Printtyp.longident lid
  in
  let d =
    Includemod.FunctorDiff.app env ~f:mty_f ~args
    |> FunctorDiff.prepare_patch ~drop:true ~ctx:`App
  in
  match d with
  | [ _, (Diffing.Change _ as c) ] ->
      Location.errorf ~loc "%t" (Linearize.app env ~expansion_token:true c)
  | _ ->
      Location.errorf ~loc
        ~sub:(Linearize.(param_suberrors app) env ~expansion_token:true d)
        "@[<hv>The functor application %tis ill-typed.@ \
         These arguments:@;<1 2>\
         @[%t@]@ do not match these parameters:@;<1 2>@[functor@ %t@ -> ...@]@]"
        may_print_app
        Err.Pp.(params_diff space got_arg short_argument d)
        Err.Pp.(params_diff space expected functor_param d)


(* We could do a better job to split the individual error items
   as sub-messages of the main interface mismatch on the whole unit. *)
let register () =
  Location.register_error_of_exn
    (function
      | Includemod.Error err -> Some (report_error err)
      | Includemod.Apply_error {loc; env; lid_app; mty_f; args} ->
          Some (Printtyp.wrap_printing_env env ~error:true (fun () ->
              report_apply_error ~loc env (lid_app, mty_f, args))
            )
      | _ -> None
    )
