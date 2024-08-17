(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Trace-specific printing *)

(* A configuration type that controls which trace we print.  This could be
   exposed, but we instead expose three separate
   [{unification,equality,moregen}] functions.  This also lets us
   give the unification case an extra optional argument without adding it to the
   equality and moregen cases. *)
type 'variety trace_format =
  | Unification : Errortrace.unification trace_format
  | Equality    : Errortrace.comparison  trace_format
  | Moregen     : Errortrace.comparison  trace_format

let incompatibility_phrase (type variety) : variety trace_format -> string =
  function
  | Unification -> "is not compatible with type"
  | Equality    -> "is not equal to type"
  | Moregen     -> "is not compatible with type"

(* Print a unification error *)
open Out_type
open Format_doc
module Fmt = Format_doc
module Style = Misc.Style

type 'a diff = 'a Out_type.diff = Same of 'a | Diff of 'a * 'a

let trees_of_trace mode =
  List.map (Errortrace.map_diff (trees_of_type_expansion mode))

let rec trace fst txt ppf = function
  | {Errortrace.got; expected} :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@]%a"
       pp_type_expansion got txt pp_type_expansion expected
       (trace false txt) rem
  | _ -> ()

type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

let diff_printing_status Errortrace.{ got      = {ty = t1; expanded = t1'};
                                      expected = {ty = t2; expanded = t2'} } =
  if  Btype.is_constr_row ~allow_ident:true t1'
   || Btype.is_constr_row ~allow_ident:true t2'
  then Discard
  else if same_path t1 t1' && same_path t2 t2' then Optional_refinement
  else Keep

let printing_status = function
  | Errortrace.Diff d -> diff_printing_status d
  | Errortrace.Escape {kind = Constraint} -> Keep
  | _ -> Keep

(** Flatten the trace and remove elements that are always discarded
    during printing *)

(* Takes [printing_status] to change behavior for [Subtype] *)
let prepare_any_trace printing_status tr =
  let clean_trace x l = match printing_status x with
    | Keep -> x :: l
    | Optional_refinement when l = [] -> [x]
    | Optional_refinement | Discard -> l
  in
  match tr with
  | [] -> []
  | elt :: rem -> elt :: List.fold_right clean_trace rem []

let prepare_trace f tr =
  prepare_any_trace printing_status (Errortrace.map f tr)

(** Keep elements that are [Diff _ ] and split the the last element if it is
    optionally elidable, require a prepared trace *)
let rec filter_trace = function
  | [] -> [], None
  | [Errortrace.Diff d as elt]
    when printing_status elt = Optional_refinement -> [], Some d
  | Errortrace.Diff d :: rem ->
      let filtered, last = filter_trace rem in
      d :: filtered, last
  | _ :: rem -> filter_trace rem

let may_prepare_expansion compact (Errortrace.{ty; expanded} as ty_exp) =
  match Types.get_desc expanded with
    Tvariant _ | Tobject _ when compact ->
      Variable_names.reserve ty; Errortrace.{ty; expanded = ty}
  | _ -> prepare_expansion ty_exp

let print_path p =
  Fmt.dprintf "%a" !Oprint.out_ident (namespaced_tree_of_path Type p)

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let print_tags ppf tags  =
  Fmt.(pp_print_list ~pp_sep:comma) print_tag ppf tags

let is_unit env ty =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Tconstr (p, _, _) -> Path.same p Predef.path_unit
  | _ -> false

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Ctype.Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explanation_diff env t3 t4 =
  match Types.get_desc t3, Types.get_desc t4 with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit env ty1 && unifiable env ty2 t4 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to provide %a as argument?@]"
          Style.inline_code "()"
        )
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit env ty1 && unifiable env t3 ty2 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to wrap the expression using \
           %a?@]"
          Style.inline_code "fun () ->"
        )
  | _ ->
      None

let explain_fixed_row_case = function
  | Errortrace.Cannot_be_closed -> doc_printf "it cannot be closed"
  | Errortrace.Cannot_add_tags tags ->
      doc_printf "it may not allow the tag(s) %a"
        print_tags tags

let pp_path ppf p =
  Style.as_inline_code Printtyp.Doc.path ppf p

let explain_fixed_row pos expl = match expl with
  | Types.Fixed_private ->
    doc_printf "The %a variant type is private" Errortrace.print_pos pos
  | Types.Univar x ->
    Variable_names.reserve x;
    doc_printf "The %a variant type is bound to the universal type variable %a"
      Errortrace.print_pos pos
      (Style.as_inline_code type_expr_with_reserved_names) x
  | Types.Reified p ->
    doc_printf "The %a variant type is bound to %a"
      Errortrace.print_pos pos
      (Style.as_inline_code
         (fun ppf p ->
           Internal_names.add p;
           print_path p ppf))
      p
  | Types.Rigid -> Format_doc.Doc.empty

let explain_variant (type variety) : variety Errortrace.variant -> _ = function
  (* Common *)
  | Errortrace.Incompatible_types_for s ->
      Some(doc_printf "@,Types for tag %a are incompatible"
             print_tag s
          )
  (* Unification *)
  | Errortrace.No_intersection ->
      Some(doc_printf "@,These two variant types have no intersection")
  | Errortrace.No_tags(pos,fields) -> Some(
      doc_printf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        Errortrace.print_pos pos
        print_tags (List.map fst fields)
    )
  | Errortrace.Fixed_row (pos,
                          k,
                          (Univar _ | Reified _ | Fixed_private as e)) ->
      Some (
        doc_printf "@,@[%a,@ %a@]" pp_doc (explain_fixed_row pos e)
          pp_doc (explain_fixed_row_case k)
      )
  | Errortrace.Fixed_row (_,_, Rigid) ->
      (* this case never happens *)
      None
  (* Equality & Moregen *)
  | Errortrace.Presence_not_guaranteed_for (pos, s) -> Some(
      doc_printf
        "@,@[The tag %a is guaranteed to be present in the %a variant type,\
         @ but not in the %a@]"
        print_tag s
        Errortrace.print_pos (Errortrace.swap_position pos)
        Errortrace.print_pos pos
    )
  | Errortrace.Openness pos ->
      Some(doc_printf "@,The %a variant type is open and the %a is not"
             Errortrace.print_pos pos
             Errortrace.print_pos (Errortrace.swap_position pos))

let explain_escape pre = function
  | Errortrace.Univ u ->
      Variable_names.reserve u;
      Some(
        doc_printf "%a@,The universal variable %a would escape its scope"
          pp_doc pre
          (Style.as_inline_code type_expr_with_reserved_names) u
      )
  | Errortrace.Constructor p -> Some(
      doc_printf
        "%a@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        pp_doc pre pp_path p
    )
  | Errortrace.Module_type p -> Some(
      doc_printf
        "%a@,@[The module type@;<1 2>%a@ would escape its scope@]"
        pp_doc pre pp_path p
    )
  | Errortrace.Equation Errortrace.{ty = _; expanded = t} ->
      Variable_names.reserve t;
      Some(
        doc_printf "%a@ @[<hov>This instance of %a is ambiguous:@ %s@]"
          pp_doc pre
          (Style.as_inline_code type_expr_with_reserved_names) t
          "it would escape the scope of its equation"
      )
  | Errortrace.Self ->
      Some (doc_printf "%a@,Self type cannot escape its class" pp_doc pre)
  | Errortrace.Constraint ->
      None

let explain_object (type variety) : variety Errortrace.obj -> _ = function
  | Errortrace.Missing_field (pos,f) -> Some(
      doc_printf "@,@[The %a object type has no method %a@]"
        Errortrace.print_pos pos Style.inline_code f
    )
  | Errortrace.Abstract_row pos -> Some(
      doc_printf
        "@,@[The %a object type has an abstract row, it cannot be closed@]"
        Errortrace.print_pos pos
    )
  | Errortrace.Self_cannot_be_closed ->
      Some (doc_printf
              "@,Self type cannot be unified with a closed object type"
           )

let explain_incompatible_fields name (diff: Types.type_expr Errortrace.diff) =
  Variable_names.reserve diff.got;
  Variable_names.reserve diff.expected;
  doc_printf "@,@[The method %a has type@ %a,@ \
  but the expected method type was@ %a@]"
    Style.inline_code name
    (Style.as_inline_code type_expr_with_reserved_names) diff.got
    (Style.as_inline_code type_expr_with_reserved_names) diff.expected


let explain_label_mismatch ~got ~expected =
  let quoted_label ppf l = Style.inline_code ppf (Asttypes.string_of_label l) in
  match got, expected with
  | Asttypes.Nolabel, Asttypes.(Labelled _ | Optional _ )  ->
      doc_printf "@,@[A label@ %a@ was expected@]"
        quoted_label expected
  | Asttypes.(Labelled _|Optional _), Asttypes.Nolabel  ->
      doc_printf
        "@,@[The first argument is labeled@ %a,@ \
         but an unlabeled argument was expected@]"
        quoted_label got
 | Asttypes.Labelled g, Asttypes.Optional e when g = e ->
      doc_printf
        "@,@[The label@ %a@ was expected to be optional@]"
        quoted_label got
  | Asttypes.Optional g, Asttypes.Labelled e when g = e ->
      doc_printf
        "@,@[The label@ %a@ was expected to not be optional@]"
        quoted_label got
  | Asttypes.(Labelled _ | Optional _), Asttypes.(Labelled _ | Optional _) ->
      doc_printf "@,@[Labels %a@ and@ %a do not match@]"
        quoted_label got
        quoted_label expected
  | Asttypes.Nolabel, Asttypes.Nolabel ->
      (* Two empty labels cannot be mismatched*)
      assert false


let explain_first_class_module = function
  | Errortrace.Package_cannot_scrape p -> Some(
      doc_printf "@,@[The module alias %a could not be expanded@]"
        pp_path p
    )
  | Errortrace.Package_inclusion pr ->
      Some(doc_printf "@,@[%a@]" Fmt.pp_doc pr)
  | Errortrace.Package_coercion pr ->
      Some(doc_printf "@,@[%a@]" Fmt.pp_doc pr)

let explanation (type variety) intro prev env
  : (Errortrace.expanded_type, variety) Errortrace.elt -> _ = function
  | Errortrace.Diff {got; expected} ->
    explanation_diff env got.expanded expected.expanded
  | Errortrace.Escape {kind; context} ->
    let pre =
      match context, kind, prev with
      | Some ctx, _, _ ->
        Variable_names.reserve ctx;
        doc_printf "@[%a@;<1 2>%a@]" pp_doc intro
          (Style.as_inline_code type_expr_with_reserved_names) ctx
      | None, Univ _, Some(Errortrace.Incompatible_fields {name; diff}) ->
        explain_incompatible_fields name diff
      | _ -> Format_doc.Doc.empty
    in
    explain_escape pre kind
  | Errortrace.Incompatible_fields { name; diff} ->
    Some(explain_incompatible_fields name diff)
  | Errortrace.Function_label_mismatch diff ->
    Some(explain_label_mismatch ~got:diff.got ~expected:diff.expected)
  | Errortrace.Variant v ->
    explain_variant v
  | Errortrace.Obj o ->
    explain_object o
  | Errortrace.First_class_module fm ->
    explain_first_class_module fm
  | Errortrace.Rec_occur(x,y) ->
    add_type_to_preparation x;
    add_type_to_preparation y;
    begin match Types.get_desc x with
    | Tvar _ | Tunivar _  ->
        Some(
          doc_printf "@,@[<hov>The type variable %a occurs inside@ %a@]"
            (Style.as_inline_code prepared_type_expr) x
            (Style.as_inline_code prepared_type_expr) y
        )
    | _ ->
        (* We had a delayed unification of the type variable with
           a non-variable after the occur check. *)
        Some Format_doc.Doc.empty
        (* There is no need to search further for an explanation, but
           we don't want to print a message of the form:
             {[ The type int occurs inside int list -> 'a |}
        *)
    end

let mismatch intro env trace =
  Errortrace.explain trace (fun ~prev h -> explanation intro prev env h)

let warn_on_missing_def env ppf t =
  match Types.get_desc t with
  | Tconstr (p,_,_) ->
    begin match Env.find_type p env with
    | exception Not_found ->
        fprintf ppf
          "@,@[<hov>Type %a is abstract because@ no corresponding\
           @ cmi file@ was found@ in path.@]" pp_path p
    | { type_manifest = Some _; _ } -> ()
    | { type_manifest = None; _ } as decl ->
        match Btype.type_origin decl with
        | Rec_check_regularity ->
            fprintf ppf
              "@,@[<hov>Type %a was considered abstract@ when checking\
               @ constraints@ in this@ recursive type definition.@]"
              pp_path p
        | Definition | Existential _ -> ()
      end
  | _ -> ()

let prepare_expansion_head empty_tr = function
  | Errortrace.Diff d ->
      Some (Errortrace.map_diff (may_prepare_expansion empty_tr) d)
  | _ -> None

let head_error_printer mode txt_got txt_but = function
  | None -> Format_doc.Doc.empty
  | Some d ->
      let d = Errortrace.map_diff (trees_of_type_expansion mode) d in
      doc_printf "%a@;<1 2>%a@ %a@;<1 2>%a"
        pp_doc txt_got pp_type_expansion d.Errortrace.got
        pp_doc txt_but pp_type_expansion d.Errortrace.expected

let warn_on_missing_defs env ppf = function
  | None -> ()
  | Some Errortrace.{got      = {ty=te1; expanded=_};
                     expected = {ty=te2; expanded=_} } ->
      warn_on_missing_def env ppf te1;
      warn_on_missing_def env ppf te2

(* [subst] comes out of equality, and is [[]] otherwise *)
let error trace_format mode subst env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  (* We want to substitute in the opposite order from [Eqtype] *)
  Variable_names.add_subst (List.map (fun (ty1,ty2) -> ty2,ty1) subst);
  let tr =
    prepare_trace
      (fun ty_exp ->
         Errortrace.{ty_exp with expanded = hide_variant_name ty_exp.expanded})
      tr
  in
  match tr with
  | [] -> assert false
  | (elt :: tr) as full_trace ->
      with_labels (not !Clflags.classic) (fun () ->
      let tr, last = filter_trace tr in
      let head = prepare_expansion_head (tr=[] && last=None) elt in
      let tr = List.map (Errortrace.map_diff prepare_expansion) tr in
      let last = Option.map (Errortrace.map_diff prepare_expansion) last in
      let head_error = head_error_printer mode txt1 txt2 head in
      let tr = trees_of_trace mode tr in
      let last =
        Option.map (Errortrace.map_diff (trees_of_type_expansion mode)) last in
      let mis = mismatch txt1 env full_trace in
      let tr = match mis, last with
        | None, Some elt -> tr @ [elt]
        | Some _, _ | _, None -> tr
       in
       fprintf ppf
        "@[<v>\
          @[%a%a@]%a%a\
         @]"
        pp_doc head_error
        pp_doc ty_expect_explanation
        (trace false (incompatibility_phrase trace_format)) tr
        (pp_print_option pp_doc) mis;
      if env <> Env.empty
      then warn_on_missing_defs env ppf head;
       Internal_names.print_explanations env ppf;
       Ident_conflicts.err_print ppf
    )

let report_error trace_format ppf mode env tr
      ?(subst = [])
      ?(type_expected_explanation = Fmt.Doc.empty)
      txt1 txt2 =
  wrap_printing_env ~error:true env (fun () ->
    error trace_format mode subst env tr txt1 ppf txt2
      type_expected_explanation)

let unification
      ppf env ({trace} : Errortrace.unification_error) =
  report_error Unification ppf Type env
    ?subst:None trace

let equality
      ppf mode env ({subst; trace} : Errortrace.equality_error) =
  report_error Equality ppf mode env
    ~subst ?type_expected_explanation:None trace

let moregen
      ppf mode env ({trace} : Errortrace.moregen_error) =
  report_error Moregen ppf mode env
    ?subst:None ?type_expected_explanation:None trace

let comparison ppf mode env = function
  | Errortrace.Equality_error error -> equality ppf mode env error
  | Errortrace.Moregen_error  error -> moregen  ppf mode env error

module Subtype = struct
  (* There's a frustrating amount of code duplication between this module and
     the outside code, particularly in [prepare_trace] and [filter_trace].
     Unfortunately, [Subtype] is *just* similar enough to have code duplication,
     while being *just* different enough (it's only [Diff]) for the abstraction
     to be nonobvious.  Someday, perhaps... *)

  let printing_status = function
    | Errortrace.Subtype.Diff d -> diff_printing_status d

  let prepare_unification_trace = prepare_trace

  let prepare_trace f tr =
    prepare_any_trace printing_status (Errortrace.Subtype.map f tr)

  let trace filter_trace get_diff fst keep_last txt ppf tr =
    with_labels (not !Clflags.classic) (fun () ->
      match tr with
      | elt :: tr' ->
        let diffed_elt = get_diff elt in
        let tr, last = filter_trace tr' in
        let tr = match keep_last, last with
          | true, Some last -> tr @ [last]
          | _ -> tr
        in
        let tr =
          trees_of_trace Type
          @@ List.map (Errortrace.map_diff prepare_expansion) tr in
        let tr =
          match fst, diffed_elt with
          | true, Some elt -> elt :: tr
          | _, _ -> tr
        in
        trace fst txt ppf tr
      | _ -> ()
    )

  let rec filter_subtype_trace = function
    | [] -> [], None
    | [Errortrace.Subtype.Diff d as elt]
      when printing_status elt = Optional_refinement ->
        [], Some d
    | Errortrace.Subtype.Diff d :: rem ->
        let ftr, last = filter_subtype_trace rem in
        d :: ftr, last

  let unification_get_diff = function
    | Errortrace.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)
    | _ -> None

  let subtype_get_diff = function
    | Errortrace.Subtype.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)

  let error
        ppf
        env
        (Errortrace.Subtype.{trace = tr_sub; unification_trace = tr_unif})
        txt1 =
    wrap_printing_env ~error:true env (fun () ->
      reset ();
      let tr_sub = prepare_trace prepare_expansion tr_sub in
      let tr_unif = prepare_unification_trace prepare_expansion tr_unif in
      let keep_first = match tr_unif with
        | [Obj _ | Variant _ | Escape _ ] | [] -> true
        | _ -> false in
      fprintf ppf "@[<v>%a"
        (trace filter_subtype_trace subtype_get_diff true keep_first txt1)
        tr_sub;
      if tr_unif = [] then fprintf ppf "@]" else
        let mis = mismatch (doc_printf "Within this type") env tr_unif in
        fprintf ppf "%a%a%t@]"
          (trace filter_trace unification_get_diff false
             (mis = None) "is not compatible with type") tr_unif
          (pp_print_option pp_doc) mis
          Ident_conflicts.err_print
    )
end

let subtype = Subtype.error

let quoted_ident ppf t =
  Style.as_inline_code !Oprint.out_ident ppf t

let type_path_expansion ppf = function
  | Same p -> quoted_ident ppf p
  | Diff(p,p') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
       quoted_ident p
       quoted_ident p'

let trees_of_type_path_expansion (tp,tp') =
  let path_tree = namespaced_tree_of_path Type in
  if Path.same tp tp' then Same(path_tree tp) else
    Diff(path_tree tp, path_tree tp)

let type_path_list ppf l =
  Fmt.pp_print_list ~pp_sep:(fun ppf () -> Fmt.pp_print_break ppf 2 0)
    type_path_expansion ppf l

let ambiguous_type ppf env tp0 tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tp0 = trees_of_type_path_expansion tp0 in
      match tpl with
      [] -> assert false
    | [tp] ->
        fprintf ppf
          "@[%a@;<1 2>%a@ \
             %a@;<1 2>%a\
           @]"
          pp_doc txt1 type_path_expansion (trees_of_type_path_expansion tp)
          pp_doc txt3 type_path_expansion tp0
    | _ ->
        fprintf ppf
          "@[%a@;<1 2>@[<hv>%a@]\
             @ %a@;<1 2>%a\
           @]"
          pp_doc txt2 type_path_list (List.map trees_of_type_path_expansion tpl)
          pp_doc txt3 type_path_expansion tp0)
