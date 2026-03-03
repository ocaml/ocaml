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
module Structured = Errortrace.Structured

type 'a diff = 'a Out_type.diff = Same of 'a | Diff of 'a * 'a

let trees_of_trace mode l =
  List.map (Errortrace.map_cdiff (trees_of_type_expansion mode)) l

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let syntactic_highlighting l r = match l, r with
  | Same l, Same r ->
      let l, r = Syntactic_highlighting.diff l r in
      Same l, Same r
  | Same l, Diff (short, expanded) ->
      let l, expanded = Syntactic_highlighting.diff l expanded in
      Same l, Diff (short, expanded)
  | Diff (short, expanded), Same r ->
      let expanded, r = Syntactic_highlighting.diff expanded r in
      Diff(short,expanded), Same r
  | Diff (short, expanded), Diff (short', expanded') ->
      let short, short' = Syntactic_highlighting.diff short short' in
      let expanded, expanded' =
        Syntactic_highlighting.diff expanded expanded' in
      Diff (short, expanded), Diff (short', expanded')

let rec trace fst txt ppf = function
  | elt :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[%a@]%a"
      (trace_elt txt) elt
      (trace false txt) rem
  | [] -> ()
and trace_elt txt ppf = function
  | { ctx; Errortrace.d = {got; expected} } ->
      let got, expected = syntactic_highlighting got expected in
      match ctx with
      | None ->
          fprintf ppf "Type@;<1 2>%a@ %s@;<1 2>%a"
            pp_type_expansion got txt pp_type_expansion expected
      | Some (In_tag l) ->
          fprintf ppf "In tag %a, type@;<1 2>%a@ %s@;<1 2>%a"
            print_tag l
            pp_type_expansion got txt pp_type_expansion expected
      | Some (In_method m) ->
          fprintf ppf
            "@,@[The method %a has type@ %a,@ \
             but the expected method type was@ %a@]"
            Style.inline_code m
            pp_type_expansion got
            pp_type_expansion expected


let diff_printing_status Errortrace.{ got      = {ty = t1; expanded = t1'};
                                      expected = {ty = t2; expanded = t2'} } =
  if  Btype.is_constr_row ~allow_ident:true t1'
   || Btype.is_constr_row ~allow_ident:true t2'
  then Structured.Discard
  else if same_path t1 t1' && same_path t2 t2' then
    Structured.Optional_refinement
  else Structured.Keep

let printing_status = function
  | { Errortrace.ctx = Some _; _} -> Structured.Context
  | d -> diff_printing_status d.Errortrace.d

let is_unit_param env ty =
  let ty, vars = Btype.tpoly_get_poly ty in
  if vars <> [] then false
  else begin
    match Types.get_desc (Ctype.expand_head env ty) with
    | Tconstr (p, _, _) -> Path.same p Predef.path_unit
    | _ -> false
  end

let unifiable env ty1 ty2 =
    try Ctype.unify env ty1 ty2; true
    with Ctype.Unify _ -> false

let promote_diff env {Errortrace.got; expected} =
  let snap = Btype.snapshot () in
  let t3, t4 = Errortrace.(got.expanded, expected.expanded) in
  let res = match Types.get_desc t3, Types.get_desc t4 with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit_param env ty1 && unifiable env ty2 t4 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to provide %a as argument?@]"
          Style.inline_code "()"
        )
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit_param env ty1 && unifiable env t3 ty2 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to wrap the expression using \
           %a?@]"
          Style.inline_code "fun () ->"
        )
  | _ -> None
  in
  Btype.backtrack snap;
  res

let may_prepare_expansion compact (ty_exp, htarget) =
  let Errortrace.{ty; expanded} = ty_exp in
  match Types.get_desc expanded with
    Tvariant _ | Tobject _ when compact ->
      Variable_names.reserve ty; Errortrace.{ty; expanded = ty}, htarget
  | _ -> prepare_expansion ty_exp, htarget

let print_path p =
  Fmt.dprintf "%a" !Oprint.out_ident (namespaced_tree_of_path Type p)

let print_tags ppf tags  =
  Fmt.(pp_print_list ~pp_sep:comma) print_tag ppf tags

let both_side_diff f x = Errortrace.no_ctx {got = f x; expected = f x}

let both_side x  = both_side_diff (Errortrace.highlight_type Independent) x
let both_side_constructor p =
  both_side_diff (fun p -> [Errortrace.Type_constructor p] ) p


let no_highlight = Errortrace.no_ctx { Errortrace.got = []; expected = []}

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


let highlight_fixed_row expl = match expl with
  | Types.Fixed_private | Types.Rigid -> no_highlight
  | Types.Univar x -> both_side x
  | Types.Reified p -> both_side_constructor p


let explain_variant (type variety) : variety Errortrace.variant -> _ = function
  (* Common *)
  | Errortrace.Arity_mismatch s ->
      Some(doc_printf "@,@[Arities for tag %a are incompatible.@]"
             print_tag s)
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


let highlight_variant (type variety) : variety Errortrace.variant -> _ =
  function
  | Errortrace.Arity_mismatch _
  | Errortrace.No_intersection
  | Errortrace.No_tags(_,_)
  | Errortrace.Presence_not_guaranteed_for _
  | Errortrace.Openness _ -> no_highlight
  | Errortrace.Fixed_row (_, _, e) ->  highlight_fixed_row e

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
  | Errortrace.Module id -> Some(
      doc_printf
        "%a@,@[The module@;<1 2>%a@ would escape its scope@]"
        pp_doc pre pp_path (Path.Pident id)
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

let highlight_escape = function
  | Errortrace.Univ u -> both_side u
  | Errortrace.Constructor p -> both_side_constructor p
  | Errortrace.Module_type _
  | Errortrace.Module _ -> no_highlight
  | Errortrace.Equation Errortrace.{ty = _; expanded = t} -> both_side t
  | Errortrace.Self
  | Errortrace.Constraint -> no_highlight

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

let explain_label_mismatch ~missing_label_msg  {Errortrace.got;expected} =
  let quoted_label ppf l = Style.inline_code ppf (Asttypes.string_of_label l) in
  match got, expected with
  | Asttypes.Nolabel, Asttypes.(Labelled _ | Optional _ )  ->
      doc_printf "@,@[A label@ %a@ was expected@]"
        quoted_label expected
  | Asttypes.(Labelled _|Optional _), Asttypes.Nolabel  ->
      doc_printf missing_label_msg
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

let explain_univar = function
  | Errortrace.Var_mismatch { diff; order} ->
      add_type_to_preparation diff.got;
      add_type_to_preparation diff.expected;
      let more = match order with
        | Equal ->  Fmt.Doc.empty
        | Less ->
          Fmt.doc_printf
            "@ The first type variable %a was introduced in@ an@ earlier@ \
             universal@ quantification."
              (Style.as_inline_code prepared_type_expr) diff.got
        | More ->
            Fmt.doc_printf
              "@ The second type variable %a was introduced in@ an@ earlier@ \
               universal@ quantification."
              (Style.as_inline_code prepared_type_expr) diff.expected
      in
      doc_printf
        "@,@[The universal variables@ %a and@ %a@ are distinct.%a@]"
        (Style.as_inline_code prepared_type_expr) diff.got
        (Style.as_inline_code prepared_type_expr) diff.expected
        pp_doc more
  | Errortrace.Quantification_mismatch delta ->
      let qp ppf x = Style.as_inline_code prepared_type_expr ppf x in
      let pp ppf ty =
        add_type_to_preparation ty;
        match Types.get_desc ty with
        | Tunivar None -> ()
        | Tunivar (Some name) ->
            Fmt.fprintf ppf
              "@,@[The universal type variable %a in the first@ type@ matches@ \
               multiple@ distinct@ variables in the second type.@]"
              Style.inline_code ("'" ^ name)
        | Tvar _ ->
              Fmt.fprintf ppf
                "@,@[The type variable %a is not generalizable@ to@ an@ \
                 universal@ type variable.@]"
                qp ty
        | _ ->
              Fmt.fprintf ppf
                "@,@[The type %a is not a type variable.@]"
                qp ty
      in
      let pp_sep _ () = () in
      doc_printf "%a" (pp_print_list ~pp_sep pp) delta

let highlight_univar = function
  | Errortrace.Var_mismatch { diff; order=_} ->
      Errortrace.no_ctx
      @@ Errortrace.map_diff (Errortrace.highlight_type Independent) diff
  | Errortrace.Quantification_mismatch delta ->
      let delta = List.map (fun t -> Errortrace.Type(Independent,t)) delta in
      Errortrace.no_ctx {Errortrace.got = delta; expected = delta }

let explanation (type variety) intro
  : (Errortrace.expanded_type, variety) Errortrace.root -> _ = function
  | Errortrace.Escape {kind; context} ->
    let pre =
      match context, kind with
      | Some ctx, _ ->
        Variable_names.reserve ctx;
        doc_printf "@[%a@;<1 2>%a@]" pp_doc intro
          (Style.as_inline_code type_expr_with_reserved_names) ctx
      | _ -> Format_doc.Doc.empty
    in
    explain_escape pre kind
  | Errortrace.Function_label_mismatch diff ->
    let missing_label_msg =
      format_of_string
        "@,@[The first argument is labeled@ %a,@ \
         but an unlabeled argument was expected@]"
    in
    Some(explain_label_mismatch ~missing_label_msg diff)
  | Errortrace.Tuple_label_mismatch diff ->
    let ast_label = function
      | None -> Asttypes.Nolabel
      | Some x -> Asttypes.Labelled x
    in
    let diff = Errortrace.map_diff ast_label diff in
    let missing_label_msg =
      format_of_string
        "@,@[The first tuple element is labeled@ %a,@ \
         but an unlabeled element was expected@]"
    in
    Some(explain_label_mismatch ~missing_label_msg diff)
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
  | Univar um -> Some (explain_univar um)
  | Highlight_hint _ -> None

let highlight_explanation_core (type variety)
  : (Errortrace.expanded_type, variety) Errortrace.root -> _ = function
  | Errortrace.Escape {kind; context = _ } -> highlight_escape kind
  | Errortrace.Function_label_mismatch _
  | Errortrace.Tuple_label_mismatch _ -> no_highlight
  | Errortrace.Variant v -> highlight_variant v
  | Errortrace.Obj _
  | Errortrace.First_class_module _ -> no_highlight
  | Errortrace.Rec_occur(x,y) ->
      let got = Errortrace.highlight_type Paired x in
      let expected = Errortrace.highlight_type Paired y in
      Errortrace.no_ctx { got; expected }
  | Errortrace.Univar um -> highlight_univar um
  | Errortrace.Highlight_hint h -> Errortrace.no_ctx h

let highlight_explanation = function
  | None | Some (Structured.Promoted(None,_))-> no_highlight
  | Some (Structured.Promoted (Some hint,_)) | Some (Structured.Hint hint) ->
      Errortrace.no_ctx hint
  | Some (Structured.Standard std) -> highlight_explanation_core std

let mismatch intro expl =
  match expl with
  | None | Some (Structured.Hint _) -> None
  | Some (Structured.Promoted (_,msg)) -> Some msg
  | Some (Structured.Standard e) -> explanation intro e

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
        | Approx_recmod ->
            fprintf ppf
              "@,@[<hov>Type %a was considered abstract@ when checking\
               @ constraints@ in this@ recursive module definition.@]"
              pp_path p
        | Equation _ | Definition | Existential _ -> ()
      end
  | _ -> ()

let prepare_expansion_head (h,empty_tr)=
  Errortrace.map_cdiff (may_prepare_expansion empty_tr) h

let head_error_printer mode txt_got txt_but = function
  | None -> Format_doc.Doc.empty
  | Some d ->
      let d = Errortrace.(map_diff (trees_of_type_expansion mode) d.d) in
      let got, expected = syntactic_highlighting d.got d.expected in
      doc_printf "%a@;<1 2>%a@ %a@;<1 2>%a"
        pp_doc txt_got pp_type_expansion got
        pp_doc txt_but pp_type_expansion expected

let warn_on_missing_defs env ppf = function
  | None -> ()
  | Some Errortrace.{ d = { got      = {ty=te1; expanded=_}, _;
                            expected = {ty=te2; expanded=_}, _ }; _ } ->
      warn_on_missing_def env ppf te1;
      warn_on_missing_def env ppf te2

let pp_print_list_comma_and elt ppf l =
  match List.rev l with
  | [] -> ()
  | [ single ] ->
      fprintf ppf "%a" elt single
  | fst :: rest ->
      fprintf
        ppf
        "%a@ and %a"
        (pp_print_list ~pp_sep:comma elt) (List.rev rest)
        elt fst

let quoted_ident ppf t =
  Style.as_inline_code !Oprint.out_ident ppf t

let pp_plural (singular, plural) ppf l =
  match l with
  | [ _ ] -> pp_print_string ppf singular
  | _ -> pp_print_string ppf plural

let explain_names env ppf =
  let explanations = Internal_names.explain env in
  List.iter
    (function
      | _, Internal_names.Equation { lhs; rhs; } ->
          add_type_to_preparation lhs;
          add_type_to_preparation rhs;
      | _, Internal_names.Existential _ ->
          ()
    ) explanations;
  List.iter
    (fun (paths, explanation) ->
       let paths = List.map tree_of_path paths in
       match explanation with
       | Internal_names.Equation { lhs; rhs; } ->
           let rhseq = tree_of_typexp [] Type_scheme rhs in
           let lhseq = tree_of_typexp [] Type_scheme lhs in
           fprintf ppf
             "@ @[<2>@{<hint>Hint@}:@ %a@ %a@ \
              introduced in the equation@ %a = %a@]"
             (pp_print_list_comma_and quoted_ident) paths
             (pp_plural ("is a type variable", "are type variables")) paths
             (Style.as_inline_code !Oprint.out_type)
             lhseq
             (Style.as_inline_code !Oprint.out_type)
             rhseq
       | Internal_names.Existential { constructor } ->
           fprintf ppf
             "@ @[<2>@{<hint>Hint@}:@ %a@ %a@ \
              bound by the constructor@ %a.@]"
             (pp_print_list_comma_and quoted_ident) paths
             (pp_plural ("is an existential type", "are existential types"))
             paths
             Style.inline_code constructor
    ) explanations

let hide_variant ty_exp =
  Errortrace.{ty_exp with expanded = hide_variant_name ty_exp.expanded}

let zip_cdiff x y =
  let open Errortrace in
  let d = {
        got = x.d.got, y.d.got;
        expected = x.d.expected, y.d.expected
      }
  in
  { ctx = x.ctx; d }

let highlight_type ty =
  let hty x = Errortrace.highlight_type Paired x.Errortrace.ty in
  Errortrace.map_cdiff hty ty

let associate_htarget { Structured.top; tr; expl} =
  match top, tr with
  | None, _ -> { Structured.top = None; tr = []; expl }
  | Some (h,c), [] ->
      let h = zip_cdiff h (highlight_explanation expl) in
      { Structured.top = Some (h, c); tr = []; expl }
  | Some (h,c), a :: q ->
      let top = Some (zip_cdiff h (highlight_type a), c) in
      let hexpl = highlight_explanation expl in
      let htrace = List.map highlight_type q @ [hexpl] in
      let tr = List.map2 zip_cdiff tr htrace in
      { Structured.top; tr; expl }

let structured_trace env tr =
  associate_htarget
  @@ Structured.parse ~promote:(promote_diff env) ~status:printing_status tr

let prepare_trace_expansion tr =
  let expand_elt (x,h) = prepare_expansion x, h in
  List.map (Errortrace.map_cdiff expand_elt) tr

(* [subst] comes out of equality, and is [[]] otherwise *)
let error trace_format mode subst env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  (* We want to substitute in the opposite order from [Eqtype] *)
  Variable_names.add_subst (List.map (fun (ty1,ty2) -> ty2,ty1) subst);
  let tr = Errortrace.map hide_variant tr in
  let str = structured_trace env tr in
  with_labels (not !Clflags.classic) (fun () ->
      let head = Option.map prepare_expansion_head str.top in
      let head_error = head_error_printer mode txt1 txt2 head in
      let tr = prepare_trace_expansion str.tr in
      let tr = trees_of_trace mode tr in
      let mis = mismatch txt1 str.expl in
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
      explain_names env ppf;
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

  let prepare_trace { Errortrace.Subtype.trace; unification_trace } =
    let trace = Errortrace.Subtype.map prepare_expansion trace in
    let unification_trace =
      Errortrace.map prepare_expansion unification_trace in
    Errortrace.Subtype.error ~trace ~unification_trace

  let parse_sub tr =
    (* The subtype part of the trace does not contain any explanation *)
    let trace = { Errortrace.root = None; path = tr } in
    associate_htarget
      (Structured.parse ~promote:(Fun.const None) ~status:printing_status trace)

  let flatten_trace filter_trace fst (str: _ Structured.s) =
    with_labels (not !Clflags.classic) (fun () ->
      match str.top, str.tr with
      | Some (elt,_), tr ->
        let diffed_elt =
          Errortrace.map_cdiff (trees_of_type_expansion Type) elt
        in
        let tr = trees_of_trace Type (filter_trace tr) in
        if fst then diffed_elt :: tr else tr
      | None, _ -> []
    )

  let subtyping_printing_status elt =
    printing_status (Errortrace.map_cdiff fst elt)

  let rec filter_trace keep_last = function
    | [] -> []
    | [elt] when
        subtyping_printing_status elt = Structured.Optional_refinement ->
        if keep_last then [elt] else []
    | d :: rem -> d :: filter_trace keep_last rem

  let obj_only_trace (trace: _ Structured.s) =
    match trace.top, trace.tr, trace.expl with
    | None, [], Some (Standard (Obj _ | Variant _ | Escape _ ))
    | None, [], (None | Some (Hint _)) -> true
    | _ -> false

  let error ppf env tr txt1 =
    wrap_printing_env ~error:true env (fun () ->
      reset ();
      let tr = prepare_trace tr in
      let tr_sub = parse_sub tr.trace in
      let str_unif = structured_trace env tr.unification_trace in
      let keep_last = obj_only_trace str_unif in
      let tr_sub = flatten_trace (filter_trace keep_last) true tr_sub in
      let tr_unif = flatten_trace Fun.id false str_unif in
      let mis = mismatch (doc_printf "Within this type") str_unif.expl in
      fprintf ppf "@[<v>%a%a%a%t@]"
        (trace true txt1) tr_sub
        (trace false "is not compatible with type") tr_unif
        (pp_print_option pp_doc) mis
        Ident_conflicts.err_print
    )
end

let subtype = Subtype.error

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
