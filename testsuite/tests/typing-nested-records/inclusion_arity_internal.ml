(* TEST
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/typing";
 expect;
*)

(* Build mismatched compiler-libs declarations directly because source declarations now have canonical arity. *)

open Types

let mk_label ?inlined name ty =
  { ld_id = Ident.create_local name;
    ld_mutable = Asttypes.Immutable;
    ld_atomic = Asttypes.Nonatomic;
    ld_type = ty;
    ld_inlined = inlined;
    ld_loc = Location.none;
    ld_attributes = [];
    ld_uid = Uid.internal_not_actually_unique;
  }

let mk_record_decl params labels =
  { type_params = params;
    type_arity = List.length params;
    type_kind = Type_record (labels, Record_regular);
    type_private = Asttypes.Public;
    type_manifest = None;
    type_variance = List.map (fun _ -> Variance.full) params;
    type_separability = List.map (fun _ -> Separability.Ind) params;
    type_is_newtype = false;
    type_expansion_scope = Btype.lowest_level;
    type_loc = Location.none;
    type_attributes = [];
    type_immediate = Type_immediacy.Unknown;
    type_unboxed_default = false;
    type_uid = Uid.internal_not_actually_unique;
  }
[%%expect{|
val mk_label :
  ?inlined:Types.type_declaration ->
  string -> Types.type_expr -> Types.label_declaration = <fun>
val mk_record_decl :
  Types.type_expr list ->
  Types.label_declaration list -> Types.type_declaration = <fun>
|}]

(* Use one path with different argument counts to exercise the guard before [Ctype.equal]. *)

let nested_path = Path.Pident (Ident.create_local "t.n")

let nested arity =
  let params = List.init arity (fun _ -> Btype.newgenvar ()) in
  mk_record_decl params [ mk_label "value" Predef.type_int ]

let outer nested_decl =
  let n_type =
    Btype.newgenty (Tconstr (nested_path, nested_decl.type_params, ref Mnil))
  in
  mk_record_decl [] [ mk_label "n" ~inlined:nested_decl n_type ]

let check d1 d2 =
  match
    Includecore.type_declarations
      ~loc:Location.none Env.initial ~mark:false "t"
      d1 (Path.Pident (Ident.create_local "t")) d2
  with
  | exception exn -> "raised " ^ Printexc.to_string exn
  | None -> "no mismatch"
  | Some (Includecore.Record_mismatch (Includecore.Label_mismatch changes)) ->
      let nested_reported =
        List.exists
          (function
            | Diffing_with_keys.Change
                (Diffing_with_keys.Type { reason = Includecore.Nested_record; _ })
              -> true
            | _ -> false)
          changes
      in
      if nested_reported then "nested-record mismatch"
      else "record mismatch without nested reason"
  | Some _ -> "unexpected mismatch kind"
[%%expect{|
val nested_path : Path.t = Path.Pident <abstr>
val nested : int -> Types.type_declaration = <fun>
val outer : Types.type_declaration -> Types.type_declaration = <fun>
val check : Types.type_declaration -> Types.type_declaration -> string =
  <fun>
|}]

(* Unequal arities must report a nested-record mismatch in both directions. *)

let one_vs_two = check (outer (nested 1)) (outer (nested 2))
[%%expect{|
val one_vs_two : string = "nested-record mismatch"
|}]

let two_vs_one = check (outer (nested 2)) (outer (nested 1))
[%%expect{|
val two_vs_one : string = "nested-record mismatch"
|}]

(* Equal nested arities keep matching. *)

let one_vs_one = check (outer (nested 1)) (outer (nested 1))
[%%expect{|
val one_vs_one : string = "no mismatch"
|}]
