module R =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
    module Id =
      struct
        let name = "Camlp4OCamlRevisedParser"
          
        let version = Sys.ocaml_version
          
      end
      
    module Make (Syntax : Sig.Camlp4Syntax) =
      struct
        open Sig
          
        include Syntax
          
        (* Camlp4_config.constructors_arity.val := True; *)
        let _ = Camlp4_config.constructors_arity := false
          
        let help_sequences () =
          (Printf.eprintf
             "\
New syntax:\
\n    (e1; e2; ... ; en) OR begin e1; e2; ... ; en end\
\n    while e do e1; e2; ... ; en done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en done\
\nOld syntax (still supported):\
\n    do {e1; e2; ... ; en}\
\n    while e do {e1; e2; ... ; en}\
\n    for v = v1 to/downto v2 do {e1; e2; ... ; en}\
\nVery old (no more supported) syntax:\
\n    do e1; e2; ... ; en-1; return en\
\n    while e do e1; e2; ... ; en; done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en; done\
\n";
           flush stderr;
           exit 1)
          
        let _ =
          Options.add "-help_seq" (Arg.Unit help_sequences)
            "Print explanations about new sequences and exit."
          
        let _ = Gram.Entry.clear a_CHAR
          
        let _ = Gram.Entry.clear a_FLOAT
          
        let _ = Gram.Entry.clear a_INT
          
        let _ = Gram.Entry.clear a_INT32
          
        let _ = Gram.Entry.clear a_INT64
          
        let _ = Gram.Entry.clear a_LABEL
          
        let _ = Gram.Entry.clear a_LIDENT
          
        let _ = Gram.Entry.clear a_NATIVEINT
          
        let _ = Gram.Entry.clear a_OPTLABEL
          
        let _ = Gram.Entry.clear a_STRING
          
        let _ = Gram.Entry.clear a_UIDENT
          
        let _ = Gram.Entry.clear a_ident
          
        let _ = Gram.Entry.clear amp_ctyp
          
        let _ = Gram.Entry.clear and_ctyp
          
        let _ = Gram.Entry.clear match_case
          
        let _ = Gram.Entry.clear match_case0
          
        let _ = Gram.Entry.clear match_case_quot
          
        let _ = Gram.Entry.clear binding
          
        let _ = Gram.Entry.clear binding_quot
          
        let _ = Gram.Entry.clear rec_binding_quot
          
        let _ = Gram.Entry.clear class_declaration
          
        let _ = Gram.Entry.clear class_description
          
        let _ = Gram.Entry.clear class_expr
          
        let _ = Gram.Entry.clear class_expr_quot
          
        let _ = Gram.Entry.clear class_fun_binding
          
        let _ = Gram.Entry.clear class_fun_def
          
        let _ = Gram.Entry.clear class_info_for_class_expr
          
        let _ = Gram.Entry.clear class_info_for_class_type
          
        let _ = Gram.Entry.clear class_longident
          
        let _ = Gram.Entry.clear class_longident_and_param
          
        let _ = Gram.Entry.clear class_name_and_param
          
        let _ = Gram.Entry.clear class_sig_item
          
        let _ = Gram.Entry.clear class_sig_item_quot
          
        let _ = Gram.Entry.clear class_signature
          
        let _ = Gram.Entry.clear class_str_item
          
        let _ = Gram.Entry.clear class_str_item_quot
          
        let _ = Gram.Entry.clear class_structure
          
        let _ = Gram.Entry.clear class_type
          
        let _ = Gram.Entry.clear class_type_declaration
          
        let _ = Gram.Entry.clear class_type_longident
          
        let _ = Gram.Entry.clear class_type_longident_and_param
          
        let _ = Gram.Entry.clear class_type_plus
          
        let _ = Gram.Entry.clear class_type_quot
          
        let _ = Gram.Entry.clear comma_ctyp
          
        let _ = Gram.Entry.clear comma_expr
          
        let _ = Gram.Entry.clear comma_ipatt
          
        let _ = Gram.Entry.clear comma_patt
          
        let _ = Gram.Entry.clear comma_type_parameter
          
        let _ = Gram.Entry.clear constrain
          
        let _ = Gram.Entry.clear constructor_arg_list
          
        let _ = Gram.Entry.clear constructor_declaration
          
        let _ = Gram.Entry.clear constructor_declarations
          
        let _ = Gram.Entry.clear ctyp
          
        let _ = Gram.Entry.clear ctyp_quot
          
        let _ = Gram.Entry.clear cvalue_binding
          
        let _ = Gram.Entry.clear direction_flag
          
        let _ = Gram.Entry.clear dummy
          
        let _ = Gram.Entry.clear eq_expr
          
        let _ = Gram.Entry.clear expr
          
        let _ = Gram.Entry.clear expr_eoi
          
        let _ = Gram.Entry.clear expr_quot
          
        let _ = Gram.Entry.clear field_expr
          
        let _ = Gram.Entry.clear field_expr_list
          
        let _ = Gram.Entry.clear fun_binding
          
        let _ = Gram.Entry.clear fun_def
          
        let _ = Gram.Entry.clear ident
          
        let _ = Gram.Entry.clear ident_quot
          
        let _ = Gram.Entry.clear implem
          
        let _ = Gram.Entry.clear interf
          
        let _ = Gram.Entry.clear ipatt
          
        let _ = Gram.Entry.clear ipatt_tcon
          
        let _ = Gram.Entry.clear label
          
        let _ = Gram.Entry.clear label_declaration
          
        let _ = Gram.Entry.clear label_declaration_list
          
        let _ = Gram.Entry.clear label_expr_list
          
        let _ = Gram.Entry.clear label_expr
          
        let _ = Gram.Entry.clear label_ipatt
          
        let _ = Gram.Entry.clear label_ipatt_list
          
        let _ = Gram.Entry.clear label_longident
          
        let _ = Gram.Entry.clear label_patt
          
        let _ = Gram.Entry.clear label_patt_list
          
        let _ = Gram.Entry.clear labeled_ipatt
          
        let _ = Gram.Entry.clear let_binding
          
        let _ = Gram.Entry.clear meth_list
          
        let _ = Gram.Entry.clear meth_decl
          
        let _ = Gram.Entry.clear module_binding
          
        let _ = Gram.Entry.clear module_binding0
          
        let _ = Gram.Entry.clear module_binding_quot
          
        let _ = Gram.Entry.clear module_declaration
          
        let _ = Gram.Entry.clear module_expr
          
        let _ = Gram.Entry.clear module_expr_quot
          
        let _ = Gram.Entry.clear module_longident
          
        let _ = Gram.Entry.clear module_longident_with_app
          
        let _ = Gram.Entry.clear module_rec_declaration
          
        let _ = Gram.Entry.clear module_type
          
        let _ = Gram.Entry.clear module_type_quot
          
        let _ = Gram.Entry.clear more_ctyp
          
        let _ = Gram.Entry.clear name_tags
          
        let _ = Gram.Entry.clear opt_as_lident
          
        let _ = Gram.Entry.clear opt_class_self_patt
          
        let _ = Gram.Entry.clear opt_class_self_type
          
        let _ = Gram.Entry.clear opt_comma_ctyp
          
        let _ = Gram.Entry.clear opt_dot_dot
          
        let _ = Gram.Entry.clear opt_eq_ctyp
          
        let _ = Gram.Entry.clear opt_expr
          
        let _ = Gram.Entry.clear opt_meth_list
          
        let _ = Gram.Entry.clear opt_mutable
          
        let _ = Gram.Entry.clear opt_polyt
          
        let _ = Gram.Entry.clear opt_private
          
        let _ = Gram.Entry.clear opt_rec
          
        let _ = Gram.Entry.clear opt_virtual
          
        let _ = Gram.Entry.clear opt_when_expr
          
        let _ = Gram.Entry.clear patt
          
        let _ = Gram.Entry.clear patt_as_patt_opt
          
        let _ = Gram.Entry.clear patt_eoi
          
        let _ = Gram.Entry.clear patt_quot
          
        let _ = Gram.Entry.clear patt_tcon
          
        let _ = Gram.Entry.clear phrase
          
        let _ = Gram.Entry.clear poly_type
          
        let _ = Gram.Entry.clear row_field
          
        let _ = Gram.Entry.clear sem_expr
          
        let _ = Gram.Entry.clear sem_expr_for_list
          
        let _ = Gram.Entry.clear sem_patt
          
        let _ = Gram.Entry.clear sem_patt_for_list
          
        let _ = Gram.Entry.clear semi
          
        let _ = Gram.Entry.clear sequence
          
        let _ = Gram.Entry.clear sig_item
          
        let _ = Gram.Entry.clear sig_item_quot
          
        let _ = Gram.Entry.clear sig_items
          
        let _ = Gram.Entry.clear star_ctyp
          
        let _ = Gram.Entry.clear str_item
          
        let _ = Gram.Entry.clear str_item_quot
          
        let _ = Gram.Entry.clear str_items
          
        let _ = Gram.Entry.clear top_phrase
          
        let _ = Gram.Entry.clear type_constraint
          
        let _ = Gram.Entry.clear type_declaration
          
        let _ = Gram.Entry.clear type_ident_and_parameters
          
        let _ = Gram.Entry.clear type_kind
          
        let _ = Gram.Entry.clear type_longident
          
        let _ = Gram.Entry.clear type_longident_and_parameters
          
        let _ = Gram.Entry.clear type_parameter
          
        let _ = Gram.Entry.clear type_parameters
          
        let _ = Gram.Entry.clear typevars
          
        let _ = Gram.Entry.clear use_file
          
        let _ = Gram.Entry.clear val_longident
          
        let _ = Gram.Entry.clear value_let
          
        let _ = Gram.Entry.clear value_val
          
        let _ = Gram.Entry.clear with_constr
          
        let _ = Gram.Entry.clear with_constr_quot
          
        let neg_string n =
          let len = String.length n
          in
            if (len > 0) && (n.[0] = '-')
            then String.sub n 1 (len - 1)
            else "-" ^ n
          
        let mkumin _loc f arg =
          match arg with
          | Ast.ExInt (_, n) -> Ast.ExInt (_loc, (neg_string n))
          | Ast.ExInt32 (_, n) -> Ast.ExInt32 (_loc, (neg_string n))
          | Ast.ExInt64 (_, n) -> Ast.ExInt64 (_loc, (neg_string n))
          | Ast.ExNativeInt (_, n) -> Ast.ExNativeInt (_loc, (neg_string n))
          | Ast.ExFlo (_, n) -> Ast.ExFlo (_loc, (neg_string n))
          | _ ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc, (Ast.IdLid (_loc, ("~" ^ f))))), arg)
          
        let mklistexp _loc last =
          let rec loop top =
            function
            | [] ->
                (match last with
                 | Some e -> e
                 | None -> Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))
            | e1 :: el ->
                let _loc =
                  if top then _loc else Loc.merge (Ast.loc_of_expr e1) _loc
                in
                  Ast.ExApp (_loc,
                    (Ast.ExApp (_loc,
                       (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e1)),
                    (loop false el))
          in loop true
          
        let mkassert _loc =
          function
          | Ast.ExId (_, (Ast.IdUid (_, "False"))) -> Ast.ExAsf _loc
          | (* this case takes care about
                                   the special assert false node *)
              e -> Ast.ExAsr (_loc, e)
          
        let append_eLem el e = el @ [ e ]
          
        let mk_anti ?(c = "") n s = "\\$" ^ (n ^ (c ^ (":" ^ s)))
          
        let mksequence _loc =
          function
          | (Ast.ExSem (_, _, _) | Ast.ExAnt (_, _) as e) ->
              Ast.ExSeq (_loc, e)
          | e -> e
          
        let mksequence' _loc =
          function
          | (Ast.ExSem (_, _, _) as e) -> Ast.ExSeq (_loc, e)
          | e -> e
          
        let rec lid_of_ident =
          function
          | Ast.IdAcc (_, _, i) -> lid_of_ident i
          | Ast.IdLid (_, lid) -> lid
          | _ -> assert false
          
        let module_type_app mt1 mt2 =
          match (mt1, mt2) with
          | (Ast.MtId (_loc, i1), Ast.MtId (_, i2)) ->
              Ast.MtId (_loc, (Ast.IdApp (_loc, i1, i2)))
          | _ -> raise Stream.Failure
          
        let module_type_acc mt1 mt2 =
          match (mt1, mt2) with
          | (Ast.MtId (_loc, i1), Ast.MtId (_, i2)) ->
              Ast.MtId (_loc, (Ast.IdAcc (_loc, i1, i2)))
          | _ -> raise Stream.Failure
          
        let bigarray_get _loc arr arg =
          let coords =
            match arg with
            | Ast.ExTup (_, (Ast.ExCom (_, e1, e2))) | Ast.ExCom (_, e1, e2)
                -> Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
            | _ -> [ arg ]
          in
            match coords with
            | [ c1 ] ->
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Bigarray")),
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Array1")),
                              (Ast.IdLid (_loc, "get")))))))),
                     arr)),
                  c1)
            | [ c1; c2 ] ->
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExApp (_loc,
                        (Ast.ExId (_loc,
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Bigarray")),
                              (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Array2")),
                                 (Ast.IdLid (_loc, "get")))))))),
                        arr)),
                     c1)),
                  c2)
            | [ c1; c2; c3 ] ->
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExApp (_loc,
                        (Ast.ExApp (_loc,
                           (Ast.ExId (_loc,
                              (Ast.IdAcc (_loc,
                                 (Ast.IdUid (_loc, "Bigarray")),
                                 (Ast.IdAcc (_loc,
                                    (Ast.IdUid (_loc, "Array3")),
                                    (Ast.IdLid (_loc, "get")))))))),
                           arr)),
                        c1)),
                     c2)),
                  c3)
            | (* | coords -> <:expr< Bigarray.Genarray.get $arr$ [| $list:coords$ |] >> ] *)
                coords ->
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Bigarray")),
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Genarray")),
                              (Ast.IdLid (_loc, "get")))))))),
                     arr)),
                  (Ast.ExArr (_loc, (Ast.exSem_of_list coords))))
          
        let bigarray_set _loc var newval =
          match var with
          | Ast.ExApp (_,
              (Ast.ExApp (_,
                 (Ast.ExId (_,
                    (Ast.IdAcc (_, (Ast.IdUid (_, "Bigarray")),
                       (Ast.IdAcc (_, (Ast.IdUid (_, "Array1")),
                          (Ast.IdLid (_, "get")))))))),
                 arr)),
              c1) ->
              Some
                (Ast.ExApp (_loc,
                   (Ast.ExApp (_loc,
                      (Ast.ExApp (_loc,
                         (Ast.ExId (_loc,
                            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Bigarray")),
                               (Ast.IdAcc (_loc,
                                  (Ast.IdUid (_loc, "Array1")),
                                  (Ast.IdLid (_loc, "set")))))))),
                         arr)),
                      c1)),
                   newval))
          | Ast.ExApp (_,
              (Ast.ExApp (_,
                 (Ast.ExApp (_,
                    (Ast.ExId (_,
                       (Ast.IdAcc (_, (Ast.IdUid (_, "Bigarray")),
                          (Ast.IdAcc (_, (Ast.IdUid (_, "Array2")),
                             (Ast.IdLid (_, "get")))))))),
                    arr)),
                 c1)),
              c2) ->
              Some
                (Ast.ExApp (_loc,
                   (Ast.ExApp (_loc,
                      (Ast.ExApp (_loc,
                         (Ast.ExApp (_loc,
                            (Ast.ExId (_loc,
                               (Ast.IdAcc (_loc,
                                  (Ast.IdUid (_loc, "Bigarray")),
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Array2")),
                                     (Ast.IdLid (_loc, "set")))))))),
                            arr)),
                         c1)),
                      c2)),
                   newval))
          | Ast.ExApp (_,
              (Ast.ExApp (_,
                 (Ast.ExApp (_,
                    (Ast.ExApp (_,
                       (Ast.ExId (_,
                          (Ast.IdAcc (_, (Ast.IdUid (_, "Bigarray")),
                             (Ast.IdAcc (_, (Ast.IdUid (_, "Array3")),
                                (Ast.IdLid (_, "get")))))))),
                       arr)),
                    c1)),
                 c2)),
              c3) ->
              Some
                (Ast.ExApp (_loc,
                   (Ast.ExApp (_loc,
                      (Ast.ExApp (_loc,
                         (Ast.ExApp (_loc,
                            (Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Bigarray")),
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Array3")),
                                        (Ast.IdLid (_loc, "set")))))))),
                               arr)),
                            c1)),
                         c2)),
                      c3)),
                   newval))
          | Ast.ExApp (_,
              (Ast.ExApp (_,
                 (Ast.ExId (_,
                    (Ast.IdAcc (_, (Ast.IdUid (_, "Bigarray")),
                       (Ast.IdAcc (_, (Ast.IdUid (_, "Genarray")),
                          (Ast.IdLid (_, "get")))))))),
                 arr)),
              (Ast.ExArr (_, coords))) ->
              Some
                (Ast.ExApp (_loc,
                   (Ast.ExApp (_loc,
                      (Ast.ExApp (_loc,
                         (Ast.ExId (_loc,
                            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Bigarray")),
                               (Ast.IdAcc (_loc,
                                  (Ast.IdUid (_loc, "Genarray")),
                                  (Ast.IdLid (_loc, "set")))))))),
                         arr)),
                      (Ast.ExArr (_loc, coords)))),
                   newval))
          | _ -> None
          
        let stopped_at _loc = Some (Loc.move_line 1 _loc)
          
        (* FIXME be more precise *)
        let rec generalized_type_of_type =
          function
          | Ast.TyArr (_, t1, t2) ->
              let (tl, rt) = generalized_type_of_type t2 in ((t1 :: tl), rt)
          | t -> ([], t)
          
        let symbolchar =
          let list =
            [ '$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '=';
              '>'; '?'; '@'; '^'; '|'; '~'; '\\' ] in
          let rec loop s i =
            if i == (String.length s)
            then true
            else if List.mem s.[i] list then loop s (i + 1) else false
          in loop
          
        let setup_op_parser entry p =
          Gram.Entry.setup_parser entry
            (fun (__strm : _ Stream.t) ->
               match Stream.peek __strm with
               | Some (((KEYWORD x | SYMBOL x), ti)) when p x ->
                   (Stream.junk __strm;
                    let _loc = Gram.token_location ti
                    in Ast.ExId (_loc, (Ast.IdLid (_loc, x))))
               | _ -> raise Stream.Failure)
          
        let _ =
          let list = [ '!'; '?'; '~' ] in
          let excl = [ "!="; "??" ]
          in
            setup_op_parser prefixop
              (fun x ->
                 (not (List.mem x excl)) &&
                   (((String.length x) >= 2) &&
                      ((List.mem x.[0] list) && (symbolchar x 1))))
          
        let _ =
          let list_ok =
            [ "<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$" ] in
          let list_first_char_ok = [ '='; '<'; '>'; '|'; '&'; '$'; '!' ] in
          let excl = [ "<-"; "||"; "&&" ]
          in
            setup_op_parser infixop0
              (fun x ->
                 (List.mem x list_ok) ||
                   ((not (List.mem x excl)) &&
                      (((String.length x) >= 2) &&
                         ((List.mem x.[0] list_first_char_ok) &&
                            (symbolchar x 1)))))
          
        let _ =
          let list = [ '@'; '^' ]
          in
            setup_op_parser infixop1
              (fun x ->
                 ((String.length x) >= 1) &&
                   ((List.mem x.[0] list) && (symbolchar x 1)))
          
        let _ =
          let list = [ '+'; '-' ]
          in
            setup_op_parser infixop2
              (fun x ->
                 (x <> "->") &&
                   (((String.length x) >= 1) &&
                      ((List.mem x.[0] list) && (symbolchar x 1))))
          
        let _ =
          let list = [ '*'; '/'; '%'; '\\' ]
          in
            setup_op_parser infixop3
              (fun x ->
                 ((String.length x) >= 1) &&
                   ((List.mem x.[0] list) &&
                      (((x.[0] <> '*') ||
                          (((String.length x) < 2) || (x.[1] <> '*')))
                         && (symbolchar x 1))))
          
        let _ =
          setup_op_parser infixop4
            (fun x ->
               ((String.length x) >= 2) &&
                 ((x.[0] == '*') && ((x.[1] == '*') && (symbolchar x 2))))
          
        let rec infix_kwds_filter (__strm : _ Stream.t) =
          match Stream.peek __strm with
          | Some (((KEYWORD "(", _) as tok)) ->
              (Stream.junk __strm;
               let xs = __strm in
               let (__strm : _ Stream.t) = xs
               in
                 (match Stream.peek __strm with
                  | Some
                      ((KEYWORD
                          (("or" | "mod" | "land" | "lor" | "lxor" | "lsl" |
                              "lsr" | "asr" | "*"
                            as i)),
                        _loc))
                      ->
                      (Stream.junk __strm;
                       (match Stream.peek __strm with
                        | Some ((KEYWORD ")", _)) ->
                            (Stream.junk __strm;
                             let xs = __strm
                             in
                               Stream.lcons (fun _ -> ((LIDENT i), _loc))
                                 (Stream.slazy
                                    (fun _ -> infix_kwds_filter xs)))
                        | _ -> raise (Stream.Error "")))
                  | _ ->
                      let xs = __strm
                      in
                        Stream.icons tok
                          (Stream.slazy (fun _ -> infix_kwds_filter xs))))
          | Some x ->
              (Stream.junk __strm;
               let xs = __strm
               in
                 Stream.icons x
                   (Stream.slazy (fun _ -> infix_kwds_filter xs)))
          | _ -> raise Stream.Failure
          
        let _ =
          Token.Filter.define_filter (Gram.get_filter ())
            (fun f strm -> infix_kwds_filter (f strm))
          
        let _ =
          Gram.Entry.setup_parser sem_expr
            (let symb1 = Gram.parse_tokens_after_filter expr in
             let symb (__strm : _ Stream.t) =
               match Stream.peek __strm with
               | Some ((ANTIQUOT ((("list" as n)), s), ti)) ->
                   (Stream.junk __strm;
                    let _loc = Gram.token_location ti
                    in Ast.ExAnt (_loc, (mk_anti ~c: "expr;" n s)))
               | _ -> symb1 __strm in
             let rec kont al (__strm : _ Stream.t) =
               match Stream.peek __strm with
               | Some ((KEYWORD ";", _)) ->
                   (Stream.junk __strm;
                    let a =
                      (try symb __strm
                       with | Stream.Failure -> raise (Stream.Error "")) in
                    let s = __strm in
                    let _loc =
                      Loc.merge (Ast.loc_of_expr al) (Ast.loc_of_expr a)
                    in kont (Ast.ExSem (_loc, al, a)) s)
               | _ -> al
             in
               fun (__strm : _ Stream.t) ->
                 let a = symb __strm in kont a __strm)
          
        let _ =
          let apply () =
            let _ = (a_CHAR : 'a_CHAR Gram.Entry.t)
            and _ = (override_flag_quot : 'override_flag_quot Gram.Entry.t)
            and _ = (row_var_flag_quot : 'row_var_flag_quot Gram.Entry.t)
            and _ = (virtual_flag_quot : 'virtual_flag_quot Gram.Entry.t)
            and _ = (private_flag_quot : 'private_flag_quot Gram.Entry.t)
            and _ = (mutable_flag_quot : 'mutable_flag_quot Gram.Entry.t)
            and _ = (direction_flag_quot : 'direction_flag_quot Gram.Entry.t)
            and _ = (rec_flag_quot : 'rec_flag_quot Gram.Entry.t)
            and _ = (package_type : 'package_type Gram.Entry.t)
            and _ = (do_sequence : 'do_sequence Gram.Entry.t)
            and _ = (infixop4 : 'infixop4 Gram.Entry.t)
            and _ = (infixop3 : 'infixop3 Gram.Entry.t)
            and _ = (infixop2 : 'infixop2 Gram.Entry.t)
            and _ = (infixop1 : 'infixop1 Gram.Entry.t)
            and _ = (infixop0 : 'infixop0 Gram.Entry.t)
            and _ = (with_constr_quot : 'with_constr_quot Gram.Entry.t)
            and _ = (with_constr : 'with_constr Gram.Entry.t)
            and _ = (value_val : 'value_val Gram.Entry.t)
            and _ = (value_let : 'value_let Gram.Entry.t)
            and _ = (val_longident : 'val_longident Gram.Entry.t)
            and _ = (use_file : 'use_file Gram.Entry.t)
            and _ = (typevars : 'typevars Gram.Entry.t)
            and _ = (type_parameters : 'type_parameters Gram.Entry.t)
            and _ = (type_parameter : 'type_parameter Gram.Entry.t)
            and _ =
              (type_longident_and_parameters :
                'type_longident_and_parameters Gram.Entry.t)
            and _ = (type_longident : 'type_longident Gram.Entry.t)
            and _ = (type_kind : 'type_kind Gram.Entry.t)
            and _ =
              (type_ident_and_parameters :
                'type_ident_and_parameters Gram.Entry.t)
            and _ = (type_declaration : 'type_declaration Gram.Entry.t)
            and _ = (type_constraint : 'type_constraint Gram.Entry.t)
            and _ = (top_phrase : 'top_phrase Gram.Entry.t)
            and _ = (str_items : 'str_items Gram.Entry.t)
            and _ = (str_item_quot : 'str_item_quot Gram.Entry.t)
            and _ = (str_item : 'str_item Gram.Entry.t)
            and _ = (star_ctyp : 'star_ctyp Gram.Entry.t)
            and _ = (sig_items : 'sig_items Gram.Entry.t)
            and _ = (sig_item_quot : 'sig_item_quot Gram.Entry.t)
            and _ = (sig_item : 'sig_item Gram.Entry.t)
            and _ = (sequence : 'sequence Gram.Entry.t)
            and _ = (semi : 'semi Gram.Entry.t)
            and _ = (sem_patt_for_list : 'sem_patt_for_list Gram.Entry.t)
            and _ = (sem_patt : 'sem_patt Gram.Entry.t)
            and _ = (sem_expr_for_list : 'sem_expr_for_list Gram.Entry.t)
            and _ = (sem_expr : 'sem_expr Gram.Entry.t)
            and _ = (row_field : 'row_field Gram.Entry.t)
            and _ = (poly_type : 'poly_type Gram.Entry.t)
            and _ = (phrase : 'phrase Gram.Entry.t)
            and _ = (patt_tcon : 'patt_tcon Gram.Entry.t)
            and _ = (patt_quot : 'patt_quot Gram.Entry.t)
            and _ = (patt_eoi : 'patt_eoi Gram.Entry.t)
            and _ = (patt_as_patt_opt : 'patt_as_patt_opt Gram.Entry.t)
            and _ = (patt : 'patt Gram.Entry.t)
            and _ = (opt_when_expr : 'opt_when_expr Gram.Entry.t)
            and _ = (opt_virtual : 'opt_virtual Gram.Entry.t)
            and _ = (opt_rec : 'opt_rec Gram.Entry.t)
            and _ = (opt_private : 'opt_private Gram.Entry.t)
            and _ = (opt_polyt : 'opt_polyt Gram.Entry.t)
            and _ = (opt_mutable : 'opt_mutable Gram.Entry.t)
            and _ = (opt_meth_list : 'opt_meth_list Gram.Entry.t)
            and _ = (opt_expr : 'opt_expr Gram.Entry.t)
            and _ = (opt_eq_ctyp : 'opt_eq_ctyp Gram.Entry.t)
            and _ = (opt_dot_dot : 'opt_dot_dot Gram.Entry.t)
            and _ = (opt_comma_ctyp : 'opt_comma_ctyp Gram.Entry.t)
            and _ = (opt_class_self_type : 'opt_class_self_type Gram.Entry.t)
            and _ = (opt_class_self_patt : 'opt_class_self_patt Gram.Entry.t)
            and _ = (opt_as_lident : 'opt_as_lident Gram.Entry.t)
            and _ = (name_tags : 'name_tags Gram.Entry.t)
            and _ = (more_ctyp : 'more_ctyp Gram.Entry.t)
            and _ = (module_type_quot : 'module_type_quot Gram.Entry.t)
            and _ = (module_type : 'module_type Gram.Entry.t)
            and _ =
              (module_rec_declaration : 'module_rec_declaration Gram.Entry.t)
            and _ =
              (module_longident_with_app :
                'module_longident_with_app Gram.Entry.t)
            and _ = (module_longident : 'module_longident Gram.Entry.t)
            and _ = (module_expr_quot : 'module_expr_quot Gram.Entry.t)
            and _ = (module_expr : 'module_expr Gram.Entry.t)
            and _ = (module_declaration : 'module_declaration Gram.Entry.t)
            and _ = (module_binding_quot : 'module_binding_quot Gram.Entry.t)
            and _ = (module_binding0 : 'module_binding0 Gram.Entry.t)
            and _ = (module_binding : 'module_binding Gram.Entry.t)
            and _ = (meth_decl : 'meth_decl Gram.Entry.t)
            and _ = (meth_list : 'meth_list Gram.Entry.t)
            and _ = (let_binding : 'let_binding Gram.Entry.t)
            and _ = (labeled_ipatt : 'labeled_ipatt Gram.Entry.t)
            and _ = (label_patt_list : 'label_patt_list Gram.Entry.t)
            and _ = (label_patt : 'label_patt Gram.Entry.t)
            and _ = (label_longident : 'label_longident Gram.Entry.t)
            and _ = (label_ipatt_list : 'label_ipatt_list Gram.Entry.t)
            and _ = (label_ipatt : 'label_ipatt Gram.Entry.t)
            and _ = (label_expr_list : 'label_expr_list Gram.Entry.t)
            and _ = (label_expr : 'label_expr Gram.Entry.t)
            and _ =
              (label_declaration_list : 'label_declaration_list Gram.Entry.t)
            and _ = (label_declaration : 'label_declaration Gram.Entry.t)
            and _ = (label : 'label Gram.Entry.t)
            and _ = (ipatt_tcon : 'ipatt_tcon Gram.Entry.t)
            and _ = (ipatt : 'ipatt Gram.Entry.t)
            and _ = (interf : 'interf Gram.Entry.t)
            and _ = (implem : 'implem Gram.Entry.t)
            and _ = (ident_quot : 'ident_quot Gram.Entry.t)
            and _ = (ident : 'ident Gram.Entry.t)
            and _ = (fun_def : 'fun_def Gram.Entry.t)
            and _ = (fun_binding : 'fun_binding Gram.Entry.t)
            and _ = (field_expr_list : 'field_expr_list Gram.Entry.t)
            and _ = (field_expr : 'field_expr Gram.Entry.t)
            and _ = (expr_quot : 'expr_quot Gram.Entry.t)
            and _ = (expr_eoi : 'expr_eoi Gram.Entry.t)
            and _ = (expr : 'expr Gram.Entry.t)
            and _ = (eq_expr : 'eq_expr Gram.Entry.t)
            and _ = (dummy : 'dummy Gram.Entry.t)
            and _ = (direction_flag : 'direction_flag Gram.Entry.t)
            and _ = (cvalue_binding : 'cvalue_binding Gram.Entry.t)
            and _ = (ctyp_quot : 'ctyp_quot Gram.Entry.t)
            and _ = (ctyp : 'ctyp Gram.Entry.t)
            and _ =
              (constructor_declarations :
                'constructor_declarations Gram.Entry.t)
            and _ =
              (constructor_declaration :
                'constructor_declaration Gram.Entry.t)
            and _ =
              (constructor_arg_list : 'constructor_arg_list Gram.Entry.t)
            and _ = (constrain : 'constrain Gram.Entry.t)
            and _ =
              (comma_type_parameter : 'comma_type_parameter Gram.Entry.t)
            and _ = (comma_patt : 'comma_patt Gram.Entry.t)
            and _ = (comma_ipatt : 'comma_ipatt Gram.Entry.t)
            and _ = (comma_expr : 'comma_expr Gram.Entry.t)
            and _ = (comma_ctyp : 'comma_ctyp Gram.Entry.t)
            and _ = (class_type_quot : 'class_type_quot Gram.Entry.t)
            and _ = (class_type_plus : 'class_type_plus Gram.Entry.t)
            and _ =
              (class_type_longident_and_param :
                'class_type_longident_and_param Gram.Entry.t)
            and _ =
              (class_type_longident : 'class_type_longident Gram.Entry.t)
            and _ =
              (class_type_declaration : 'class_type_declaration Gram.Entry.t)
            and _ = (class_type : 'class_type Gram.Entry.t)
            and _ = (class_structure : 'class_structure Gram.Entry.t)
            and _ = (class_str_item_quot : 'class_str_item_quot Gram.Entry.t)
            and _ = (class_str_item : 'class_str_item Gram.Entry.t)
            and _ = (class_signature : 'class_signature Gram.Entry.t)
            and _ = (class_sig_item_quot : 'class_sig_item_quot Gram.Entry.t)
            and _ = (class_sig_item : 'class_sig_item Gram.Entry.t)
            and _ =
              (class_name_and_param : 'class_name_and_param Gram.Entry.t)
            and _ =
              (class_longident_and_param :
                'class_longident_and_param Gram.Entry.t)
            and _ = (class_longident : 'class_longident Gram.Entry.t)
            and _ =
              (class_info_for_class_type :
                'class_info_for_class_type Gram.Entry.t)
            and _ =
              (class_info_for_class_expr :
                'class_info_for_class_expr Gram.Entry.t)
            and _ = (class_fun_def : 'class_fun_def Gram.Entry.t)
            and _ = (class_fun_binding : 'class_fun_binding Gram.Entry.t)
            and _ = (class_expr_quot : 'class_expr_quot Gram.Entry.t)
            and _ = (class_expr : 'class_expr Gram.Entry.t)
            and _ = (class_description : 'class_description Gram.Entry.t)
            and _ = (class_declaration : 'class_declaration Gram.Entry.t)
            and _ = (binding_quot : 'binding_quot Gram.Entry.t)
            and _ = (binding : 'binding Gram.Entry.t)
            and _ = (match_case_quot : 'match_case_quot Gram.Entry.t)
            and _ = (match_case0 : 'match_case0 Gram.Entry.t)
            and _ = (match_case : 'match_case Gram.Entry.t)
            and _ = (and_ctyp : 'and_ctyp Gram.Entry.t)
            and _ = (amp_ctyp : 'amp_ctyp Gram.Entry.t)
            and _ = (a_ident : 'a_ident Gram.Entry.t)
            and _ = (a_UIDENT : 'a_UIDENT Gram.Entry.t)
            and _ = (a_STRING : 'a_STRING Gram.Entry.t)
            and _ = (a_OPTLABEL : 'a_OPTLABEL Gram.Entry.t)
            and _ = (a_NATIVEINT : 'a_NATIVEINT Gram.Entry.t)
            and _ = (rec_binding_quot : 'rec_binding_quot Gram.Entry.t)
            and _ = (a_LIDENT : 'a_LIDENT Gram.Entry.t)
            and _ = (a_LABEL : 'a_LABEL Gram.Entry.t)
            and _ = (a_INT64 : 'a_INT64 Gram.Entry.t)
            and _ = (a_INT32 : 'a_INT32 Gram.Entry.t)
            and _ = (a_INT : 'a_INT Gram.Entry.t)
            and _ = (a_FLOAT : 'a_FLOAT Gram.Entry.t) in
            let grammar_entry_create = Gram.Entry.mk in
            let (* <:str_item< open $i$ >> *)
              (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
              (* Same remark for ?a:b *) infixop5 : 'infixop5 Gram.Entry.t =
              grammar_entry_create "infixop5"
            and (* | i = opt_label; "("; p = patt_tcon; ")" -> *)
              (* <:patt< ? $i$ : ($p$) >> *)
              (* | i = opt_label; "("; p = ipatt_tcon; ")" ->
            <:patt< ? $i$ : ($p$) >>
        | i = opt_label; "("; p = ipatt_tcon; "="; e = expr; ")" ->
            <:patt< ? $i$ : ($p$ = $e$) >>                             *)
              string_list : 'string_list Gram.Entry.t =
              grammar_entry_create "string_list"
            and opt_override : 'opt_override Gram.Entry.t =
              grammar_entry_create "opt_override"
            and unquoted_typevars : 'unquoted_typevars Gram.Entry.t =
              grammar_entry_create "unquoted_typevars"
            and value_val_opt_override :
              'value_val_opt_override Gram.Entry.t =
              grammar_entry_create "value_val_opt_override"
            and method_opt_override : 'method_opt_override Gram.Entry.t =
              grammar_entry_create "method_opt_override"
            and module_longident_dot_lparen :
              'module_longident_dot_lparen Gram.Entry.t =
              grammar_entry_create "module_longident_dot_lparen"
            and optional_type_parameter :
              'optional_type_parameter Gram.Entry.t =
              grammar_entry_create "optional_type_parameter"
            and fun_def_cont_no_when : 'fun_def_cont_no_when Gram.Entry.t =
              grammar_entry_create "fun_def_cont_no_when"
            and fun_def_cont : 'fun_def_cont Gram.Entry.t =
              grammar_entry_create "fun_def_cont"
            and sequence' : 'sequence' Gram.Entry.t =
              grammar_entry_create "sequence'"
            and infixop6 : 'infixop6 Gram.Entry.t =
              grammar_entry_create "infixop6"
            in
              (Gram.extend (module_expr : 'module_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "top"), None,
                         [ ([ Gram.Skeyword "struct";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (str_items : 'str_items Gram.Entry.t));
                              Gram.Skeyword "end" ],
                            (Gram.Action.mk
                               (fun _ (st : 'str_items) _ (_loc : Gram.Loc.t)
                                  -> (Ast.MeStr (_loc, st) : 'module_expr))));
                           ([ Gram.Skeyword "functor"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword ")"; Gram.Skeyword "->"; Gram.
                              Sself ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ _
                                  (t : 'module_type) _ (i : 'a_UIDENT) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MeFun (_loc, i, t, me) : 'module_expr)))) ]);
                        ((Some "apply"), None,
                         [ ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (me2 : 'module_expr) (me1 : 'module_expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MeApp (_loc, me1, me2) : 'module_expr)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val : 'value_val Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (package_type :
                                     'package_type Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p : 'package_type) _ (e : 'expr) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MePkg (_loc,
                                     (Ast.ExTyc (_loc, e,
                                        (Ast.TyPkg (_loc, p))))) :
                                    'module_expr))));
                           ([ Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val : 'value_val Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (e : 'expr) _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.MePkg (_loc, e) : 'module_expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (me : 'module_expr) _
                                  (_loc : Gram.Loc.t) -> (me : 'module_expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (mt : 'module_type) _
                                  (me : 'module_expr) _ (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.MeTyc (_loc, me, mt) : 'module_expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'module_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MeId (_loc, i) : 'module_expr))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.module_expr_tag :
                                        'module_expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "mexp" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"mexp\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "mexp" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.MeAnt (_loc,
                                         (mk_anti ~c: "module_expr" n s)) :
                                        'module_expr)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (str_item : 'str_item Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "top"), None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.StExp (_loc, e) : 'str_item))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.str_item_tag :
                                        'str_item)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "stri" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"stri\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "stri" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.StAnt (_loc,
                                         (mk_anti ~c: "str_item" n s)) :
                                        'str_item)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "class"; Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_declaration :
                                     'class_type_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ctd : 'class_type_declaration) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StClt (_loc, ctd) : 'str_item))));
                           ([ Gram.Skeyword "class";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_declaration :
                                     'class_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (cd : 'class_declaration) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StCls (_loc, cd) : 'str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_let : 'value_let Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_rec : 'opt_rec Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (binding : 'binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (bi : 'binding) (r : 'opt_rec) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StVal (_loc, r, bi) : 'str_item))));
                           ([ Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_declaration :
                                     'type_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (td : 'type_declaration) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StTyp (_loc, td) : 'str_item))));
                           ([ Gram.Skeyword "open";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'module_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StOpn (_loc, Ast.OvNil, i) :
                                    'str_item))));
                           ([ Gram.Skeyword "open"; Gram.Skeyword "!";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'module_longident) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StOpn (_loc, Ast.OvOverride, i) :
                                    'str_item))));
                           ([ Gram.Skeyword "module"; Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _ (i : 'a_ident) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StMty (_loc, i, mt) : 'str_item))));
                           ([ Gram.Skeyword "module"; Gram.Skeyword "rec";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_binding :
                                     'module_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mb : 'module_binding) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StRecMod (_loc, mb) : 'str_item))));
                           ([ Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_binding0 :
                                     'module_binding0 Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mb : 'module_binding0) (i : 'a_UIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StMod (_loc, i, mb) : 'str_item))));
                           ([ Gram.Skeyword "include";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ (_loc : Gram.Loc.t)
                                  -> (Ast.StInc (_loc, me) : 'str_item))));
                           ([ Gram.Skeyword "external";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (string_list : 'string_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (sl : 'string_list) _ (t : 'ctyp) _
                                  (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.StExt (_loc, i, t, sl) : 'str_item))));
                           ([ Gram.Skeyword "exception";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_declaration :
                                     'constructor_declaration Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_longident :
                                     'type_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'type_longident) _
                                  (t : 'constructor_declaration) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StExc (_loc, t, (Ast.OSome i)) :
                                    'str_item))));
                           ([ Gram.Skeyword "exception";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_declaration :
                                     'constructor_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'constructor_declaration) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StExc (_loc, t, Ast.ONone) :
                                    'str_item)))) ]) ]))
                    ());
               Gram.extend (module_binding0 : 'module_binding0 Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ (_loc : Gram.Loc.t)
                                  -> (me : 'module_binding0))));
                           ([ Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ (mt : 'module_type)
                                  _ (_loc : Gram.Loc.t) ->
                                  (Ast.MeTyc (_loc, me, mt) :
                                    'module_binding0))));
                           ([ Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword ")"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (mb : 'module_binding0) _
                                  (mt : 'module_type) _ (m : 'a_UIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MeFun (_loc, m, mt, mb) :
                                    'module_binding0)))) ]) ]))
                    ());
               Gram.extend (module_binding : 'module_binding Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ (mt : 'module_type)
                                  _ (m : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.MbColEq (_loc, m, mt, me) :
                                    'module_binding))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.module_binding_tag :
                                        'module_binding)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"\", _)"));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ (mt : 'module_type)
                                  _ (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" as n)), m) ->
                                      (Ast.MbColEq (_loc, (mk_anti n m), mt,
                                         me) :
                                        'module_binding)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" as n)), s) ->
                                      (Ast.MbAnt (_loc,
                                         (mk_anti ~c: "module_binding" n s)) :
                                        'module_binding)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("module_binding" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"module_binding\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("module_binding" | "anti" | "list"
                                         as n)),
                                      s) ->
                                      (Ast.MbAnt (_loc,
                                         (mk_anti ~c: "module_binding" n s)) :
                                        'module_binding)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (b2 : 'module_binding) _
                                  (b1 : 'module_binding) (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.MbAnd (_loc, b1, b2) :
                                    'module_binding)))) ]) ]))
                    ());
               Gram.extend (module_type : 'module_type Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "top"), None,
                         [ ([ Gram.Skeyword "functor"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":"; Gram.Sself;
                              Gram.Skeyword ")"; Gram.Skeyword "->"; Gram.
                              Sself ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _ _
                                  (t : 'module_type) _ (i : 'a_UIDENT) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MtFun (_loc, i, t, mt) : 'module_type)))) ]);
                        ((Some "with"), None,
                         [ ([ Gram.Sself; Gram.Skeyword "with";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (with_constr : 'with_constr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (wc : 'with_constr) _ (mt : 'module_type)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MtWit (_loc, mt, wc) : 'module_type)))) ]);
                        ((Some "apply"), None,
                         [ ([ Gram.Sself; Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj (dummy : 'dummy Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (mt2 : 'module_type)
                                  (mt1 : 'module_type) (_loc : Gram.Loc.t) ->
                                  (module_type_app mt1 mt2 : 'module_type)))) ]);
                        ((Some "."), None,
                         [ ([ Gram.Sself; Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (mt2 : 'module_type) _
                                  (mt1 : 'module_type) (_loc : Gram.Loc.t) ->
                                  (module_type_acc mt1 mt2 : 'module_type)))) ]);
                        ((Some "sig"), None,
                         [ ([ Gram.Skeyword "sig";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sig_items : 'sig_items Gram.Entry.t));
                              Gram.Skeyword "end" ],
                            (Gram.Action.mk
                               (fun _ (sg : 'sig_items) _ (_loc : Gram.Loc.t)
                                  -> (Ast.MtSig (_loc, sg) : 'module_type)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "module"; Gram.Skeyword "type";
                              Gram.Skeyword "of";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MtOf (_loc, me) : 'module_type))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (mt : 'module_type) _
                                  (_loc : Gram.Loc.t) -> (mt : 'module_type))));
                           ([ Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.MtQuo (_loc, i) : 'module_type))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident_with_app :
                                     'module_longident_with_app Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'module_longident_with_app)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MtId (_loc, i) : 'module_type))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.module_type_tag :
                                        'module_type)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "mtyp" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"mtyp\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "mtyp" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.MtAnt (_loc,
                                         (mk_anti ~c: "module_type" n s)) :
                                        'module_type)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (sig_item : 'sig_item Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "top"), None,
                         [ ([ Gram.Skeyword "class"; Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_declaration :
                                     'class_type_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ctd : 'class_type_declaration) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgClt (_loc, ctd) : 'sig_item))));
                           ([ Gram.Skeyword "class";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_description :
                                     'class_description Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (cd : 'class_description) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgCls (_loc, cd) : 'sig_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val : 'value_val Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (i : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgVal (_loc, i, t) : 'sig_item))));
                           ([ Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_declaration :
                                     'type_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'type_declaration) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgTyp (_loc, t) : 'sig_item))));
                           ([ Gram.Skeyword "open";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'module_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgOpn (_loc, i) : 'sig_item))));
                           ([ Gram.Skeyword "module"; Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.SgMty (_loc, i, (Ast.MtNil _loc)) :
                                    'sig_item))));
                           ([ Gram.Skeyword "module"; Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _ (i : 'a_ident) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgMty (_loc, i, mt) : 'sig_item))));
                           ([ Gram.Skeyword "module"; Gram.Skeyword "rec";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_rec_declaration :
                                     'module_rec_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mb : 'module_rec_declaration) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgRecMod (_loc, mb) : 'sig_item))));
                           ([ Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_declaration :
                                     'module_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_declaration)
                                  (i : 'a_UIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.SgMod (_loc, i, mt) : 'sig_item))));
                           ([ Gram.Skeyword "include";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _ (_loc : Gram.Loc.t)
                                  -> (Ast.SgInc (_loc, mt) : 'sig_item))));
                           ([ Gram.Skeyword "external";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (string_list : 'string_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (sl : 'string_list) _ (t : 'ctyp) _
                                  (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.SgExt (_loc, i, t, sl) : 'sig_item))));
                           ([ Gram.Skeyword "exception";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_declaration :
                                     'constructor_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'constructor_declaration) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgExc (_loc, t) : 'sig_item))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.sig_item_tag :
                                        'sig_item)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "sigi" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"sigi\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "sigi" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.SgAnt (_loc,
                                         (mk_anti ~c: "sig_item" n s)) :
                                        'sig_item)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (module_declaration : 'module_declaration Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword ")"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (mt : 'module_declaration) _
                                  (t : 'module_type) _ (i : 'a_UIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MtFun (_loc, i, t, mt) :
                                    'module_declaration))));
                           ([ Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _ (_loc : Gram.Loc.t)
                                  -> (mt : 'module_declaration)))) ]) ]))
                    ());
               Gram.extend
                 (module_rec_declaration :
                   'module_rec_declaration Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _ (m : 'a_UIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MbCol (_loc, m, mt) :
                                    'module_rec_declaration))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.module_binding_tag :
                                        'module_rec_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "module_binding" | "anti" |
                                           "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"module_binding\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "module_binding" | "anti" |
                                           "list"
                                         as n)),
                                      s) ->
                                      (Ast.MbAnt (_loc,
                                         (mk_anti ~c: "module_binding" n s)) :
                                        'module_rec_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (m2 : 'module_rec_declaration) _
                                  (m1 : 'module_rec_declaration)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MbAnd (_loc, m1, m2) :
                                    'module_rec_declaration)))) ]) ]))
                    ());
               Gram.extend (with_constr : 'with_constr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t));
                              Gram.Skeyword ":=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident_with_app :
                                     'module_longident_with_app Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i2 : 'module_longident_with_app) _
                                  (i1 : 'module_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.WcMoS (_loc, i1, i2) : 'with_constr))));
                           ([ Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_longident_and_parameters :
                                     'type_longident_and_parameters Gram.
                                       Entry.t));
                              Gram.Skeyword ":=";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _
                                  (t1 : 'type_longident_and_parameters) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.WcTyS (_loc, t1, t2) : 'with_constr))));
                           ([ Gram.Skeyword "type";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\"), _)"));
                              Gram.Skeyword ":=";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (__camlp4_0 : Gram.Token.t)
                                  _ (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" | "anti" as n)),
                                      s) ->
                                      (Ast.WcTyS (_loc,
                                         (Ast.TyAnt (_loc,
                                            (mk_anti ~c: "ctyp" n s))),
                                         t) :
                                        'with_constr)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident_with_app :
                                     'module_longident_with_app Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i2 : 'module_longident_with_app) _
                                  (i1 : 'module_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.WcMod (_loc, i1, i2) : 'with_constr))));
                           ([ Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_longident_and_parameters :
                                     'type_longident_and_parameters Gram.
                                       Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _
                                  (t1 : 'type_longident_and_parameters) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.WcTyp (_loc, t1, t2) : 'with_constr))));
                           ([ Gram.Skeyword "type";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\"), _)"));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (__camlp4_0 : Gram.Token.t)
                                  _ (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" | "anti" as n)),
                                      s) ->
                                      (Ast.WcTyp (_loc,
                                         (Ast.TyAnt (_loc,
                                            (mk_anti ~c: "ctyp" n s))),
                                         t) :
                                        'with_constr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.with_constr_tag :
                                        'with_constr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "with_constr" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"with_constr\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "with_constr" | "anti" | "list"
                                         as n)),
                                      s) ->
                                      (Ast.WcAnt (_loc,
                                         (mk_anti ~c: "with_constr" n s)) :
                                        'with_constr)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (wc2 : 'with_constr) _
                                  (wc1 : 'with_constr) (_loc : Gram.Loc.t) ->
                                  (Ast.WcAnd (_loc, wc1, wc2) : 'with_constr)))) ]) ]))
                    ());
               Gram.extend (expr : 'expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "top"), (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Skeyword "object";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_class_self_patt :
                                     'opt_class_self_patt Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_structure :
                                     'class_structure Gram.Entry.t));
                              Gram.Skeyword "end" ],
                            (Gram.Action.mk
                               (fun _ (cst : 'class_structure)
                                  (csp : 'opt_class_self_patt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExObj (_loc, csp, cst) : 'expr))));
                           ([ Gram.Skeyword "while";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Skeyword "do";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (do_sequence : 'do_sequence Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (seq : 'do_sequence) _ (e : 'sequence) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExWhi (_loc, (mksequence' _loc e),
                                     seq) :
                                    'expr))));
                           ([ Gram.Skeyword "for";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (direction_flag :
                                     'direction_flag Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Skeyword "do";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (do_sequence : 'do_sequence Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (seq : 'do_sequence) _ (e2 : 'sequence)
                                  (df : 'direction_flag) (e1 : 'sequence) _
                                  (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExFor (_loc, i, (mksequence' _loc e1),
                                     (mksequence' _loc e2), df, seq) :
                                    'expr))));
                           ([ Gram.Skeyword "do";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (do_sequence : 'do_sequence Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (seq : 'do_sequence) _
                                  (_loc : Gram.Loc.t) ->
                                  (mksequence _loc seq : 'expr))));
                           ([ Gram.Skeyword "if"; Gram.Sself;
                              Gram.Skeyword "then"; Gram.Sself;
                              Gram.Skeyword "else"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e3 : 'expr) _ (e2 : 'expr) _
                                  (e1 : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExIfe (_loc, e1, e2, e3) : 'expr))));
                           ([ Gram.Skeyword "try";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Skeyword "with";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (match_case : 'match_case Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (a : 'match_case) _ (e : 'sequence) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExTry (_loc, (mksequence' _loc e), a) :
                                    'expr))));
                           ([ Gram.Skeyword "match";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Skeyword "with";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (match_case : 'match_case Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (a : 'match_case) _ (e : 'sequence) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExMat (_loc, (mksequence' _loc e), a) :
                                    'expr))));
                           ([ Gram.Skeyword "fun";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_def : 'fun_def Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'fun_def) _ (_loc : Gram.Loc.t) ->
                                  (e : 'expr))));
                           ([ Gram.Skeyword "fun"; Gram.Skeyword "[";
                              Gram.Slist0sep
                                ((Gram.Snterm
                                    (Gram.Entry.obj
                                       (match_case0 :
                                         'match_case0 Gram.Entry.t))),
                                (Gram.Skeyword "|"));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (a : 'match_case0 list) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExFun (_loc, (Ast.mcOr_of_list a)) :
                                    'expr))));
                           ([ Gram.Skeyword "let"; Gram.Skeyword "open";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t));
                              Gram.Skeyword "in"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (i : 'module_longident) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExOpI (_loc, i, Ast.OvNil, e) : 'expr))));
                           ([ Gram.Skeyword "let"; Gram.Skeyword "open";
                              Gram.Skeyword "!";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t));
                              Gram.Skeyword "in"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (i : 'module_longident) _ _
                                  _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExOpI (_loc, i, Ast.OvOverride, e) :
                                    'expr))));
                           ([ Gram.Skeyword "let"; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_binding0 :
                                     'module_binding0 Gram.Entry.t));
                              Gram.Skeyword "in"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (mb : 'module_binding0)
                                  (m : 'a_UIDENT) _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExLmd (_loc, m, mb, e) : 'expr))));
                           ([ Gram.Skeyword "let";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_rec : 'opt_rec Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (binding : 'binding Gram.Entry.t));
                              Gram.Skeyword "in"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (x : 'expr) _ (bi : 'binding)
                                  (r : 'opt_rec) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExLet (_loc, r, bi, x) : 'expr)))) ]);
                        ((Some "where"), None,
                         [ ([ Gram.Sself; Gram.Skeyword "where";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_rec : 'opt_rec Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (let_binding : 'let_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (lb : 'let_binding) (rf : 'opt_rec) _
                                  (e : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExLet (_loc, rf, lb, e) : 'expr)))) ]);
                        ((Some ":="), (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Sself; Gram.Skeyword ":="; Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj (dummy : 'dummy Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (match bigarray_set _loc e1 e2 with
                                   | Some e -> e
                                   | None -> Ast.ExAss (_loc, e1, e2) :
                                    'expr)))) ]);
                        ((Some "||"), (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (infixop6 : 'infixop6 Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (op : 'infixop6)
                                  (e1 : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc, op, e1)), e2) :
                                    'expr)))) ]);
                        ((Some "&&"), (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (infixop5 : 'infixop5 Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (op : 'infixop5)
                                  (e1 : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc, op, e1)), e2) :
                                    'expr)))) ]);
                        ((Some "<"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (infixop0 : 'infixop0 Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (op : 'infixop0)
                                  (e1 : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc, op, e1)), e2) :
                                    'expr)))) ]);
                        ((Some "^"), (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (infixop1 : 'infixop1 Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (op : 'infixop1)
                                  (e1 : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc, op, e1)), e2) :
                                    'expr)))) ]);
                        ((Some "+"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (infixop2 : 'infixop2 Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (op : 'infixop2)
                                  (e1 : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc, op, e1)), e2) :
                                    'expr)))) ]);
                        ((Some "*"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (infixop3 : 'infixop3 Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (op : 'infixop3)
                                  (e1 : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc, op, e1)), e2) :
                                    'expr))));
                           ([ Gram.Sself; Gram.Skeyword "mod"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdLid (_loc, "mod")))),
                                        e1)),
                                     e2) :
                                    'expr))));
                           ([ Gram.Sself; Gram.Skeyword "lxor"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdLid (_loc, "lxor")))),
                                        e1)),
                                     e2) :
                                    'expr))));
                           ([ Gram.Sself; Gram.Skeyword "lor"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdLid (_loc, "lor")))),
                                        e1)),
                                     e2) :
                                    'expr))));
                           ([ Gram.Sself; Gram.Skeyword "land"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdLid (_loc, "land")))),
                                        e1)),
                                     e2) :
                                    'expr)))) ]);
                        ((Some "**"), (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Sself;
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (infixop4 : 'infixop4 Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (op : 'infixop4)
                                  (e1 : 'expr) (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc, op, e1)), e2) :
                                    'expr))));
                           ([ Gram.Sself; Gram.Skeyword "lsr"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdLid (_loc, "lsr")))),
                                        e1)),
                                     e2) :
                                    'expr))));
                           ([ Gram.Sself; Gram.Skeyword "lsl"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdLid (_loc, "lsl")))),
                                        e1)),
                                     e2) :
                                    'expr))));
                           ([ Gram.Sself; Gram.Skeyword "asr"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdLid (_loc, "asr")))),
                                        e1)),
                                     e2) :
                                    'expr)))) ]);
                        ((Some "unary minus"),
                         (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Skeyword "-."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (mkumin _loc "-." e : 'expr))));
                           ([ Gram.Skeyword "-"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (mkumin _loc "-" e : 'expr)))) ]);
                        ((Some "apply"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Skeyword "lazy"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExLaz (_loc, e) : 'expr))));
                           ([ Gram.Skeyword "new";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_longident :
                                     'class_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'class_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExNew (_loc, i) : 'expr))));
                           ([ Gram.Skeyword "assert"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (mkassert _loc e : 'expr))));
                           ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc, e1, e2) : 'expr)))) ]);
                        ((Some "label"), (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Skeyword "?";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExOlb (_loc, i, (Ast.ExNil _loc)) :
                                    'expr))));
                           ([ Gram.Skeyword "?";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (i : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExOlb (_loc, i, e) : 'expr))));
                           ([ Gram.Stoken
                                (((function | OPTLABEL _ -> true | _ -> false),
                                  "OPTLABEL _"));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | OPTLABEL i ->
                                      (Ast.ExOlb (_loc, i, e) : 'expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function | LABEL _ -> true | _ -> false),
                                  "LABEL _"));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LABEL i ->
                                      (Ast.ExLab (_loc, i, e) : 'expr)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExLab (_loc, i, (Ast.ExNil _loc)) :
                                    'expr))));
                           ([ Gram.Skeyword "~";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (i : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExLab (_loc, i, e) : 'expr)))) ]);
                        ((Some "."), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself; Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (lab : 'label) _ (e : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExSnd (_loc, e, lab) : 'expr))));
                           ([ Gram.Sself; Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExAcc (_loc, e1, e2) : 'expr))));
                           ([ Gram.Sself; Gram.Skeyword ".";
                              Gram.Skeyword "{";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_expr : 'comma_expr Gram.Entry.t));
                              Gram.Skeyword "}" ],
                            (Gram.Action.mk
                               (fun _ (e2 : 'comma_expr) _ _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (bigarray_get _loc e1 e2 : 'expr))));
                           ([ Gram.Sself; Gram.Skeyword ".";
                              Gram.Skeyword "["; Gram.Sself;
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (e2 : 'expr) _ _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExSte (_loc, e1, e2) : 'expr))));
                           ([ Gram.Sself; Gram.Skeyword ".";
                              Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (e2 : 'expr) _ _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExAre (_loc, e1, e2) : 'expr)))) ]);
                        ((Some "~-"), (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (prefixop : 'prefixop Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (f : 'prefixop)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc, f, e) : 'expr))));
                           ([ Gram.Skeyword "!"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExAcc (_loc, e,
                                     (Ast.ExId (_loc,
                                        (Ast.IdLid (_loc, "val"))))) :
                                    'expr)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "("; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (package_type :
                                     'package_type Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (pt : 'package_type) _
                                  (me : 'module_expr) _ _ (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.ExPkg (_loc,
                                     (Ast.MeTyc (_loc, me, pt))) :
                                    'expr))));
                           ([ Gram.Skeyword "("; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (me : 'module_expr) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExPkg (_loc, me) : 'expr))));
                           ([ Gram.Skeyword "begin"; Gram.Skeyword "end" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) :
                                    'expr))));
                           ([ Gram.Skeyword "begin";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Skeyword "end" ],
                            (Gram.Action.mk
                               (fun _ (seq : 'sequence) _ (_loc : Gram.Loc.t)
                                  -> (mksequence _loc seq : 'expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (e : 'expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ":>";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t : 'ctyp) _ (e : 'expr) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExCoe (_loc, e, (Ast.TyNil _loc), t) :
                                    'expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ":>";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t2 : 'ctyp) _ (t : 'ctyp) _
                                  (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExCoe (_loc, e, t, t2) : 'expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ";"; Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ _ (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (mksequence _loc e : 'expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (seq : 'sequence) _ (e : 'expr) _
                                  (_loc : Gram.Loc.t) ->
                                  (mksequence _loc (Ast.ExSem (_loc, e, seq)) :
                                    'expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ",";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_expr : 'comma_expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (el : 'comma_expr) _ (e : 'expr) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExTup (_loc,
                                     (Ast.ExCom (_loc, e, el))) :
                                    'expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t : 'ctyp) _ (e : 'expr) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExTyc (_loc, e, t) : 'expr))));
                           ([ Gram.Skeyword "("; Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) :
                                    'expr))));
                           ([ Gram.Skeyword "{<";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (field_expr_list :
                                     'field_expr_list Gram.Entry.t));
                              Gram.Skeyword ">}" ],
                            (Gram.Action.mk
                               (fun _ (fel : 'field_expr_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExOvr (_loc, fel) : 'expr))));
                           ([ Gram.Skeyword "{<"; Gram.Skeyword ">}" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExOvr (_loc, (Ast.RbNil _loc)) :
                                    'expr))));
                           ([ Gram.Skeyword "{"; Gram.Skeyword "("; Gram.
                              Sself; Gram.Skeyword ")"; Gram.Skeyword "with";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (label_expr_list :
                                     'label_expr_list Gram.Entry.t));
                              Gram.Skeyword "}" ],
                            (Gram.Action.mk
                               (fun _ (el : 'label_expr_list) _ _ (e : 'expr)
                                  _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExRec (_loc, el, e) : 'expr))));
                           ([ Gram.Skeyword "{";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (label_expr_list :
                                     'label_expr_list Gram.Entry.t));
                              Gram.Skeyword "}" ],
                            (Gram.Action.mk
                               (fun _ (el : 'label_expr_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExRec (_loc, el, (Ast.ExNil _loc)) :
                                    'expr))));
                           ([ Gram.Skeyword "[|";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_expr : 'sem_expr Gram.Entry.t));
                              Gram.Skeyword "|]" ],
                            (Gram.Action.mk
                               (fun _ (el : 'sem_expr) _ (_loc : Gram.Loc.t)
                                  -> (Ast.ExArr (_loc, el) : 'expr))));
                           ([ Gram.Skeyword "[|"; Gram.Skeyword "|]" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExArr (_loc, (Ast.ExNil _loc)) :
                                    'expr))));
                           ([ Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_expr_for_list :
                                     'sem_expr_for_list Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (mk_list : 'sem_expr_for_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (mk_list
                                     (Ast.ExId (_loc,
                                        (Ast.IdUid (_loc, "[]")))) :
                                    'expr))));
                           ([ Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_expr_for_list :
                                     'sem_expr_for_list Gram.Entry.t));
                              Gram.Skeyword "::"; Gram.Sself;
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (last : 'expr) _
                                  (mk_list : 'sem_expr_for_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (mk_list last : 'expr))));
                           ([ Gram.Skeyword "["; Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))) :
                                    'expr))));
                           ([ Gram.Skeyword "`";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExVrn (_loc, s) : 'expr))));
                           ([ Gram.Stry
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (val_longident :
                                        'val_longident Gram.Entry.t))) ],
                            (Gram.Action.mk
                               (fun (i : 'val_longident) (_loc : Gram.Loc.t)
                                  -> (Ast.ExId (_loc, i) : 'expr))));
                           ([ Gram.Stry
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (module_longident_dot_lparen :
                                        'module_longident_dot_lparen Gram.
                                          Entry.t)));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (e : 'sequence)
                                  (i : 'module_longident_dot_lparen)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExOpI (_loc, i, Ast.OvNil, e) : 'expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_CHAR : 'a_CHAR Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_CHAR) (_loc : Gram.Loc.t) ->
                                  (Ast.ExChr (_loc, s) : 'expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_STRING : 'a_STRING Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_STRING) (_loc : Gram.Loc.t) ->
                                  (Ast.ExStr (_loc, s) : 'expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_FLOAT : 'a_FLOAT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_FLOAT) (_loc : Gram.Loc.t) ->
                                  (Ast.ExFlo (_loc, s) : 'expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_NATIVEINT : 'a_NATIVEINT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_NATIVEINT) (_loc : Gram.Loc.t) ->
                                  (Ast.ExNativeInt (_loc, s) : 'expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_INT64 : 'a_INT64 Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT64) (_loc : Gram.Loc.t) ->
                                  (Ast.ExInt64 (_loc, s) : 'expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_INT32 : 'a_INT32 Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT32) (_loc : Gram.Loc.t) ->
                                  (Ast.ExInt32 (_loc, s) : 'expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (a_INT : 'a_INT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT) (_loc : Gram.Loc.t) ->
                                  (Ast.ExInt (_loc, s) : 'expr))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("seq", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"seq\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("seq" as n)), s) ->
                                      (Ast.ExSeq (_loc,
                                         (Ast.ExAnt (_loc,
                                            (mk_anti ~c: "expr" n s)))) :
                                        'expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("tup", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"tup\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("tup" as n)), s) ->
                                      (Ast.ExTup (_loc,
                                         (Ast.ExAnt (_loc,
                                            (mk_anti ~c: "expr" n s)))) :
                                        'expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("`bool", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"`bool\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("`bool" as n)), s) ->
                                      (Ast.ExId (_loc,
                                         (Ast.IdAnt (_loc, (mk_anti n s)))) :
                                        'expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("exp" | "" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"exp\" | \"\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("exp" | "" | "anti" as n)),
                                      s) ->
                                      (Ast.ExAnt (_loc,
                                         (mk_anti ~c: "expr" n s)) :
                                        'expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.expr_tag :
                                        'expr)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (do_sequence : 'do_sequence Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "done" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) :
                                    'do_sequence))));
                           ([ Gram.Stry
                                (Gram.srules do_sequence
                                   [ ([ Gram.Snterm
                                          (Gram.Entry.obj
                                             (sequence :
                                               'sequence Gram.Entry.t));
                                        Gram.Skeyword "done" ],
                                      (Gram.Action.mk
                                         (fun _ (seq : 'sequence)
                                            (_loc : Gram.Loc.t) ->
                                            (seq : 'e__3)))) ]) ],
                            (Gram.Action.mk
                               (fun (seq : 'e__3) (_loc : Gram.Loc.t) ->
                                  (seq : 'do_sequence))));
                           ([ Gram.Stry
                                (Gram.srules do_sequence
                                   [ ([ Gram.Skeyword "{"; Gram.Skeyword "}" ],
                                      (Gram.Action.mk
                                         (fun _ _ (_loc : Gram.Loc.t) ->
                                            (() : 'e__2)))) ]) ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) :
                                    'do_sequence))));
                           ([ Gram.Stry
                                (Gram.srules do_sequence
                                   [ ([ Gram.Skeyword "{";
                                        Gram.Snterm
                                          (Gram.Entry.obj
                                             (sequence :
                                               'sequence Gram.Entry.t));
                                        Gram.Skeyword "}" ],
                                      (Gram.Action.mk
                                         (fun _ (seq : 'sequence) _
                                            (_loc : Gram.Loc.t) ->
                                            (seq : 'e__1)))) ]) ],
                            (Gram.Action.mk
                               (fun (seq : 'e__1) (_loc : Gram.Loc.t) ->
                                  (seq : 'do_sequence)))) ]) ]))
                    ());
               Gram.extend (infixop5 : 'infixop5 Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.srules infixop5
                                [ ([ Gram.Skeyword "&&" ],
                                   (Gram.Action.mk
                                      (fun (x : Gram.Token.t)
                                         (_loc : Gram.Loc.t) ->
                                         (Gram.Token.extract_string x :
                                           'e__4))));
                                  ([ Gram.Skeyword "&" ],
                                   (Gram.Action.mk
                                      (fun (x : Gram.Token.t)
                                         (_loc : Gram.Loc.t) ->
                                         (Gram.Token.extract_string x :
                                           'e__4)))) ] ],
                            (Gram.Action.mk
                               (fun (x : 'e__4) (_loc : Gram.Loc.t) ->
                                  (Ast.ExId (_loc, (Ast.IdLid (_loc, x))) :
                                    'infixop5)))) ]) ]))
                    ());
               Gram.extend (infixop6 : 'infixop6 Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.srules infixop6
                                [ ([ Gram.Skeyword "||" ],
                                   (Gram.Action.mk
                                      (fun (x : Gram.Token.t)
                                         (_loc : Gram.Loc.t) ->
                                         (Gram.Token.extract_string x :
                                           'e__5))));
                                  ([ Gram.Skeyword "or" ],
                                   (Gram.Action.mk
                                      (fun (x : Gram.Token.t)
                                         (_loc : Gram.Loc.t) ->
                                         (Gram.Token.extract_string x :
                                           'e__5)))) ] ],
                            (Gram.Action.mk
                               (fun (x : 'e__5) (_loc : Gram.Loc.t) ->
                                  (Ast.ExId (_loc, (Ast.IdLid (_loc, x))) :
                                    'infixop6)))) ]) ]))
                    ());
               Gram.extend
                 (sem_expr_for_list : 'sem_expr_for_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                  (fun acc ->
                                     Ast.ExApp (_loc,
                                       (Ast.ExApp (_loc,
                                          (Ast.ExId (_loc,
                                             (Ast.IdUid (_loc, "::")))),
                                          e)),
                                       acc) :
                                    'sem_expr_for_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (e : 'expr) (_loc : Gram.Loc.t) ->
                                  (fun acc ->
                                     Ast.ExApp (_loc,
                                       (Ast.ExApp (_loc,
                                          (Ast.ExId (_loc,
                                             (Ast.IdUid (_loc, "::")))),
                                          e)),
                                       acc) :
                                    'sem_expr_for_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (el : 'sem_expr_for_list) _ (e : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (fun acc ->
                                     Ast.ExApp (_loc,
                                       (Ast.ExApp (_loc,
                                          (Ast.ExId (_loc,
                                             (Ast.IdUid (_loc, "::")))),
                                          e)),
                                       (el acc)) :
                                    'sem_expr_for_list)))) ]) ]))
                    ());
               Gram.extend (comma_expr : 'comma_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterml
                                ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                                "top") ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                  (e : 'comma_expr))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.ExAnt (_loc,
                                         (mk_anti ~c: "expr," n s)) :
                                        'comma_expr)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword ","; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e2 : 'comma_expr) _ (e1 : 'comma_expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExCom (_loc, e1, e2) : 'comma_expr)))) ]) ]))
                    ());
               Gram.extend (dummy : 'dummy Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) -> (() : 'dummy)))) ]) ]))
                    ());
               Gram.extend (sequence' : 'sequence' Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence : 'sequence Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (el : 'sequence) _ (_loc : Gram.Loc.t) ->
                                  (fun e -> Ast.ExSem (_loc, e, el) :
                                    'sequence'))));
                           ([ Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (fun e -> e : 'sequence'))));
                           ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (fun e -> e : 'sequence')))) ]) ]))
                    ());
               Gram.extend (sequence : 'sequence Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence' : 'sequence' Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (k : 'sequence') (e : 'expr)
                                  (_loc : Gram.Loc.t) -> (k e : 'sequence))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.ExAnt (_loc,
                                         (mk_anti ~c: "expr;" n s)) :
                                        'sequence)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "let"; Gram.Skeyword "open";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t));
                              Gram.Skeyword "in"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'sequence) _ (i : 'module_longident)
                                  _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExOpI (_loc, i, Ast.OvNil, e) :
                                    'sequence))));
                           ([ Gram.Skeyword "let"; Gram.Skeyword "open";
                              Gram.Skeyword "!";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_longident :
                                     'module_longident Gram.Entry.t));
                              Gram.Skeyword "in"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'sequence) _ (i : 'module_longident)
                                  _ _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExOpI (_loc, i, Ast.OvOverride, e) :
                                    'sequence))));
                           ([ Gram.Skeyword "let"; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_binding0 :
                                     'module_binding0 Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (el : 'sequence) _
                                  (mb : 'module_binding0) (m : 'a_UIDENT) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExLmd (_loc, m, mb,
                                     (mksequence _loc el)) :
                                    'sequence))));
                           ([ Gram.Skeyword "let"; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_binding0 :
                                     'module_binding0 Gram.Entry.t));
                              Gram.Skeyword "in";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence' : 'sequence' Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (k : 'sequence') (e : 'expr) _
                                  (mb : 'module_binding0) (m : 'a_UIDENT) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (k (Ast.ExLmd (_loc, m, mb, e)) :
                                    'sequence))));
                           ([ Gram.Skeyword "let";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_rec : 'opt_rec Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (binding : 'binding Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (el : 'sequence) _ (bi : 'binding)
                                  (rf : 'opt_rec) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExLet (_loc, rf, bi,
                                     (mksequence _loc el)) :
                                    'sequence))));
                           ([ Gram.Skeyword "let";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_rec : 'opt_rec Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (binding : 'binding Gram.Entry.t));
                              Gram.Skeyword "in";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sequence' : 'sequence' Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (k : 'sequence') (e : 'expr) _
                                  (bi : 'binding) (rf : 'opt_rec) _
                                  (_loc : Gram.Loc.t) ->
                                  (k (Ast.ExLet (_loc, rf, bi, e)) :
                                    'sequence)))) ]) ]))
                    ());
               Gram.extend (binding : 'binding Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (let_binding : 'let_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (b : 'let_binding) (_loc : Gram.Loc.t) ->
                                  (b : 'binding))));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (b2 : 'binding) _ (b1 : 'binding)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.BiAnd (_loc, b1, b2) : 'binding))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "anti" as n)), s) ->
                                      (Ast.BiAnt (_loc,
                                         (mk_anti ~c: "binding" n s)) :
                                        'binding)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"anti\"), _)"));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "anti" as n)), s) ->
                                      (Ast.BiEq (_loc,
                                         (Ast.PaAnt (_loc,
                                            (mk_anti ~c: "patt" n s))),
                                         e) :
                                        'binding)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("binding" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"binding\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("binding" | "list" as n)), s)
                                      ->
                                      (Ast.BiAnt (_loc,
                                         (mk_anti ~c: "binding" n s)) :
                                        'binding)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (let_binding : 'let_binding Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ipatt : 'ipatt Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_binding : 'fun_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'fun_binding) (p : 'ipatt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.BiEq (_loc, p, e) : 'let_binding)))) ]) ]))
                    ());
               Gram.extend (fun_binding : 'fun_binding Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (cvalue_binding :
                                     'cvalue_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (bi : 'cvalue_binding)
                                  (_loc : Gram.Loc.t) -> (bi : 'fun_binding))));
                           ([ Gram.Stry
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (labeled_ipatt :
                                        'labeled_ipatt Gram.Entry.t)));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'fun_binding) (p : 'labeled_ipatt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExFun (_loc,
                                     (Ast.McArr (_loc, p, (Ast.ExNil _loc),
                                        e))) :
                                    'fun_binding))));
                           ([ Gram.Stry
                                (Gram.srules fun_binding
                                   [ ([ Gram.Skeyword "(";
                                        Gram.Skeyword "type" ],
                                      (Gram.Action.mk
                                         (fun _ _ (_loc : Gram.Loc.t) ->
                                            (() : 'e__6)))) ]);
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ")"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'fun_binding) _ (i : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExFUN (_loc, i, e) : 'fun_binding)))) ]) ]))
                    ());
               Gram.extend (match_case : 'match_case Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ipatt : 'ipatt Gram.Entry.t));
                              Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (p : 'ipatt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.McArr (_loc, p, (Ast.ExNil _loc), e) :
                                    'match_case))));
                           ([ Gram.Skeyword "[";
                              Gram.Slist0sep
                                ((Gram.Snterm
                                    (Gram.Entry.obj
                                       (match_case0 :
                                         'match_case0 Gram.Entry.t))),
                                (Gram.Skeyword "|"));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (l : 'match_case0 list) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.mcOr_of_list l : 'match_case)))) ]) ]))
                    ());
               Gram.extend (match_case0 : 'match_case0 Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (patt_as_patt_opt :
                                     'patt_as_patt_opt Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_when_expr :
                                     'opt_when_expr Gram.Entry.t));
                              Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (w : 'opt_when_expr)
                                  (p : 'patt_as_patt_opt) (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.McArr (_loc, p, w, e) : 'match_case0))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"anti\"), _)"));
                              Gram.Skeyword "when";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (w : 'expr) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "anti" as n)), s) ->
                                      (Ast.McArr (_loc,
                                         (Ast.PaAnt (_loc,
                                            (mk_anti ~c: "patt" n s))),
                                         w, e) :
                                        'match_case0)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"anti\"), _)"));
                              Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "anti" as n)), s) ->
                                      (Ast.McArr (_loc,
                                         (Ast.PaAnt (_loc,
                                            (mk_anti ~c: "patt" n s))),
                                         (Ast.ExNil _loc), e) :
                                        'match_case0)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "anti" as n)), s) ->
                                      (Ast.McAnt (_loc,
                                         (mk_anti ~c: "match_case" n s)) :
                                        'match_case0)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("match_case" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"match_case\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("match_case" | "list" as n)),
                                      s) ->
                                      (Ast.McAnt (_loc,
                                         (mk_anti ~c: "match_case" n s)) :
                                        'match_case0)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (opt_when_expr : 'opt_when_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.ExNil _loc : 'opt_when_expr))));
                           ([ Gram.Skeyword "when";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (w : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (w : 'opt_when_expr)))) ]) ]))
                    ());
               Gram.extend
                 (patt_as_patt_opt : 'patt_as_patt_opt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'patt) (_loc : Gram.Loc.t) ->
                                  (p : 'patt_as_patt_opt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword "as";
                              Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p2 : 'patt) _ (p1 : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaAli (_loc, p1, p2) :
                                    'patt_as_patt_opt)))) ]) ]))
                    ());
               Gram.extend (label_expr_list : 'label_expr_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_expr : 'label_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (b1 : 'label_expr) (_loc : Gram.Loc.t) ->
                                  (b1 : 'label_expr_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_expr : 'label_expr Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (b1 : 'label_expr) (_loc : Gram.Loc.t)
                                  -> (b1 : 'label_expr_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_expr : 'label_expr Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (b2 : 'label_expr_list) _
                                  (b1 : 'label_expr) (_loc : Gram.Loc.t) ->
                                  (Ast.RbSem (_loc, b1, b2) :
                                    'label_expr_list)))) ]) ]))
                    ());
               Gram.extend (label_expr : 'label_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_longident :
                                     'label_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'label_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.RbEq (_loc, i,
                                     (Ast.ExId (_loc,
                                        (Ast.IdLid (_loc, (lid_of_ident i)))))) :
                                    'label_expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_longident :
                                     'label_longident Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_binding : 'fun_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'fun_binding) (i : 'label_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.RbEq (_loc, i, e) : 'label_expr))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.RbAnt (_loc,
                                         (mk_anti ~c: "rec_binding" n s)) :
                                        'label_expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"anti\"), _)"));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "anti" as n)), s) ->
                                      (Ast.RbEq (_loc,
                                         (Ast.IdAnt (_loc,
                                            (mk_anti ~c: "ident" n s))),
                                         e) :
                                        'label_expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "anti" as n)), s) ->
                                      (Ast.RbAnt (_loc,
                                         (mk_anti ~c: "rec_binding" n s)) :
                                        'label_expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("rec_binding", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"rec_binding\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("rec_binding" as n)), s) ->
                                      (Ast.RbAnt (_loc,
                                         (mk_anti ~c: "rec_binding" n s)) :
                                        'label_expr)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (fun_def : 'fun_def Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stry
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (labeled_ipatt :
                                        'labeled_ipatt Gram.Entry.t)));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_def_cont :
                                     'fun_def_cont Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun ((w, e) : 'fun_def_cont)
                                  (p : 'labeled_ipatt) (_loc : Gram.Loc.t) ->
                                  (Ast.ExFun (_loc,
                                     (Ast.McArr (_loc, p, w, e))) :
                                    'fun_def))));
                           ([ Gram.Stry
                                (Gram.srules fun_def
                                   [ ([ Gram.Skeyword "(";
                                        Gram.Skeyword "type" ],
                                      (Gram.Action.mk
                                         (fun _ _ (_loc : Gram.Loc.t) ->
                                            (() : 'e__7)))) ]);
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ")";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_def_cont_no_when :
                                     'fun_def_cont_no_when Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'fun_def_cont_no_when) _
                                  (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExFUN (_loc, i, e) : 'fun_def)))) ]) ]))
                    ());
               Gram.extend (fun_def_cont : 'fun_def_cont Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (((Ast.ExNil _loc), e) : 'fun_def_cont))));
                           ([ Gram.Skeyword "when";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (w : 'expr) _
                                  (_loc : Gram.Loc.t) ->
                                  ((w, e) : 'fun_def_cont))));
                           ([ Gram.Stry
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (labeled_ipatt :
                                        'labeled_ipatt Gram.Entry.t)));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun ((w, e) : 'fun_def_cont)
                                  (p : 'labeled_ipatt) (_loc : Gram.Loc.t) ->
                                  (((Ast.ExNil _loc),
                                    (Ast.ExFun (_loc,
                                       (Ast.McArr (_loc, p, w, e))))) :
                                    'fun_def_cont))));
                           ([ Gram.Stry
                                (Gram.srules fun_def_cont
                                   [ ([ Gram.Skeyword "(";
                                        Gram.Skeyword "type" ],
                                      (Gram.Action.mk
                                         (fun _ _ (_loc : Gram.Loc.t) ->
                                            (() : 'e__8)))) ]);
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ")";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_def_cont_no_when :
                                     'fun_def_cont_no_when Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'fun_def_cont_no_when) _
                                  (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (((Ast.ExNil _loc),
                                    (Ast.ExFUN (_loc, i, e))) :
                                    'fun_def_cont)))) ]) ]))
                    ());
               Gram.extend
                 (fun_def_cont_no_when : 'fun_def_cont_no_when Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (e : 'fun_def_cont_no_when))));
                           ([ Gram.Stry
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (labeled_ipatt :
                                        'labeled_ipatt Gram.Entry.t)));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_def_cont :
                                     'fun_def_cont Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun ((w, e) : 'fun_def_cont)
                                  (p : 'labeled_ipatt) (_loc : Gram.Loc.t) ->
                                  (Ast.ExFun (_loc,
                                     (Ast.McArr (_loc, p, w, e))) :
                                    'fun_def_cont_no_when))));
                           ([ Gram.Stry
                                (Gram.srules fun_def_cont_no_when
                                   [ ([ Gram.Skeyword "(";
                                        Gram.Skeyword "type" ],
                                      (Gram.Action.mk
                                         (fun _ _ (_loc : Gram.Loc.t) ->
                                            (() : 'e__9)))) ]);
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ")"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (e : 'fun_def_cont_no_when) _
                                  (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.ExFUN (_loc, i, e) :
                                    'fun_def_cont_no_when)))) ]) ]))
                    ());
               Gram.extend (patt : 'patt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "|"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself; Gram.Skeyword "|"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'patt) _ (p1 : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaOrp (_loc, p1, p2) : 'patt)))) ]);
                        ((Some ".."), (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Sself; Gram.Skeyword ".."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'patt) _ (p1 : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaRng (_loc, p1, p2) : 'patt)))) ]);
                        ((Some "apply"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Skeyword "lazy"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p : 'patt) _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaLaz (_loc, p) : 'patt))));
                           ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'patt) (p1 : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaApp (_loc, p1, p2) : 'patt)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "?"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (patt_tcon : 'patt_tcon Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (e : 'expr) _ (p : 'patt_tcon) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaOlbi (_loc, "", p, e) : 'patt))));
                           ([ Gram.Skeyword "?"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (patt_tcon : 'patt_tcon Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p : 'patt_tcon) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaOlb (_loc, "", p) : 'patt))));
                           ([ Gram.Skeyword "?";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (Ast.PaOlb (_loc, (mk_anti n i),
                                         (Ast.PaNil _loc)) :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "?";
                              Gram.Stoken
                                (((function | LIDENT _ -> true | _ -> false),
                                  "LIDENT _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LIDENT i ->
                                      (Ast.PaOlb (_loc, i, (Ast.PaNil _loc)) :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "?";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)"));
                              Gram.Skeyword ":"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (patt_tcon : 'patt_tcon Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (eq_expr : 'eq_expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (f : 'eq_expr) (p : 'patt_tcon) _ _
                                  (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (f (mk_anti n i) p : 'patt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function | OPTLABEL _ -> true | _ -> false),
                                  "OPTLABEL _"));
                              Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (patt_tcon : 'patt_tcon Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (eq_expr : 'eq_expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (f : 'eq_expr) (p : 'patt_tcon) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | OPTLABEL i -> (f i p : 'patt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Stoken
                                (((function | LIDENT _ -> true | _ -> false),
                                  "LIDENT _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LIDENT i ->
                                      (Ast.PaLab (_loc, i, (Ast.PaNil _loc)) :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (Ast.PaLab (_loc, (mk_anti n i),
                                         (Ast.PaNil _loc)) :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)"));
                              Gram.Skeyword ":"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p : 'patt) _ (__camlp4_0 : Gram.Token.t)
                                  _ (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (Ast.PaLab (_loc, (mk_anti n i), p) :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function | LABEL _ -> true | _ -> false),
                                  "LABEL _"));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p : 'patt) (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LABEL i ->
                                      (Ast.PaLab (_loc, i, p) : 'patt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_longident :
                                     'type_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'type_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyp (_loc, i) : 'patt))));
                           ([ Gram.Skeyword "`";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaVrn (_loc, s) : 'patt))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.patt_tag :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaAny _loc : 'patt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ",";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_patt : 'comma_patt Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (pl : 'comma_patt) _ (p : 'patt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTup (_loc,
                                     (Ast.PaCom (_loc, p, pl))) :
                                    'patt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword "as"; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p2 : 'patt) _ (p : 'patt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaAli (_loc, p, p2) : 'patt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t : 'ctyp) _ (p : 'patt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyc (_loc, p, t) : 'patt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p : 'patt) _ (_loc : Gram.Loc.t) ->
                                  (p : 'patt))));
                           ([ Gram.Skeyword "("; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (package_type :
                                     'package_type Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (pt : 'package_type) _ (m : 'a_UIDENT)
                                  _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyc (_loc, (Ast.PaMod (_loc, m)),
                                     (Ast.TyPkg (_loc, pt))) :
                                    'patt))));
                           ([ Gram.Skeyword "("; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (m : 'a_UIDENT) _ _ (_loc : Gram.Loc.t)
                                  -> (Ast.PaMod (_loc, m) : 'patt))));
                           ([ Gram.Skeyword "("; Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaId (_loc, (Ast.IdUid (_loc, "()"))) :
                                    'patt))));
                           ([ Gram.Skeyword "{";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (label_patt_list :
                                     'label_patt_list Gram.Entry.t));
                              Gram.Skeyword "}" ],
                            (Gram.Action.mk
                               (fun _ (pl : 'label_patt_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaRec (_loc, pl) : 'patt))));
                           ([ Gram.Skeyword "[|";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_patt : 'sem_patt Gram.Entry.t));
                              Gram.Skeyword "|]" ],
                            (Gram.Action.mk
                               (fun _ (pl : 'sem_patt) _ (_loc : Gram.Loc.t)
                                  -> (Ast.PaArr (_loc, pl) : 'patt))));
                           ([ Gram.Skeyword "[|"; Gram.Skeyword "|]" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaArr (_loc, (Ast.PaNil _loc)) :
                                    'patt))));
                           ([ Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_patt_for_list :
                                     'sem_patt_for_list Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (mk_list : 'sem_patt_for_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (mk_list
                                     (Ast.PaId (_loc,
                                        (Ast.IdUid (_loc, "[]")))) :
                                    'patt))));
                           ([ Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_patt_for_list :
                                     'sem_patt_for_list Gram.Entry.t));
                              Gram.Skeyword "::"; Gram.Sself;
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (last : 'patt) _
                                  (mk_list : 'sem_patt_for_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (mk_list last : 'patt))));
                           ([ Gram.Skeyword "["; Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaId (_loc, (Ast.IdUid (_loc, "[]"))) :
                                    'patt))));
                           ([ Gram.Skeyword "-";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_FLOAT : 'a_FLOAT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_FLOAT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaFlo (_loc, (neg_string s)) : 'patt))));
                           ([ Gram.Skeyword "-";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_NATIVEINT : 'a_NATIVEINT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_NATIVEINT) _ (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.PaNativeInt (_loc, (neg_string s)) :
                                    'patt))));
                           ([ Gram.Skeyword "-";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_INT64 : 'a_INT64 Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT64) _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaInt64 (_loc, (neg_string s)) :
                                    'patt))));
                           ([ Gram.Skeyword "-";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_INT32 : 'a_INT32 Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT32) _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaInt32 (_loc, (neg_string s)) :
                                    'patt))));
                           ([ Gram.Skeyword "-";
                              Gram.Snterm
                                (Gram.Entry.obj (a_INT : 'a_INT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT) _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaInt (_loc, (neg_string s)) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_CHAR : 'a_CHAR Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_CHAR) (_loc : Gram.Loc.t) ->
                                  (Ast.PaChr (_loc, s) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_STRING : 'a_STRING Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_STRING) (_loc : Gram.Loc.t) ->
                                  (Ast.PaStr (_loc, s) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_FLOAT : 'a_FLOAT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_FLOAT) (_loc : Gram.Loc.t) ->
                                  (Ast.PaFlo (_loc, s) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_NATIVEINT : 'a_NATIVEINT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_NATIVEINT) (_loc : Gram.Loc.t) ->
                                  (Ast.PaNativeInt (_loc, s) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_INT64 : 'a_INT64 Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT64) (_loc : Gram.Loc.t) ->
                                  (Ast.PaInt64 (_loc, s) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_INT32 : 'a_INT32 Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT32) (_loc : Gram.Loc.t) ->
                                  (Ast.PaInt32 (_loc, s) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (a_INT : 'a_INT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_INT) (_loc : Gram.Loc.t) ->
                                  (Ast.PaInt (_loc, s) : 'patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (ident : 'ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'ident) (_loc : Gram.Loc.t) ->
                                  (Ast.PaId (_loc, i) : 'patt))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("`bool", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"`bool\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("`bool" as n)), s) ->
                                      (Ast.PaId (_loc,
                                         (Ast.IdAnt (_loc, (mk_anti n s)))) :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("tup", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"tup\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("tup" as n)), s) ->
                                      (Ast.PaTup (_loc,
                                         (Ast.PaAnt (_loc,
                                            (mk_anti ~c: "patt" n s)))) :
                                        'patt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "pat" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"pat\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "pat" | "anti" as n)),
                                      s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt" n s)) :
                                        'patt)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (comma_patt : 'comma_patt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'patt) (_loc : Gram.Loc.t) ->
                                  (p : 'comma_patt))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt," n s)) :
                                        'comma_patt)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword ","; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'comma_patt) _ (p1 : 'comma_patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaCom (_loc, p1, p2) : 'comma_patt)))) ]) ]))
                    ());
               Gram.extend (sem_patt : 'sem_patt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'patt) (_loc : Gram.Loc.t) ->
                                  (p : 'sem_patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (p : 'patt) (_loc : Gram.Loc.t) ->
                                  (p : 'sem_patt))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt;" n s)) :
                                        'sem_patt)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'sem_patt) _ (p1 : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, p1, p2) : 'sem_patt)))) ]) ]))
                    ());
               Gram.extend
                 (sem_patt_for_list : 'sem_patt_for_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'patt) (_loc : Gram.Loc.t) ->
                                  (fun acc ->
                                     Ast.PaApp (_loc,
                                       (Ast.PaApp (_loc,
                                          (Ast.PaId (_loc,
                                             (Ast.IdUid (_loc, "::")))),
                                          p)),
                                       acc) :
                                    'sem_patt_for_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (p : 'patt) (_loc : Gram.Loc.t) ->
                                  (fun acc ->
                                     Ast.PaApp (_loc,
                                       (Ast.PaApp (_loc,
                                          (Ast.PaId (_loc,
                                             (Ast.IdUid (_loc, "::")))),
                                          p)),
                                       acc) :
                                    'sem_patt_for_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (pl : 'sem_patt_for_list) _ (p : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (fun acc ->
                                     Ast.PaApp (_loc,
                                       (Ast.PaApp (_loc,
                                          (Ast.PaId (_loc,
                                             (Ast.IdUid (_loc, "::")))),
                                          p)),
                                       (pl acc)) :
                                    'sem_patt_for_list)))) ]) ]))
                    ());
               Gram.extend (label_patt_list : 'label_patt_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_patt : 'label_patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p1 : 'label_patt) (_loc : Gram.Loc.t) ->
                                  (p1 : 'label_patt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_patt : 'label_patt Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (p1 : 'label_patt) (_loc : Gram.Loc.t)
                                  -> (p1 : 'label_patt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_patt : 'label_patt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Skeyword "_";
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ _ _ (p1 : 'label_patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) :
                                    'label_patt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_patt : 'label_patt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ _ (p1 : 'label_patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) :
                                    'label_patt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_patt : 'label_patt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'label_patt_list) _
                                  (p1 : 'label_patt) (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, p1, p2) :
                                    'label_patt_list)))) ]) ]))
                    ());
               Gram.extend (label_patt : 'label_patt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_longident :
                                     'label_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'label_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaEq (_loc, i,
                                     (Ast.PaId (_loc,
                                        (Ast.IdLid (_loc, (lid_of_ident i)))))) :
                                    'label_patt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_longident :
                                     'label_longident Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'patt) _ (i : 'label_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaEq (_loc, i, p) : 'label_patt))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt;" n s)) :
                                        'label_patt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.patt_tag :
                                        'label_patt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "pat" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"pat\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "pat" | "anti" as n)),
                                      s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt" n s)) :
                                        'label_patt)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (ipatt : 'ipatt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaAny _loc : 'ipatt))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.PaId (_loc, (Ast.IdLid (_loc, s))) :
                                    'ipatt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ",";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_ipatt : 'comma_ipatt Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (pl : 'comma_ipatt) _ (p : 'ipatt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTup (_loc,
                                     (Ast.PaCom (_loc, p, pl))) :
                                    'ipatt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword "as"; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p2 : 'ipatt) _ (p : 'ipatt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaAli (_loc, p, p2) : 'ipatt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t : 'ctyp) _ (p : 'ipatt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyc (_loc, p, t) : 'ipatt))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p : 'ipatt) _ (_loc : Gram.Loc.t) ->
                                  (p : 'ipatt))));
                           ([ Gram.Skeyword "("; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (package_type :
                                     'package_type Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (pt : 'package_type) _ (m : 'a_UIDENT)
                                  _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyc (_loc, (Ast.PaMod (_loc, m)),
                                     (Ast.TyPkg (_loc, pt))) :
                                    'ipatt))));
                           ([ Gram.Skeyword "("; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (m : 'a_UIDENT) _ _ (_loc : Gram.Loc.t)
                                  -> (Ast.PaMod (_loc, m) : 'ipatt))));
                           ([ Gram.Skeyword "("; Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.PaId (_loc, (Ast.IdUid (_loc, "()"))) :
                                    'ipatt))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.patt_tag :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("tup", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"tup\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("tup" as n)), s) ->
                                      (Ast.PaTup (_loc,
                                         (Ast.PaAnt (_loc,
                                            (mk_anti ~c: "patt" n s)))) :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "pat" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"pat\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "pat" | "anti" as n)),
                                      s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt" n s)) :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "{";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (label_ipatt_list :
                                     'label_ipatt_list Gram.Entry.t));
                              Gram.Skeyword "}" ],
                            (Gram.Action.mk
                               (fun _ (pl : 'label_ipatt_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaRec (_loc, pl) : 'ipatt)))) ]) ]))
                    ());
               Gram.extend (labeled_ipatt : 'labeled_ipatt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ipatt : 'ipatt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'ipatt) (_loc : Gram.Loc.t) ->
                                  (p : 'labeled_ipatt)))) ]) ]))
                    ());
               Gram.extend (comma_ipatt : 'comma_ipatt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ipatt : 'ipatt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'ipatt) (_loc : Gram.Loc.t) ->
                                  (p : 'comma_ipatt))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt," n s)) :
                                        'comma_ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword ","; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'comma_ipatt) _ (p1 : 'comma_ipatt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaCom (_loc, p1, p2) : 'comma_ipatt)))) ]) ]))
                    ());
               Gram.extend
                 (label_ipatt_list : 'label_ipatt_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_ipatt : 'label_ipatt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p1 : 'label_ipatt) (_loc : Gram.Loc.t)
                                  -> (p1 : 'label_ipatt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_ipatt : 'label_ipatt Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (p1 : 'label_ipatt) (_loc : Gram.Loc.t)
                                  -> (p1 : 'label_ipatt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_ipatt : 'label_ipatt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Skeyword "_";
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ _ _ (p1 : 'label_ipatt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) :
                                    'label_ipatt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_ipatt : 'label_ipatt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ _ (p1 : 'label_ipatt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) :
                                    'label_ipatt_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_ipatt : 'label_ipatt Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p2 : 'label_ipatt_list) _
                                  (p1 : 'label_ipatt) (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, p1, p2) :
                                    'label_ipatt_list)))) ]) ]))
                    ());
               Gram.extend (label_ipatt : 'label_ipatt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_longident :
                                     'label_longident Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (ipatt : 'ipatt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'ipatt) _ (i : 'label_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaEq (_loc, i, p) : 'label_ipatt))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.patt_tag :
                                        'label_ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt;" n s)) :
                                        'label_ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "pat" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"pat\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "pat" | "anti" as n)),
                                      s) ->
                                      (Ast.PaAnt (_loc,
                                         (mk_anti ~c: "patt" n s)) :
                                        'label_ipatt)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (type_declaration : 'type_declaration Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_ident_and_parameters :
                                     'type_ident_and_parameters Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_eq_ctyp : 'opt_eq_ctyp Gram.Entry.t));
                              Gram.Slist0
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (constrain : 'constrain Gram.Entry.t))) ],
                            (Gram.Action.mk
                               (fun (cl : 'constrain list)
                                  (tk : 'opt_eq_ctyp)
                                  ((n, tpl) : 'type_ident_and_parameters)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyDcl (_loc, n, tpl, tk, cl) :
                                    'type_declaration))));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'type_declaration) _
                                  (t1 : 'type_declaration)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyAnd (_loc, t1, t2) :
                                    'type_declaration))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'type_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctypand" n s)) :
                                        'type_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" | "anti" as n)),
                                      s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'type_declaration)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (constrain : 'constrain Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "constraint";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'ctyp) _
                                  (_loc : Gram.Loc.t) ->
                                  ((t1, t2) : 'constrain)))) ]) ]))
                    ());
               Gram.extend (opt_eq_ctyp : 'opt_eq_ctyp Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.TyNil _loc : 'opt_eq_ctyp))));
                           ([ Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_kind : 'type_kind Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (tk : 'type_kind) _ (_loc : Gram.Loc.t)
                                  -> (tk : 'opt_eq_ctyp)))) ]) ]))
                    ());
               Gram.extend (type_kind : 'type_kind Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (t : 'type_kind)))) ]) ]))
                    ());
               Gram.extend
                 (type_ident_and_parameters :
                   'type_ident_and_parameters Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Slist0
                                (Gram.Snterm
                                   (Gram.Entry.obj
                                      (optional_type_parameter :
                                        'optional_type_parameter Gram.Entry.t))) ],
                            (Gram.Action.mk
                               (fun (tpl : 'optional_type_parameter list)
                                  (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  ((i, tpl) : 'type_ident_and_parameters)))) ]) ]))
                    ());
               Gram.extend
                 (type_longident_and_parameters :
                   'type_longident_and_parameters Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_longident :
                                     'type_longident Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (type_parameters :
                                     'type_parameters Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (tpl : 'type_parameters)
                                  (i : 'type_longident) (_loc : Gram.Loc.t)
                                  ->
                                  (tpl (Ast.TyId (_loc, i)) :
                                    'type_longident_and_parameters)))) ]) ]))
                    ());
               Gram.extend (type_parameters : 'type_parameters Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (fun t -> t : 'type_parameters))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_parameter :
                                     'type_parameter Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'type_parameter) (_loc : Gram.Loc.t)
                                  ->
                                  (fun acc -> Ast.TyApp (_loc, acc, t) :
                                    'type_parameters))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_parameter :
                                     'type_parameter Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'type_parameters)
                                  (t1 : 'type_parameter) (_loc : Gram.Loc.t)
                                  ->
                                  (fun acc -> t2 (Ast.TyApp (_loc, acc, t1)) :
                                    'type_parameters)))) ]) ]))
                    ());
               Gram.extend (type_parameter : 'type_parameter Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "-"; Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuM (_loc, i) : 'type_parameter))));
                           ([ Gram.Skeyword "+"; Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuP (_loc, i) : 'type_parameter))));
                           ([ Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuo (_loc, i) : 'type_parameter))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'type_parameter)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" | "anti" as n)),
                                      s) ->
                                      (Ast.TyAnt (_loc, (mk_anti n s)) :
                                        'type_parameter)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (optional_type_parameter :
                   'optional_type_parameter Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyAny _loc : 'optional_type_parameter))));
                           ([ Gram.Skeyword "-"; Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyAnM _loc : 'optional_type_parameter))));
                           ([ Gram.Skeyword "+"; Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyAnP _loc : 'optional_type_parameter))));
                           ([ Gram.Skeyword "-"; Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuM (_loc, i) :
                                    'optional_type_parameter))));
                           ([ Gram.Skeyword "+"; Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuP (_loc, i) :
                                    'optional_type_parameter))));
                           ([ Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuo (_loc, i) :
                                    'optional_type_parameter))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'optional_type_parameter)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" | "anti" as n)),
                                      s) ->
                                      (Ast.TyAnt (_loc, (mk_anti n s)) :
                                        'optional_type_parameter)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (ctyp : 'ctyp Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "=="), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself; Gram.Skeyword "=="; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyMan (_loc, t1, t2) : 'ctyp)))) ]);
                        ((Some "private"), (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Skeyword "private";
                              Gram.Snterml
                                ((Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)),
                                "alias") ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyPrv (_loc, t) : 'ctyp)))) ]);
                        ((Some "alias"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself; Gram.Skeyword "as"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyAli (_loc, t1, t2) : 'ctyp)))) ]);
                        ((Some "forall"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Skeyword "!";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (typevars : 'typevars Gram.Entry.t));
                              Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'typevars) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyPol (_loc, t1, t2) : 'ctyp)))) ]);
                        ((Some "arrow"), (Some Camlp4.Sig.Grammar.RightA),
                         [ ([ Gram.Sself; Gram.Skeyword "->"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyArr (_loc, t1, t2) : 'ctyp)))) ]);
                        ((Some "label"), (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_OPTLABEL : 'a_OPTLABEL Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (i : 'a_OPTLABEL)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyOlb (_loc, i, t) : 'ctyp))));
                           ([ Gram.Skeyword "?";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (i : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyOlb (_loc, i, t) : 'ctyp))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LABEL : 'a_LABEL Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (i : 'a_LABEL)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyLab (_loc, i, t) : 'ctyp))));
                           ([ Gram.Skeyword "~";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (i : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyLab (_loc, i, t) : 'ctyp)))) ]);
                        ((Some "apply"), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) (t1 : 'ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (let t = Ast.TyApp (_loc, t1, t2)
                                   in
                                     try
                                       Ast.TyId (_loc, (Ast.ident_of_ctyp t))
                                     with | Invalid_argument _ -> t :
                                    'ctyp)))) ]);
                        ((Some "."), (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Sself; Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (try
                                     Ast.TyId (_loc,
                                       (Ast.IdAcc (_loc,
                                          (Ast.ident_of_ctyp t1),
                                          (Ast.ident_of_ctyp t2))))
                                   with
                                   | Invalid_argument s ->
                                       raise (Stream.Error s) :
                                    'ctyp)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "("; Gram.Skeyword "module";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (package_type :
                                     'package_type Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p : 'package_type) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyPkg (_loc, p) : 'ctyp))));
                           ([ Gram.Skeyword "<";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_meth_list :
                                     'opt_meth_list Gram.Entry.t));
                              Gram.Skeyword ">" ],
                            (Gram.Action.mk
                               (fun _ (t : 'opt_meth_list) _
                                  (_loc : Gram.Loc.t) -> (t : 'ctyp))));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_longident :
                                     'class_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'class_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCls (_loc, i) : 'ctyp))));
                           ([ Gram.Skeyword "{";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (label_declaration_list :
                                     'label_declaration_list Gram.Entry.t));
                              Gram.Skeyword "}" ],
                            (Gram.Action.mk
                               (fun _ (t : 'label_declaration_list) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyRec (_loc, t) : 'ctyp))));
                           ([ Gram.Skeyword "[<";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (row_field : 'row_field Gram.Entry.t));
                              Gram.Skeyword ">";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (name_tags : 'name_tags Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (ntl : 'name_tags) _ (rfl : 'row_field)
                                  _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrnInfSup (_loc, rfl, ntl) : 'ctyp))));
                           ([ Gram.Skeyword "[<";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (row_field : 'row_field Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (rfl : 'row_field) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrnInf (_loc, rfl) : 'ctyp))));
                           ([ Gram.Skeyword "["; Gram.Skeyword "<";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (row_field : 'row_field Gram.Entry.t));
                              Gram.Skeyword ">";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (name_tags : 'name_tags Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (ntl : 'name_tags) _ (rfl : 'row_field)
                                  _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrnInfSup (_loc, rfl, ntl) : 'ctyp))));
                           ([ Gram.Skeyword "["; Gram.Skeyword "<";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (row_field : 'row_field Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (rfl : 'row_field) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrnInf (_loc, rfl) : 'ctyp))));
                           ([ Gram.Skeyword "["; Gram.Skeyword ">";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (row_field : 'row_field Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (rfl : 'row_field) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrnSup (_loc, rfl) : 'ctyp))));
                           ([ Gram.Skeyword "["; Gram.Skeyword ">";
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrnSup (_loc, (Ast.TyNil _loc)) :
                                    'ctyp))));
                           ([ Gram.Skeyword "["; Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (row_field : 'row_field Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (rfl : 'row_field) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrnEq (_loc, rfl) : 'ctyp))));
                           ([ Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_declarations :
                                     'constructor_declarations Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (t : 'constructor_declarations) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TySum (_loc, t) : 'ctyp))));
                           ([ Gram.Skeyword "["; Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.TySum (_loc, (Ast.TyNil _loc)) :
                                    'ctyp))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t : 'ctyp) _ (_loc : Gram.Loc.t) ->
                                  (t : 'ctyp))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword "*";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (star_ctyp : 'star_ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (tl : 'star_ctyp) _ (t : 'ctyp) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyTup (_loc,
                                     (Ast.TySta (_loc, t, tl))) :
                                    'ctyp))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.TyId (_loc, (Ast.IdUid (_loc, i))) :
                                    'ctyp))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.TyId (_loc, (Ast.IdLid (_loc, i))) :
                                    'ctyp))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'ctyp)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("id", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"id\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("id" as n)), s) ->
                                      (Ast.TyId (_loc,
                                         (Ast.IdAnt (_loc,
                                            (mk_anti ~c: "ident" n s)))) :
                                        'ctyp)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("tup", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"tup\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("tup" as n)), s) ->
                                      (Ast.TyTup (_loc,
                                         (Ast.TyAnt (_loc,
                                            (mk_anti ~c: "ctyp" n s)))) :
                                        'ctyp)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" | "anti" as n)),
                                      s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'ctyp)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "_" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyAny _loc : 'ctyp))));
                           ([ Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuo (_loc, i) : 'ctyp)))) ]) ]))
                    ());
               Gram.extend (star_ctyp : 'star_ctyp Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (t : 'star_ctyp))));
                           ([ Gram.Sself; Gram.Skeyword "*"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'star_ctyp) _ (t1 : 'star_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TySta (_loc, t1, t2) : 'star_ctyp))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp*" n s)) :
                                        'star_ctyp)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'star_ctyp)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (constructor_declarations :
                   'constructor_declarations Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.TyId (_loc, (Ast.IdUid (_loc, s))) :
                                    'constructor_declarations))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (s : 'a_UIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (let (tl, rt) = generalized_type_of_type t
                                   in
                                     Ast.TyCol (_loc,
                                       (Ast.TyId (_loc,
                                          (Ast.IdUid (_loc, s)))),
                                       (Ast.TyArr (_loc,
                                          (Ast.tyAnd_of_list tl), rt))) :
                                    'constructor_declarations))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword "of";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_arg_list :
                                     'constructor_arg_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'constructor_arg_list) _
                                  (s : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.TyOf (_loc,
                                     (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))),
                                     t) :
                                    'constructor_declarations))));
                           ([ Gram.Sself; Gram.Skeyword "|"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'constructor_declarations) _
                                  (t1 : 'constructor_declarations)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyOr (_loc, t1, t2) :
                                    'constructor_declarations))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'constructor_declarations)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp|" n s)) :
                                        'constructor_declarations)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'constructor_declarations)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (constructor_declaration :
                   'constructor_declaration Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (s : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.TyId (_loc, (Ast.IdUid (_loc, s))) :
                                    'constructor_declaration))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword "of";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_arg_list :
                                     'constructor_arg_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'constructor_arg_list) _
                                  (s : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.TyOf (_loc,
                                     (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))),
                                     t) :
                                    'constructor_declaration))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'constructor_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'constructor_declaration)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (constructor_arg_list : 'constructor_arg_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (t : 'constructor_arg_list))));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'constructor_arg_list) _
                                  (t1 : 'constructor_arg_list)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyAnd (_loc, t1, t2) :
                                    'constructor_arg_list))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctypand" n s)) :
                                        'constructor_arg_list)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (label_declaration_list :
                   'label_declaration_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_declaration :
                                     'label_declaration Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t1 : 'label_declaration)
                                  (_loc : Gram.Loc.t) ->
                                  (t1 : 'label_declaration_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_declaration :
                                     'label_declaration Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (t1 : 'label_declaration)
                                  (_loc : Gram.Loc.t) ->
                                  (t1 : 'label_declaration_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_declaration :
                                     'label_declaration Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'label_declaration_list) _
                                  (t1 : 'label_declaration)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TySem (_loc, t1, t2) :
                                    'label_declaration_list)))) ]) ]))
                    ());
               Gram.extend
                 (label_declaration : 'label_declaration Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":"; Gram.Skeyword "mutable";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ _ (s : 'a_LIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCol (_loc,
                                     (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))),
                                     (Ast.TyMut (_loc, t))) :
                                    'label_declaration))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (s : 'a_LIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCol (_loc,
                                     (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))),
                                     t) :
                                    'label_declaration))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'label_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp;" n s)) :
                                        'label_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'label_declaration)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_ident : 'a_ident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (i : 'a_ident))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (i : 'a_ident)))) ]) ]))
                    ());
               Gram.extend (ident : 'ident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (j : 'ident) _ (i : 'a_UIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.IdAcc (_loc, (Ast.IdUid (_loc, i)), j) :
                                    'ident))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)"));
                              Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (i : 'ident) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAcc (_loc,
                                         (Ast.IdAnt (_loc,
                                            (mk_anti ~c: "ident" n s))),
                                         i) :
                                        'ident)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdLid (_loc, i) : 'ident))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdUid (_loc, i) : 'ident))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAnt (_loc,
                                         (mk_anti ~c: "ident" n s)) :
                                        'ident)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (module_longident : 'module_longident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdUid (_loc, i) : 'module_longident))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (l : 'module_longident) _ (m : 'a_UIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.IdAcc (_loc, (Ast.IdUid (_loc, m)), l) :
                                    'module_longident))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAnt (_loc,
                                         (mk_anti ~c: "ident" n s)) :
                                        'module_longident)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (module_longident_with_app :
                   'module_longident_with_app Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "apply"), None,
                         [ ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (j : 'module_longident_with_app)
                                  (i : 'module_longident_with_app)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.IdApp (_loc, i, j) :
                                    'module_longident_with_app)))) ]);
                        ((Some "."), None,
                         [ ([ Gram.Sself; Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (j : 'module_longident_with_app) _
                                  (i : 'module_longident_with_app)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.IdAcc (_loc, i, j) :
                                    'module_longident_with_app)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (i : 'module_longident_with_app) _
                                  (_loc : Gram.Loc.t) ->
                                  (i : 'module_longident_with_app))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdUid (_loc, i) :
                                    'module_longident_with_app))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAnt (_loc,
                                         (mk_anti ~c: "ident" n s)) :
                                        'module_longident_with_app)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (module_longident_dot_lparen :
                   'module_longident_dot_lparen Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword "."; Gram.Skeyword "(" ],
                            (Gram.Action.mk
                               (fun _ _ (i : 'a_UIDENT) (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.IdUid (_loc, i) :
                                    'module_longident_dot_lparen))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (l : 'module_longident_dot_lparen) _
                                  (m : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdAcc (_loc, (Ast.IdUid (_loc, m)), l) :
                                    'module_longident_dot_lparen))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)"));
                              Gram.Skeyword "."; Gram.Skeyword "(" ],
                            (Gram.Action.mk
                               (fun _ _ (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAnt (_loc,
                                         (mk_anti ~c: "ident" n s)) :
                                        'module_longident_dot_lparen)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (type_longident : 'type_longident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "apply"), None,
                         [ ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (j : 'type_longident)
                                  (i : 'type_longident) (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.IdApp (_loc, i, j) : 'type_longident)))) ]);
                        ((Some "."), None,
                         [ ([ Gram.Sself; Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (j : 'type_longident) _
                                  (i : 'type_longident) (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.IdAcc (_loc, i, j) : 'type_longident)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (i : 'type_longident) _
                                  (_loc : Gram.Loc.t) ->
                                  (i : 'type_longident))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdUid (_loc, i) : 'type_longident))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdLid (_loc, i) : 'type_longident))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAnt (_loc,
                                         (mk_anti ~c: "ident" n s)) :
                                        'type_longident)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (label_longident : 'label_longident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdLid (_loc, i) : 'label_longident))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (l : 'label_longident) _ (m : 'a_UIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.IdAcc (_loc, (Ast.IdUid (_loc, m)), l) :
                                    'label_longident))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAnt (_loc,
                                         (mk_anti ~c: "ident" n s)) :
                                        'label_longident)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (class_type_longident : 'class_type_longident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_longident :
                                     'type_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'type_longident) (_loc : Gram.Loc.t)
                                  -> (x : 'class_type_longident)))) ]) ]))
                    ());
               Gram.extend (val_longident : 'val_longident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ident : 'ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'ident) (_loc : Gram.Loc.t) ->
                                  (x : 'val_longident)))) ]) ]))
                    ());
               Gram.extend (class_longident : 'class_longident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_longident :
                                     'label_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'label_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (x : 'class_longident)))) ]) ]))
                    ());
               Gram.extend
                 (class_declaration : 'class_declaration Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_info_for_class_expr :
                                     'class_info_for_class_expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_fun_binding :
                                     'class_fun_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ce : 'class_fun_binding)
                                  (ci : 'class_info_for_class_expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeEq (_loc, ci, ce) :
                                    'class_declaration))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.class_expr_tag :
                                        'class_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "cdcl" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"cdcl\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "cdcl" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CeAnt (_loc,
                                         (mk_anti ~c: "class_expr" n s)) :
                                        'class_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (c2 : 'class_declaration) _
                                  (c1 : 'class_declaration)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeAnd (_loc, c1, c2) :
                                    'class_declaration)))) ]) ]))
                    ());
               Gram.extend
                 (class_fun_binding : 'class_fun_binding Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (labeled_ipatt :
                                     'labeled_ipatt Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (cfb : 'class_fun_binding)
                                  (p : 'labeled_ipatt) (_loc : Gram.Loc.t) ->
                                  (Ast.CeFun (_loc, p, cfb) :
                                    'class_fun_binding))));
                           ([ Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_plus :
                                     'class_type_plus Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_expr : 'class_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ce : 'class_expr) _
                                  (ct : 'class_type_plus) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeTyc (_loc, ce, ct) :
                                    'class_fun_binding))));
                           ([ Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_expr : 'class_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ce : 'class_expr) _ (_loc : Gram.Loc.t)
                                  -> (ce : 'class_fun_binding)))) ]) ]))
                    ());
               Gram.extend
                 (class_info_for_class_type :
                   'class_info_for_class_type Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_virtual : 'opt_virtual Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_name_and_param :
                                     'class_name_and_param Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun ((i, ot) : 'class_name_and_param)
                                  (mv : 'opt_virtual) (_loc : Gram.Loc.t) ->
                                  (Ast.CtCon (_loc, mv,
                                     (Ast.IdLid (_loc, i)), ot) :
                                    'class_info_for_class_type)))) ]) ]))
                    ());
               Gram.extend
                 (class_info_for_class_expr :
                   'class_info_for_class_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_virtual : 'opt_virtual Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_name_and_param :
                                     'class_name_and_param Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun ((i, ot) : 'class_name_and_param)
                                  (mv : 'opt_virtual) (_loc : Gram.Loc.t) ->
                                  (Ast.CeCon (_loc, mv,
                                     (Ast.IdLid (_loc, i)), ot) :
                                    'class_info_for_class_expr)))) ]) ]))
                    ());
               Gram.extend
                 (class_name_and_param : 'class_name_and_param Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  ((i, (Ast.TyNil _loc)) :
                                    'class_name_and_param))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_type_parameter :
                                     'comma_type_parameter Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (x : 'comma_type_parameter) _
                                  (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  ((i, x) : 'class_name_and_param)))) ]) ]))
                    ());
               Gram.extend
                 (comma_type_parameter : 'comma_type_parameter Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_parameter :
                                     'type_parameter Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'type_parameter) (_loc : Gram.Loc.t)
                                  -> (t : 'comma_type_parameter))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp," n s)) :
                                        'comma_type_parameter)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword ","; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'comma_type_parameter) _
                                  (t1 : 'comma_type_parameter)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCom (_loc, t1, t2) :
                                    'comma_type_parameter)))) ]) ]))
                    ());
               Gram.extend (opt_comma_ctyp : 'opt_comma_ctyp Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.TyNil _loc : 'opt_comma_ctyp))));
                           ([ Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_ctyp : 'comma_ctyp Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (x : 'comma_ctyp) _ (_loc : Gram.Loc.t)
                                  -> (x : 'opt_comma_ctyp)))) ]) ]))
                    ());
               Gram.extend (comma_ctyp : 'comma_ctyp Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (t : 'comma_ctyp))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp," n s)) :
                                        'comma_ctyp)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword ","; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'comma_ctyp) _ (t1 : 'comma_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCom (_loc, t1, t2) : 'comma_ctyp)))) ]) ]))
                    ());
               Gram.extend (class_fun_def : 'class_fun_def Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "->";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_expr : 'class_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ce : 'class_expr) _ (_loc : Gram.Loc.t)
                                  -> (ce : 'class_fun_def))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (labeled_ipatt :
                                     'labeled_ipatt Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (ce : 'class_fun_def)
                                  (p : 'labeled_ipatt) (_loc : Gram.Loc.t) ->
                                  (Ast.CeFun (_loc, p, ce) : 'class_fun_def)))) ]) ]))
                    ());
               Gram.extend (class_expr : 'class_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "top"), None,
                         [ ([ Gram.Skeyword "let";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_rec : 'opt_rec Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (binding : 'binding Gram.Entry.t));
                              Gram.Skeyword "in"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (ce : 'class_expr) _ (bi : 'binding)
                                  (rf : 'opt_rec) _ (_loc : Gram.Loc.t) ->
                                  (Ast.CeLet (_loc, rf, bi, ce) :
                                    'class_expr))));
                           ([ Gram.Skeyword "fun";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (labeled_ipatt :
                                     'labeled_ipatt Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_fun_def :
                                     'class_fun_def Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ce : 'class_fun_def)
                                  (p : 'labeled_ipatt) _ (_loc : Gram.Loc.t)
                                  -> (Ast.CeFun (_loc, p, ce) : 'class_expr)))) ]);
                        ((Some "apply"), (Some Camlp4.Sig.Grammar.NonA),
                         [ ([ Gram.Sself;
                              Gram.Snterml
                                ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                                "label") ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (ce : 'class_expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeApp (_loc, ce, e) : 'class_expr)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (ce : 'class_expr) _
                                  (_loc : Gram.Loc.t) -> (ce : 'class_expr))));
                           ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type : 'class_type Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (ct : 'class_type) _ (ce : 'class_expr)
                                  _ (_loc : Gram.Loc.t) ->
                                  (Ast.CeTyc (_loc, ce, ct) : 'class_expr))));
                           ([ Gram.Skeyword "object";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_class_self_patt :
                                     'opt_class_self_patt Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_structure :
                                     'class_structure Gram.Entry.t));
                              Gram.Skeyword "end" ],
                            (Gram.Action.mk
                               (fun _ (cst : 'class_structure)
                                  (csp : 'opt_class_self_patt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeStr (_loc, csp, cst) : 'class_expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_longident_and_param :
                                     'class_longident_and_param Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ce : 'class_longident_and_param)
                                  (_loc : Gram.Loc.t) -> (ce : 'class_expr))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.class_expr_tag :
                                        'class_expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "cexp" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"cexp\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "cexp" | "anti" as n)),
                                      s) ->
                                      (Ast.CeAnt (_loc,
                                         (mk_anti ~c: "class_expr" n s)) :
                                        'class_expr)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (class_longident_and_param :
                   'class_longident_and_param Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_longident :
                                     'class_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ci : 'class_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeCon (_loc, Ast.ViNil, ci,
                                     (Ast.TyNil _loc)) :
                                    'class_longident_and_param))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_longident :
                                     'class_longident Gram.Entry.t));
                              Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_ctyp : 'comma_ctyp Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (t : 'comma_ctyp) _
                                  (ci : 'class_longident) (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.CeCon (_loc, Ast.ViNil, ci, t) :
                                    'class_longident_and_param)))) ]) ]))
                    ());
               Gram.extend (class_structure : 'class_structure Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Slist0
                                (Gram.srules class_structure
                                   [ ([ Gram.Snterm
                                          (Gram.Entry.obj
                                             (class_str_item :
                                               'class_str_item Gram.Entry.t));
                                        Gram.Snterm
                                          (Gram.Entry.obj
                                             (semi : 'semi Gram.Entry.t)) ],
                                      (Gram.Action.mk
                                         (fun _ (cst : 'class_str_item)
                                            (_loc : Gram.Loc.t) ->
                                            (cst : 'e__10)))) ]) ],
                            (Gram.Action.mk
                               (fun (l : 'e__10 list) (_loc : Gram.Loc.t) ->
                                  (Ast.crSem_of_list l : 'class_structure))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "cst" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"cst\" | \"anti\" | \"list\"), _)"));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (cst : 'class_structure) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "cst" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CrSem (_loc,
                                         (Ast.CrAnt (_loc,
                                            (mk_anti ~c: "class_str_item" n s))),
                                         cst) :
                                        'class_structure)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "cst" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"cst\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "cst" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CrAnt (_loc,
                                         (mk_anti ~c: "class_str_item" n s)) :
                                        'class_structure)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (opt_class_self_patt : 'opt_class_self_patt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.PaNil _loc : 'opt_class_self_patt))));
                           ([ Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t : 'ctyp) _ (p : 'patt) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyc (_loc, p, t) :
                                    'opt_class_self_patt))));
                           ([ Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p : 'patt) _ (_loc : Gram.Loc.t) ->
                                  (p : 'opt_class_self_patt)))) ]) ]))
                    ());
               Gram.extend (class_str_item : 'class_str_item Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Skeyword "initializer";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (se : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (Ast.CrIni (_loc, se) : 'class_str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_constraint :
                                     'type_constraint Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'ctyp) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CrCtr (_loc, t1, t2) :
                                    'class_str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (method_opt_override :
                                     'method_opt_override Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_private : 'opt_private Gram.Entry.t));
                              Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (l : 'label) _
                                  (pf : 'opt_private)
                                  (o : 'method_opt_override)
                                  (_loc : Gram.Loc.t) ->
                                  (if o <> Ast.OvNil
                                   then
                                     raise
                                       (Stream.Error
                                          "override (!) is incompatible with virtual")
                                   else Ast.CrVir (_loc, l, pf, t) :
                                    'class_str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (method_opt_override :
                                     'method_opt_override Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_private : 'opt_private Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_polyt : 'opt_polyt Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (fun_binding : 'fun_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'fun_binding) (topt : 'opt_polyt)
                                  (l : 'label) (pf : 'opt_private)
                                  (o : 'method_opt_override)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CrMth (_loc, l, o, pf, e, topt) :
                                    'class_str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (method_opt_override :
                                     'method_opt_override Gram.Entry.t));
                              Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_private : 'opt_private Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (l : 'label)
                                  (pf : 'opt_private) _
                                  (o : 'method_opt_override)
                                  (_loc : Gram.Loc.t) ->
                                  (if o <> Ast.OvNil
                                   then
                                     raise
                                       (Stream.Error
                                          "override (!) is incompatible with virtual")
                                   else Ast.CrVir (_loc, l, pf, t) :
                                    'class_str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val_opt_override :
                                     'value_val_opt_override Gram.Entry.t));
                              Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_mutable : 'opt_mutable Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (l : 'label)
                                  (mf : 'opt_mutable) _
                                  (o : 'value_val_opt_override)
                                  (_loc : Gram.Loc.t) ->
                                  (if o <> Ast.OvNil
                                   then
                                     raise
                                       (Stream.Error
                                          "override (!) is incompatible with virtual")
                                   else Ast.CrVvr (_loc, l, mf, t) :
                                    'class_str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val_opt_override :
                                     'value_val_opt_override Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_mutable : 'opt_mutable Gram.Entry.t));
                              Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (l : 'label) _
                                  (mf : 'opt_mutable)
                                  (o : 'value_val_opt_override)
                                  (_loc : Gram.Loc.t) ->
                                  (if o <> Ast.OvNil
                                   then
                                     raise
                                       (Stream.Error
                                          "override (!) is incompatible with virtual")
                                   else Ast.CrVvr (_loc, l, mf, t) :
                                    'class_str_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val_opt_override :
                                     'value_val_opt_override Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_mutable : 'opt_mutable Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (cvalue_binding :
                                     'cvalue_binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'cvalue_binding) (lab : 'label)
                                  (mf : 'opt_mutable)
                                  (o : 'value_val_opt_override)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CrVal (_loc, lab, o, mf, e) :
                                    'class_str_item))));
                           ([ Gram.Skeyword "inherit";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_override :
                                     'opt_override Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_expr : 'class_expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_as_lident :
                                     'opt_as_lident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (pb : 'opt_as_lident) (ce : 'class_expr)
                                  (o : 'opt_override) _ (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.CrInh (_loc, o, ce, pb) :
                                    'class_str_item))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.class_str_item_tag :
                                        'class_str_item)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "cst" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"cst\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "cst" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CrAnt (_loc,
                                         (mk_anti ~c: "class_str_item" n s)) :
                                        'class_str_item)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (method_opt_override : 'method_opt_override Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "method" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.OvNil : 'method_opt_override))));
                           ([ Gram.Skeyword "method";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("!" | "override" | "anti"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"!\" | \"override\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("!" | "override" | "anti" as n)), s)
                                      ->
                                      (Ast.OvAnt (mk_anti n s) :
                                        'method_opt_override)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "method"; Gram.Skeyword "!" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.OvOverride : 'method_opt_override)))) ]) ]))
                    ());
               Gram.extend
                 (value_val_opt_override :
                   'value_val_opt_override Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val : 'value_val Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.OvNil : 'value_val_opt_override))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val : 'value_val Gram.Entry.t));
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("!" | "override" | "anti"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"!\" | \"override\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("!" | "override" | "anti" as n)), s)
                                      ->
                                      (Ast.OvAnt (mk_anti n s) :
                                        'value_val_opt_override)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val : 'value_val Gram.Entry.t));
                              Gram.Skeyword "!" ],
                            (Gram.Action.mk
                               (fun _ _ (_loc : Gram.Loc.t) ->
                                  (Ast.OvOverride : 'value_val_opt_override)))) ]) ]))
                    ());
               Gram.extend (opt_as_lident : 'opt_as_lident Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  ("" : 'opt_as_lident))));
                           ([ Gram.Skeyword "as";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) _ (_loc : Gram.Loc.t) ->
                                  (i : 'opt_as_lident)))) ]) ]))
                    ());
               Gram.extend (opt_polyt : 'opt_polyt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.TyNil _loc : 'opt_polyt))));
                           ([ Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (_loc : Gram.Loc.t) ->
                                  (t : 'opt_polyt)))) ]) ]))
                    ());
               Gram.extend (cvalue_binding : 'cvalue_binding Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword ":>";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (t : 'ctyp) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExCoe (_loc, e, (Ast.TyNil _loc), t) :
                                    'cvalue_binding))));
                           ([ Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t));
                              Gram.Skeyword ":>";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (t2 : 'ctyp) _
                                  (t : 'poly_type) _ (_loc : Gram.Loc.t) ->
                                  (match t with
                                   | Ast.TyPol (_, _, _) ->
                                       raise
                                         (Stream.Error
                                            "unexpected polytype here")
                                   | _ -> Ast.ExCoe (_loc, e, t, t2) :
                                    'cvalue_binding))));
                           ([ Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (t : 'poly_type) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExTyc (_loc, e, t) : 'cvalue_binding))));
                           ([ Gram.Skeyword ":"; Gram.Skeyword "type";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (unquoted_typevars :
                                     'unquoted_typevars Gram.Entry.t));
                              Gram.Skeyword ".";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (t2 : 'ctyp) _
                                  (t1 : 'unquoted_typevars) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (let u = Ast.TyTypePol (_loc, t1, t2)
                                   in Ast.ExTyc (_loc, e, u) :
                                    'cvalue_binding))));
                           ([ Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (e : 'cvalue_binding)))) ]) ]))
                    ());
               Gram.extend (label : 'label Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (i : 'label)))) ]) ]))
                    ());
               Gram.extend (class_type : 'class_type Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "object";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_class_self_type :
                                     'opt_class_self_type Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_signature :
                                     'class_signature Gram.Entry.t));
                              Gram.Skeyword "end" ],
                            (Gram.Action.mk
                               (fun _ (csg : 'class_signature)
                                  (cst : 'opt_class_self_type) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtSig (_loc, cst, csg) : 'class_type))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_longident_and_param :
                                     'class_type_longident_and_param Gram.
                                       Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ct : 'class_type_longident_and_param)
                                  (_loc : Gram.Loc.t) -> (ct : 'class_type))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.class_type_tag :
                                        'class_type)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "ctyp" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"ctyp\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "ctyp" | "anti" as n)),
                                      s) ->
                                      (Ast.CtAnt (_loc,
                                         (mk_anti ~c: "class_type" n s)) :
                                        'class_type)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend
                 (class_type_longident_and_param :
                   'class_type_longident_and_param Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_longident :
                                     'class_type_longident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'class_type_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtCon (_loc, Ast.ViNil, i,
                                     (Ast.TyNil _loc)) :
                                    'class_type_longident_and_param))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_longident :
                                     'class_type_longident Gram.Entry.t));
                              Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_ctyp : 'comma_ctyp Gram.Entry.t));
                              Gram.Skeyword "]" ],
                            (Gram.Action.mk
                               (fun _ (t : 'comma_ctyp) _
                                  (i : 'class_type_longident)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtCon (_loc, Ast.ViNil, i, t) :
                                    'class_type_longident_and_param)))) ]) ]))
                    ());
               Gram.extend (class_type_plus : 'class_type_plus Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type : 'class_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ct : 'class_type) (_loc : Gram.Loc.t) ->
                                  (ct : 'class_type_plus))));
                           ([ Gram.Skeyword "[";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "]"; Gram.Skeyword "->"; Gram.
                              Sself ],
                            (Gram.Action.mk
                               (fun (ct : 'class_type_plus) _ _ (t : 'ctyp) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtFun (_loc, t, ct) :
                                    'class_type_plus)))) ]) ]))
                    ());
               Gram.extend
                 (opt_class_self_type : 'opt_class_self_type Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.TyNil _loc : 'opt_class_self_type))));
                           ([ Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (t : 'ctyp) _ (_loc : Gram.Loc.t) ->
                                  (t : 'opt_class_self_type)))) ]) ]))
                    ());
               Gram.extend (class_signature : 'class_signature Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Slist0
                                (Gram.srules class_signature
                                   [ ([ Gram.Snterm
                                          (Gram.Entry.obj
                                             (class_sig_item :
                                               'class_sig_item Gram.Entry.t));
                                        Gram.Snterm
                                          (Gram.Entry.obj
                                             (semi : 'semi Gram.Entry.t)) ],
                                      (Gram.Action.mk
                                         (fun _ (csg : 'class_sig_item)
                                            (_loc : Gram.Loc.t) ->
                                            (csg : 'e__11)))) ]) ],
                            (Gram.Action.mk
                               (fun (l : 'e__11 list) (_loc : Gram.Loc.t) ->
                                  (Ast.cgSem_of_list l : 'class_signature))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "csg" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"csg\" | \"anti\" | \"list\"), _)"));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (csg : 'class_signature) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "csg" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CgSem (_loc,
                                         (Ast.CgAnt (_loc,
                                            (mk_anti ~c: "class_sig_item" n s))),
                                         csg) :
                                        'class_signature)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "csg" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"csg\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "csg" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CgAnt (_loc,
                                         (mk_anti ~c: "class_sig_item" n s)) :
                                        'class_signature)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (class_sig_item : 'class_sig_item Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_constraint :
                                     'type_constraint Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t2 : 'ctyp) _ (t1 : 'ctyp) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CgCtr (_loc, t1, t2) :
                                    'class_sig_item))));
                           ([ Gram.Skeyword "method";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_private : 'opt_private Gram.Entry.t));
                              Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (l : 'label) _
                                  (pf : 'opt_private) _ (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.CgVir (_loc, l, pf, t) :
                                    'class_sig_item))));
                           ([ Gram.Skeyword "method";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_private : 'opt_private Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (l : 'label)
                                  (pf : 'opt_private) _ (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.CgMth (_loc, l, pf, t) :
                                    'class_sig_item))));
                           ([ Gram.Skeyword "method";
                              Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_private : 'opt_private Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (l : 'label)
                                  (pf : 'opt_private) _ _ (_loc : Gram.Loc.t)
                                  ->
                                  (Ast.CgVir (_loc, l, pf, t) :
                                    'class_sig_item))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (value_val : 'value_val Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_mutable : 'opt_mutable Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_virtual : 'opt_virtual Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (l : 'label)
                                  (mv : 'opt_virtual) (mf : 'opt_mutable) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CgVal (_loc, l, mf, mv, t) :
                                    'class_sig_item))));
                           ([ Gram.Skeyword "inherit";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type : 'class_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (cs : 'class_type) _ (_loc : Gram.Loc.t)
                                  -> (Ast.CgInh (_loc, cs) : 'class_sig_item))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.class_sig_item_tag :
                                        'class_sig_item)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "csg" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"csg\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "csg" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CgAnt (_loc,
                                         (mk_anti ~c: "class_sig_item" n s)) :
                                        'class_sig_item)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (type_constraint : 'type_constraint Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "constraint" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (() : 'type_constraint))));
                           ([ Gram.Skeyword "type" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (() : 'type_constraint)))) ]) ]))
                    ());
               Gram.extend
                 (class_description : 'class_description Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_info_for_class_type :
                                     'class_info_for_class_type Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_plus :
                                     'class_type_plus Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ct : 'class_type_plus) _
                                  (ci : 'class_info_for_class_type)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtCol (_loc, ci, ct) :
                                    'class_description))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.class_type_tag :
                                        'class_description)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "typ" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "typ" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CtAnt (_loc,
                                         (mk_anti ~c: "class_type" n s)) :
                                        'class_description)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (cd2 : 'class_description) _
                                  (cd1 : 'class_description)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtAnd (_loc, cd1, cd2) :
                                    'class_description)))) ]) ]))
                    ());
               Gram.extend
                 (class_type_declaration :
                   'class_type_declaration Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_info_for_class_type :
                                     'class_info_for_class_type Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type : 'class_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ct : 'class_type) _
                                  (ci : 'class_info_for_class_type)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtEq (_loc, ci, ct) :
                                    'class_type_declaration))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.class_type_tag :
                                        'class_type_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "typ" | "anti" | "list"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "typ" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.CtAnt (_loc,
                                         (mk_anti ~c: "class_type" n s)) :
                                        'class_type_declaration)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (cd2 : 'class_type_declaration) _
                                  (cd1 : 'class_type_declaration)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtAnd (_loc, cd1, cd2) :
                                    'class_type_declaration)))) ]) ]))
                    ());
               Gram.extend (field_expr_list : 'field_expr_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (field_expr : 'field_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (b1 : 'field_expr) (_loc : Gram.Loc.t) ->
                                  (b1 : 'field_expr_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (field_expr : 'field_expr Gram.Entry.t));
                              Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (b1 : 'field_expr) (_loc : Gram.Loc.t)
                                  -> (b1 : 'field_expr_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (field_expr : 'field_expr Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (b2 : 'field_expr_list) _
                                  (b1 : 'field_expr) (_loc : Gram.Loc.t) ->
                                  (Ast.RbSem (_loc, b1, b2) :
                                    'field_expr_list)))) ]) ]))
                    ());
               Gram.extend (field_expr : 'field_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (label : 'label Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterml
                                ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                                "top") ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (l : 'label)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.RbEq (_loc, (Ast.IdLid (_loc, l)), e) :
                                    'field_expr))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.RbAnt (_loc,
                                         (mk_anti ~c: "rec_binding" n s)) :
                                        'field_expr)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "bi" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"bi\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "bi" | "anti" as n)), s)
                                      ->
                                      (Ast.RbAnt (_loc,
                                         (mk_anti ~c: "rec_binding" n s)) :
                                        'field_expr)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (meth_list : 'meth_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (meth_decl : 'meth_decl Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_dot_dot : 'opt_dot_dot Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (v : 'opt_dot_dot) (m : 'meth_decl)
                                  (_loc : Gram.Loc.t) ->
                                  ((m, v) : 'meth_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (meth_decl : 'meth_decl Gram.Entry.t));
                              Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_dot_dot : 'opt_dot_dot Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (v : 'opt_dot_dot) _ (m : 'meth_decl)
                                  (_loc : Gram.Loc.t) ->
                                  ((m, v) : 'meth_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (meth_decl : 'meth_decl Gram.Entry.t));
                              Gram.Skeyword ";"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun ((ml, v) : 'meth_list) _ (m : 'meth_decl)
                                  (_loc : Gram.Loc.t) ->
                                  (((Ast.TySem (_loc, m, ml)), v) :
                                    'meth_list)))) ]) ]))
                    ());
               Gram.extend (meth_decl : 'meth_decl Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (poly_type : 'poly_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'poly_type) _ (lab : 'a_LIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCol (_loc,
                                     (Ast.TyId (_loc,
                                        (Ast.IdLid (_loc, lab)))),
                                     t) :
                                    'meth_decl))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'meth_decl)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp;" n s)) :
                                        'meth_decl)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'meth_decl)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (opt_meth_list : 'opt_meth_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_dot_dot : 'opt_dot_dot Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (v : 'opt_dot_dot) (_loc : Gram.Loc.t) ->
                                  (Ast.TyObj (_loc, (Ast.TyNil _loc), v) :
                                    'opt_meth_list))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (meth_list : 'meth_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun ((ml, v) : 'meth_list)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyObj (_loc, ml, v) : 'opt_meth_list)))) ]) ]))
                    ());
               Gram.extend (poly_type : 'poly_type Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (t : 'poly_type)))) ]) ]))
                    ());
               Gram.extend (package_type : 'package_type Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'module_type) (_loc : Gram.Loc.t) ->
                                  (p : 'package_type)))) ]) ]))
                    ());
               Gram.extend (typevars : 'typevars Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Skeyword "'";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyQuo (_loc, i) : 'typevars))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'typevars)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'typevars)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'typevars) (t1 : 'typevars)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyApp (_loc, t1, t2) : 'typevars)))) ]) ]))
                    ());
               Gram.extend
                 (unquoted_typevars : 'unquoted_typevars Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, (Some Camlp4.Sig.Grammar.LeftA),
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) (_loc : Gram.Loc.t) ->
                                  (Ast.TyId (_loc, (Ast.IdLid (_loc, i))) :
                                    'unquoted_typevars))));
                           ([ Gram.Stoken
                                (((function
                                   | QUOTATION _ -> true
                                   | _ -> false),
                                  "QUOTATION _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | QUOTATION x ->
                                      (Quotation.expand _loc x Quotation.
                                         DynAst.ctyp_tag :
                                        'unquoted_typevars)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'unquoted_typevars)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'unquoted_typevars)
                                  (t1 : 'unquoted_typevars)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyApp (_loc, t1, t2) :
                                    'unquoted_typevars)))) ]) ]))
                    ());
               Gram.extend (row_field : 'row_field Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (t : 'row_field))));
                           ([ Gram.Skeyword "`";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t));
                              Gram.Skeyword "of";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (amp_ctyp : 'amp_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'amp_ctyp) _ (i : 'a_ident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyOf (_loc, (Ast.TyVrn (_loc, i)), t) :
                                    'row_field))));
                           ([ Gram.Skeyword "`";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t));
                              Gram.Skeyword "of"; Gram.Skeyword "&";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (amp_ctyp : 'amp_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'amp_ctyp) _ _ (i : 'a_ident) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyOfAmp (_loc, (Ast.TyVrn (_loc, i)),
                                     t) :
                                    'row_field))));
                           ([ Gram.Skeyword "`";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrn (_loc, i) : 'row_field))));
                           ([ Gram.Sself; Gram.Skeyword "|"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'row_field) _ (t1 : 'row_field)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyOr (_loc, t1, t2) : 'row_field))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp|" n s)) :
                                        'row_field)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'row_field)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (amp_ctyp : 'amp_ctyp Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (t : 'amp_ctyp))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("list", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"list\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("list" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp&" n s)) :
                                        'amp_ctyp)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "&"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'amp_ctyp) _ (t1 : 'amp_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyAmp (_loc, t1, t2) : 'amp_ctyp)))) ]) ]))
                    ());
               Gram.extend (name_tags : 'name_tags Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "`";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrn (_loc, i) : 'name_tags))));
                           ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (t2 : 'name_tags) (t1 : 'name_tags)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyApp (_loc, t1, t2) : 'name_tags))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "typ"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"typ\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "typ" as n)), s) ->
                                      (Ast.TyAnt (_loc,
                                         (mk_anti ~c: "ctyp" n s)) :
                                        'name_tags)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (eq_expr : 'eq_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (fun i p -> Ast.PaOlb (_loc, i, p) :
                                    'eq_expr))));
                           ([ Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                  (fun i p -> Ast.PaOlbi (_loc, i, p, e) :
                                    'eq_expr)))) ]) ]))
                    ());
               Gram.extend (patt_tcon : 'patt_tcon Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'patt) (_loc : Gram.Loc.t) ->
                                  (p : 'patt_tcon))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (p : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyc (_loc, p, t) : 'patt_tcon)))) ]) ]))
                    ());
               Gram.extend (ipatt : 'ipatt Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "?"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (ipatt_tcon : 'ipatt_tcon Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (e : 'expr) _ (p : 'ipatt_tcon) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaOlbi (_loc, "", p, e) : 'ipatt))));
                           ([ Gram.Skeyword "?"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (ipatt_tcon : 'ipatt_tcon Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (p : 'ipatt_tcon) _ _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaOlb (_loc, "", p) : 'ipatt))));
                           ([ Gram.Skeyword "?";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (Ast.PaOlb (_loc, (mk_anti n i),
                                         (Ast.PaNil _loc)) :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "?";
                              Gram.Stoken
                                (((function | LIDENT _ -> true | _ -> false),
                                  "LIDENT _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LIDENT i ->
                                      (Ast.PaOlb (_loc, i, (Ast.PaNil _loc)) :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "?";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)"));
                              Gram.Skeyword ":"; Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (ipatt_tcon : 'ipatt_tcon Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (eq_expr : 'eq_expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (f : 'eq_expr) (p : 'ipatt_tcon) _ _
                                  (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (f (mk_anti n i) p : 'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function | OPTLABEL _ -> true | _ -> false),
                                  "OPTLABEL _"));
                              Gram.Skeyword "(";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (ipatt_tcon : 'ipatt_tcon Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (eq_expr : 'eq_expr Gram.Entry.t));
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (f : 'eq_expr) (p : 'ipatt_tcon) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | OPTLABEL i -> (f i p : 'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Stoken
                                (((function | LIDENT _ -> true | _ -> false),
                                  "LIDENT _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LIDENT i ->
                                      (Ast.PaLab (_loc, i, (Ast.PaNil _loc)) :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (Ast.PaLab (_loc, (mk_anti n i),
                                         (Ast.PaNil _loc)) :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)"));
                              Gram.Skeyword ":"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p : 'ipatt) _
                                  (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), i) ->
                                      (Ast.PaLab (_loc, (mk_anti n i), p) :
                                        'ipatt)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function | LABEL _ -> true | _ -> false),
                                  "LABEL _"));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (p : 'ipatt) (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LABEL i ->
                                      (Ast.PaLab (_loc, i, p) : 'ipatt)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (ipatt_tcon : 'ipatt_tcon Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (ipatt : 'ipatt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (p : 'ipatt) (_loc : Gram.Loc.t) ->
                                  (p : 'ipatt_tcon))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (ipatt : 'ipatt Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (t : 'ctyp) _ (p : 'ipatt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaTyc (_loc, p, t) : 'ipatt_tcon)))) ]) ]))
                    ());
               Gram.extend (direction_flag : 'direction_flag Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("to" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"to\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("to" | "anti" as n)), s) ->
                                      (Ast.DiAnt (mk_anti n s) :
                                        'direction_flag)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "downto" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.DiDownto : 'direction_flag))));
                           ([ Gram.Skeyword "to" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.DiTo : 'direction_flag)))) ]) ]))
                    ());
               Gram.extend (opt_private : 'opt_private Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.PrNil : 'opt_private))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("private" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"private\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("private" | "anti" as n)), s)
                                      ->
                                      (Ast.PrAnt (mk_anti n s) :
                                        'opt_private)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "private" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.PrPrivate : 'opt_private)))) ]) ]))
                    ());
               Gram.extend (opt_mutable : 'opt_mutable Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.MuNil : 'opt_mutable))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("mutable" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"mutable\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("mutable" | "anti" as n)), s)
                                      ->
                                      (Ast.MuAnt (mk_anti n s) :
                                        'opt_mutable)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "mutable" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.MuMutable : 'opt_mutable)))) ]) ]))
                    ());
               Gram.extend (opt_virtual : 'opt_virtual Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.ViNil : 'opt_virtual))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("virtual" | "anti"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"virtual\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("virtual" | "anti" as n)), s)
                                      ->
                                      (Ast.ViAnt (mk_anti n s) :
                                        'opt_virtual)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "virtual" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.ViVirtual : 'opt_virtual)))) ]) ]))
                    ());
               Gram.extend (opt_dot_dot : 'opt_dot_dot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.RvNil : 'opt_dot_dot))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ((".." | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"..\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT (((".." | "anti" as n)), s) ->
                                      (Ast.RvAnt (mk_anti n s) :
                                        'opt_dot_dot)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword ".." ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.RvRowVar : 'opt_dot_dot)))) ]) ]))
                    ());
               Gram.extend (opt_rec : 'opt_rec Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.ReNil : 'opt_rec))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("rec" | "anti"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"rec\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("rec" | "anti" as n)), s) ->
                                      (Ast.ReAnt (mk_anti n s) : 'opt_rec)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "rec" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.ReRecursive : 'opt_rec)))) ]) ]))
                    ());
               Gram.extend (opt_override : 'opt_override Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.OvNil : 'opt_override))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("!" | "override" | "anti"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"!\" | \"override\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("!" | "override" | "anti" as n)), s)
                                      ->
                                      (Ast.OvAnt (mk_anti n s) :
                                        'opt_override)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "!" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (Ast.OvOverride : 'opt_override)))) ]) ]))
                    ());
               Gram.extend (opt_expr : 'opt_expr Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.ExNil _loc : 'opt_expr))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                  (e : 'opt_expr)))) ]) ]))
                    ());
               Gram.extend (interf : 'interf Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | EOI -> true | _ -> false),
                                  "EOI")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | EOI -> (([], None) : 'interf)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (sig_item : 'sig_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun ((sil, stopped) : 'interf) _
                                  (si : 'sig_item) (_loc : Gram.Loc.t) ->
                                  (((si :: sil), stopped) : 'interf))));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_expr : 'opt_expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (dp : 'opt_expr) (n : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (([ Ast.SgDir (_loc, n, dp) ],
                                    (stopped_at _loc)) : 'interf)))) ]) ]))
                    ());
               Gram.extend (sig_items : 'sig_items Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Slist0
                                (Gram.srules sig_items
                                   [ ([ Gram.Snterm
                                          (Gram.Entry.obj
                                             (sig_item :
                                               'sig_item Gram.Entry.t));
                                        Gram.Snterm
                                          (Gram.Entry.obj
                                             (semi : 'semi Gram.Entry.t)) ],
                                      (Gram.Action.mk
                                         (fun _ (sg : 'sig_item)
                                            (_loc : Gram.Loc.t) ->
                                            (sg : 'e__12)))) ]) ],
                            (Gram.Action.mk
                               (fun (l : 'e__12 list) (_loc : Gram.Loc.t) ->
                                  (Ast.sgSem_of_list l : 'sig_items))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "sigi" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"sigi\" | \"anti\" | \"list\"), _)"));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (sg : 'sig_items) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "sigi" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.SgSem (_loc,
                                         (Ast.SgAnt (_loc,
                                            (mk_anti n ~c: "sig_item" s))),
                                         sg) :
                                        'sig_items)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "sigi" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"sigi\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "sigi" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.SgAnt (_loc,
                                         (mk_anti n ~c: "sig_item" s)) :
                                        'sig_items)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (implem : 'implem Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | EOI -> true | _ -> false),
                                  "EOI")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | EOI -> (([], None) : 'implem)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (str_item : 'str_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun ((sil, stopped) : 'implem) _
                                  (si : 'str_item) (_loc : Gram.Loc.t) ->
                                  (((si :: sil), stopped) : 'implem))));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_expr : 'opt_expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (dp : 'opt_expr) (n : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (([ Ast.StDir (_loc, n, dp) ],
                                    (stopped_at _loc)) : 'implem)))) ]) ]))
                    ());
               Gram.extend (str_items : 'str_items Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Slist0
                                (Gram.srules str_items
                                   [ ([ Gram.Snterm
                                          (Gram.Entry.obj
                                             (str_item :
                                               'str_item Gram.Entry.t));
                                        Gram.Snterm
                                          (Gram.Entry.obj
                                             (semi : 'semi Gram.Entry.t)) ],
                                      (Gram.Action.mk
                                         (fun _ (st : 'str_item)
                                            (_loc : Gram.Loc.t) ->
                                            (st : 'e__13)))) ]) ],
                            (Gram.Action.mk
                               (fun (l : 'e__13 list) (_loc : Gram.Loc.t) ->
                                  (Ast.stSem_of_list l : 'str_items))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "stri" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"stri\" | \"anti\" | \"list\"), _)"));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (st : 'str_items) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "stri" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.StSem (_loc,
                                         (Ast.StAnt (_loc,
                                            (mk_anti n ~c: "str_item" s))),
                                         st) :
                                        'str_items)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "stri" | "anti" | "list"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"stri\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "stri" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.StAnt (_loc,
                                         (mk_anti n ~c: "str_item" s)) :
                                        'str_items)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (top_phrase : 'top_phrase Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | EOI -> true | _ -> false),
                                  "EOI")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | EOI -> (None : 'top_phrase)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (phrase : 'phrase Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ph : 'phrase) (_loc : Gram.Loc.t) ->
                                  (Some ph : 'top_phrase)))) ]) ]))
                    ());
               Gram.extend (use_file : 'use_file Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | EOI -> true | _ -> false),
                                  "EOI")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | EOI -> (([], None) : 'use_file)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (str_item : 'str_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun ((sil, stopped) : 'use_file) _
                                  (si : 'str_item) (_loc : Gram.Loc.t) ->
                                  (((si :: sil), stopped) : 'use_file))));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_expr : 'opt_expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (dp : 'opt_expr) (n : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (([ Ast.StDir (_loc, n, dp) ],
                                    (stopped_at _loc)) : 'use_file)))) ]) ]))
                    ());
               Gram.extend (phrase : 'phrase Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (str_item : 'str_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (st : 'str_item) (_loc : Gram.Loc.t) ->
                                  (st : 'phrase))));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_expr : 'opt_expr Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun _ (dp : 'opt_expr) (n : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StDir (_loc, n, dp) : 'phrase)))) ]) ]))
                    ());
               Gram.extend (a_INT : 'a_INT Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | INT (_, _) -> true | _ -> false),
                                  "INT (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | INT (_, s) -> (s : 'a_INT)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "int" | "`int"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"int\" | \"`int\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "int" | "`int" as n)),
                                      s) -> (mk_anti n s : 'a_INT)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_INT32 : 'a_INT32 Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | INT32 (_, _) -> true
                                   | _ -> false),
                                  "INT32 (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | INT32 (_, s) -> (s : 'a_INT32)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "int32" | "`int32"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"int32\" | \"`int32\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "int32" | "`int32" as n)), s)
                                      -> (mk_anti n s : 'a_INT32)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_INT64 : 'a_INT64 Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | INT64 (_, _) -> true
                                   | _ -> false),
                                  "INT64 (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | INT64 (_, s) -> (s : 'a_INT64)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "int64" | "`int64"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"int64\" | \"`int64\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "int64" | "`int64" as n)), s)
                                      -> (mk_anti n s : 'a_INT64)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_NATIVEINT : 'a_NATIVEINT Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | NATIVEINT (_, _) -> true
                                   | _ -> false),
                                  "NATIVEINT (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | NATIVEINT (_, s) -> (s : 'a_NATIVEINT)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT
                                       (("" | "nativeint" | "`nativeint"), _)
                                       -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"nativeint\" | \"`nativeint\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "nativeint" | "`nativeint" as
                                         n)),
                                      s) -> (mk_anti n s : 'a_NATIVEINT)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_FLOAT : 'a_FLOAT Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | FLOAT (_, _) -> true
                                   | _ -> false),
                                  "FLOAT (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | FLOAT (_, s) -> (s : 'a_FLOAT)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "flo" | "`flo"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"flo\" | \"`flo\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "flo" | "`flo" as n)),
                                      s) -> (mk_anti n s : 'a_FLOAT)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_CHAR : 'a_CHAR Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | CHAR (_, _) -> true
                                   | _ -> false),
                                  "CHAR (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | CHAR (_, s) -> (s : 'a_CHAR)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "chr" | "`chr"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"chr\" | \"`chr\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "chr" | "`chr" as n)),
                                      s) -> (mk_anti n s : 'a_CHAR)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_UIDENT : 'a_UIDENT Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | UIDENT _ -> true | _ -> false),
                                  "UIDENT _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | UIDENT s -> (s : 'a_UIDENT)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "uid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"uid\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "uid" as n)), s) ->
                                      (mk_anti n s : 'a_UIDENT)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_LIDENT : 'a_LIDENT Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | LIDENT _ -> true | _ -> false),
                                  "LIDENT _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LIDENT s -> (s : 'a_LIDENT)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "lid"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"lid\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "lid" as n)), s) ->
                                      (mk_anti n s : 'a_LIDENT)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_LABEL : 'a_LABEL Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | LABEL _ -> true | _ -> false),
                                  "LABEL _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | LABEL s -> (s : 'a_LABEL)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "~";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT ("", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"\", _)"));
                              Gram.Skeyword ":" ],
                            (Gram.Action.mk
                               (fun _ (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" as n)), s) ->
                                      (mk_anti n s : 'a_LABEL)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_OPTLABEL : 'a_OPTLABEL Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function | OPTLABEL _ -> true | _ -> false),
                                  "OPTLABEL _")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | OPTLABEL s -> (s : 'a_OPTLABEL)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "?";
                              Gram.Stoken
                                (((function
                                   | ANTIQUOT ("", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"\", _)"));
                              Gram.Skeyword ":" ],
                            (Gram.Action.mk
                               (fun _ (__camlp4_0 : Gram.Token.t) _
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" as n)), s) ->
                                      (mk_anti n s : 'a_OPTLABEL)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (a_STRING : 'a_STRING Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | STRING (_, _) -> true
                                   | _ -> false),
                                  "STRING (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | STRING (_, s) -> (s : 'a_STRING)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "str" | "`str"), _) ->
                                       true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"str\" | \"`str\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" | "str" | "`str" as n)),
                                      s) -> (mk_anti n s : 'a_STRING)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (string_list : 'string_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Stoken
                                (((function
                                   | STRING (_, _) -> true
                                   | _ -> false),
                                  "STRING (_, _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | STRING (_, x) ->
                                      (Ast.LCons (x, Ast.LNil) :
                                        'string_list)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | STRING (_, _) -> true
                                   | _ -> false),
                                  "STRING (_, _)"));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (xs : 'string_list)
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | STRING (_, x) ->
                                      (Ast.LCons (x, xs) : 'string_list)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "str_list"), _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"str_list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT (("" | "str_list"), s) ->
                                      (Ast.LAnt (mk_anti "str_list" s) :
                                        'string_list)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (value_let : 'value_let Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "value" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (() : 'value_let)))) ]) ]))
                    ());
               Gram.extend (value_val : 'value_val Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword "value" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) ->
                                  (() : 'value_val)))) ]) ]))
                    ());
               Gram.extend (semi : 'semi Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Skeyword ";" ],
                            (Gram.Action.mk
                               (fun _ (_loc : Gram.Loc.t) -> (() : 'semi)))) ]) ]))
                    ());
               Gram.extend (expr_quot : 'expr_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.ExNil _loc : 'expr_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                  (e : 'expr_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_expr : 'sem_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e2 : 'sem_expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExSem (_loc, e1, e2) : 'expr_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Skeyword ",";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_expr : 'comma_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (e2 : 'comma_expr) _ (e1 : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExCom (_loc, e1, e2) : 'expr_quot)))) ]) ]))
                    ());
               Gram.extend (patt_quot : 'patt_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.PaNil _loc : 'patt_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'patt) (_loc : Gram.Loc.t) ->
                                  (x : 'patt_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'patt) _ (x : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (let i =
                                     match x with
                                     | Ast.PaAnt (loc, s) ->
                                         Ast.IdAnt (loc, s)
                                     | p -> Ast.ident_of_patt p
                                   in Ast.PaEq (_loc, i, y) : 'patt_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_patt : 'sem_patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'sem_patt) _ (x : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaSem (_loc, x, y) : 'patt_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Skeyword ",";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_patt : 'comma_patt Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'comma_patt) _ (x : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.PaCom (_loc, x, y) : 'patt_quot)))) ]) ]))
                    ());
               Gram.extend (ctyp_quot : 'ctyp_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.TyNil _loc : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'more_ctyp) (_loc : Gram.Loc.t) ->
                                  (x : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "and";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_arg_list :
                                     'constructor_arg_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'constructor_arg_list) _
                                  (x : 'more_ctyp) (_loc : Gram.Loc.t) ->
                                  (Ast.TyAnd (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "&";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (amp_ctyp : 'amp_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'amp_ctyp) _ (x : 'more_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyAmp (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "*";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (star_ctyp : 'star_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'star_ctyp) _ (x : 'more_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TySta (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (label_declaration_list :
                                     'label_declaration_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (z : 'label_declaration_list) _
                                  (y : 'more_ctyp) _ (x : 'more_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TySem (_loc, (Ast.TyCol (_loc, x, y)),
                                     z) :
                                    'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'more_ctyp) _ (x : 'more_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCol (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "of"; Gram.Skeyword "&";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (amp_ctyp : 'amp_ctyp Gram.Entry.t));
                              Gram.Skeyword "|";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (row_field : 'row_field Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (z : 'row_field) _ (y : 'amp_ctyp) _ _
                                  (x : 'more_ctyp) (_loc : Gram.Loc.t) ->
                                  (Ast.TyOr (_loc,
                                     (Ast.TyOfAmp (_loc, x, y)), z) :
                                    'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "of"; Gram.Skeyword "&";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (amp_ctyp : 'amp_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'amp_ctyp) _ _ (x : 'more_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyOfAmp (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "of";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_arg_list :
                                     'constructor_arg_list Gram.Entry.t));
                              Gram.Skeyword "|";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_declarations :
                                     'constructor_declarations Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (z : 'constructor_declarations) _
                                  (y : 'constructor_arg_list) _
                                  (x : 'more_ctyp) (_loc : Gram.Loc.t) ->
                                  (Ast.TyOr (_loc, (Ast.TyOf (_loc, x, y)),
                                     z) :
                                    'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "of";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_arg_list :
                                     'constructor_arg_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'constructor_arg_list) _
                                  (x : 'more_ctyp) (_loc : Gram.Loc.t) ->
                                  (Ast.TyOf (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword "|";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (constructor_declarations :
                                     'constructor_declarations Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'constructor_declarations) _
                                  (x : 'more_ctyp) (_loc : Gram.Loc.t) ->
                                  (Ast.TyOr (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (label_declaration_list :
                                     'label_declaration_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'label_declaration_list) _
                                  (x : 'more_ctyp) (_loc : Gram.Loc.t) ->
                                  (Ast.TySem (_loc, x, y) : 'ctyp_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (more_ctyp : 'more_ctyp Gram.Entry.t));
                              Gram.Skeyword ",";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (comma_ctyp : 'comma_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (y : 'comma_ctyp) _ (x : 'more_ctyp)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.TyCom (_loc, x, y) : 'ctyp_quot)))) ]) ]))
                    ());
               Gram.extend (more_ctyp : 'more_ctyp Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (type_parameter :
                                     'type_parameter Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'type_parameter) (_loc : Gram.Loc.t)
                                  -> (x : 'more_ctyp))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj (ctyp : 'ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'ctyp) (_loc : Gram.Loc.t) ->
                                  (x : 'more_ctyp))));
                           ([ Gram.Skeyword "`";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_ident : 'a_ident Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyVrn (_loc, x) : 'more_ctyp))));
                           ([ Gram.Skeyword "mutable"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (x : 'more_ctyp) _ (_loc : Gram.Loc.t) ->
                                  (Ast.TyMut (_loc, x) : 'more_ctyp)))) ]) ]))
                    ());
               Gram.extend (str_item_quot : 'str_item_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.StNil _loc : 'str_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (str_item : 'str_item Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (st : 'str_item) (_loc : Gram.Loc.t) ->
                                  (st : 'str_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (str_item : 'str_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (st2 : 'str_item_quot) _
                                  (st1 : 'str_item) (_loc : Gram.Loc.t) ->
                                  (match st2 with
                                   | Ast.StNil _ -> st1
                                   | _ -> Ast.StSem (_loc, st1, st2) :
                                    'str_item_quot))));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_expr : 'opt_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (dp : 'opt_expr) (n : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.StDir (_loc, n, dp) : 'str_item_quot)))) ]) ]))
                    ());
               Gram.extend (sig_item_quot : 'sig_item_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.SgNil _loc : 'sig_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (sig_item : 'sig_item Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (sg : 'sig_item) (_loc : Gram.Loc.t) ->
                                  (sg : 'sig_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (sig_item : 'sig_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (sg2 : 'sig_item_quot) _
                                  (sg1 : 'sig_item) (_loc : Gram.Loc.t) ->
                                  (match sg2 with
                                   | Ast.SgNil _ -> sg1
                                   | _ -> Ast.SgSem (_loc, sg1, sg2) :
                                    'sig_item_quot))));
                           ([ Gram.Skeyword "#";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_expr : 'opt_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (dp : 'opt_expr) (n : 'a_LIDENT) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.SgDir (_loc, n, dp) : 'sig_item_quot)))) ]) ]))
                    ());
               Gram.extend
                 (module_type_quot : 'module_type_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.MtNil _loc : 'module_type_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'module_type) (_loc : Gram.Loc.t) ->
                                  (x : 'module_type_quot)))) ]) ]))
                    ());
               Gram.extend
                 (module_expr_quot : 'module_expr_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.MeNil _loc : 'module_expr_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'module_expr) (_loc : Gram.Loc.t) ->
                                  (x : 'module_expr_quot)))) ]) ]))
                    ());
               Gram.extend (match_case_quot : 'match_case_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.McNil _loc : 'match_case_quot))));
                           ([ Gram.Slist0sep
                                ((Gram.Snterm
                                    (Gram.Entry.obj
                                       (match_case0 :
                                         'match_case0 Gram.Entry.t))),
                                (Gram.Skeyword "|")) ],
                            (Gram.Action.mk
                               (fun (x : 'match_case0 list)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.mcOr_of_list x : 'match_case_quot)))) ]) ]))
                    ());
               Gram.extend (binding_quot : 'binding_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.BiNil _loc : 'binding_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (binding : 'binding Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'binding) (_loc : Gram.Loc.t) ->
                                  (x : 'binding_quot)))) ]) ]))
                    ());
               Gram.extend
                 (rec_binding_quot : 'rec_binding_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.RbNil _loc : 'rec_binding_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (label_expr_list :
                                     'label_expr_list Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'label_expr_list)
                                  (_loc : Gram.Loc.t) ->
                                  (x : 'rec_binding_quot)))) ]) ]))
                    ());
               Gram.extend
                 (module_binding_quot : 'module_binding_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.MbNil _loc : 'module_binding_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ (mt : 'module_type)
                                  _ (m : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.MbColEq (_loc, m, mt, me) :
                                    'module_binding_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _ (m : 'a_UIDENT)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MbCol (_loc, m, mt) :
                                    'module_binding_quot))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"\", _)"));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t));
                              Gram.Skeyword "=";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_expr : 'module_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (me : 'module_expr) _ (mt : 'module_type)
                                  _ (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" as n)), m) ->
                                      (Ast.MbColEq (_loc, (mk_anti n m), mt,
                                         me) :
                                        'module_binding_quot)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"\", _)"));
                              Gram.Skeyword ":";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (module_type : 'module_type Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (mt : 'module_type) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" as n)), m) ->
                                      (Ast.MbCol (_loc, (mk_anti n m), mt) :
                                        'module_binding_quot)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"\", _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("" as n)), s) ->
                                      (Ast.MbAnt (_loc,
                                         (mk_anti ~c: "module_binding" n s)) :
                                        'module_binding_quot)
                                  | _ -> assert false)));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("module_binding" | "anti"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"module_binding\" | \"anti\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("module_binding" | "anti" as n)), s)
                                      ->
                                      (Ast.MbAnt (_loc,
                                         (mk_anti ~c: "module_binding" n s)) :
                                        'module_binding_quot)
                                  | _ -> assert false)));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (b2 : 'module_binding_quot) _
                                  (b1 : 'module_binding_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.MbAnd (_loc, b1, b2) :
                                    'module_binding_quot)))) ]) ]))
                    ());
               Gram.extend (ident_quot : 'ident_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ ((Some "apply"), None,
                         [ ([ Gram.Sself; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (j : 'ident_quot) (i : 'ident_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.IdApp (_loc, i, j) : 'ident_quot)))) ]);
                        ((Some "."), None,
                         [ ([ Gram.Sself; Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (j : 'ident_quot) _ (i : 'ident_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.IdAcc (_loc, i, j) : 'ident_quot)))) ]);
                        ((Some "simple"), None,
                         [ ([ Gram.Skeyword "("; Gram.Sself;
                              Gram.Skeyword ")" ],
                            (Gram.Action.mk
                               (fun _ (i : 'ident_quot) _ (_loc : Gram.Loc.t)
                                  -> (i : 'ident_quot))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)"));
                              Gram.Skeyword "."; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (i : 'ident_quot) _
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAcc (_loc,
                                         (Ast.IdAnt (_loc,
                                            (mk_anti ~c: "ident" n s))),
                                         i) :
                                        'ident_quot)
                                  | _ -> assert false)));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdLid (_loc, i) : 'ident_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (a_UIDENT : 'a_UIDENT Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (i : 'a_UIDENT) (_loc : Gram.Loc.t) ->
                                  (Ast.IdUid (_loc, i) : 'ident_quot))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT (("" | "id" | "anti" | "list"),
                                       _) -> true
                                   | _ -> false),
                                  "ANTIQUOT ((\"\" | \"id\" | \"anti\" | \"list\"), _)")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT
                                      ((("" | "id" | "anti" | "list" as n)),
                                      s) ->
                                      (Ast.IdAnt (_loc,
                                         (mk_anti ~c: "ident" n s)) :
                                        'ident_quot)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (class_expr_quot : 'class_expr_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.CeNil _loc : 'class_expr_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_expr : 'class_expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'class_expr) (_loc : Gram.Loc.t) ->
                                  (x : 'class_expr_quot))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("virtual", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"virtual\", _)"));
                              Gram.Snterm
                                (Gram.Entry.obj (ident : 'ident Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_comma_ctyp :
                                     'opt_comma_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ot : 'opt_comma_ctyp) (i : 'ident)
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("virtual" as n)), s) ->
                                      (let anti =
                                         Ast.ViAnt
                                           (mk_anti ~c: "class_expr" n s)
                                       in Ast.CeCon (_loc, anti, i, ot) :
                                        'class_expr_quot)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_name_and_param :
                                     'class_name_and_param Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun ((i, ot) : 'class_name_and_param) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeCon (_loc, Ast.ViVirtual,
                                     (Ast.IdLid (_loc, i)), ot) :
                                    'class_expr_quot))));
                           ([ Gram.Sself; Gram.Skeyword "="; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (ce2 : 'class_expr_quot) _
                                  (ce1 : 'class_expr_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeEq (_loc, ce1, ce2) :
                                    'class_expr_quot))));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (ce2 : 'class_expr_quot) _
                                  (ce1 : 'class_expr_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CeAnd (_loc, ce1, ce2) :
                                    'class_expr_quot)))) ]) ]))
                    ());
               Gram.extend (class_type_quot : 'class_type_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.CtNil _loc : 'class_type_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_type_plus :
                                     'class_type_plus Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'class_type_plus)
                                  (_loc : Gram.Loc.t) ->
                                  (x : 'class_type_quot))));
                           ([ Gram.Stoken
                                (((function
                                   | ANTIQUOT ("virtual", _) -> true
                                   | _ -> false),
                                  "ANTIQUOT (\"virtual\", _)"));
                              Gram.Snterm
                                (Gram.Entry.obj (ident : 'ident Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_comma_ctyp :
                                     'opt_comma_ctyp Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (ot : 'opt_comma_ctyp) (i : 'ident)
                                  (__camlp4_0 : Gram.Token.t)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | ANTIQUOT ((("virtual" as n)), s) ->
                                      (let anti =
                                         Ast.ViAnt
                                           (mk_anti ~c: "class_type" n s)
                                       in Ast.CtCon (_loc, anti, i, ot) :
                                        'class_type_quot)
                                  | _ -> assert false)));
                           ([ Gram.Skeyword "virtual";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (class_name_and_param :
                                     'class_name_and_param Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun ((i, ot) : 'class_name_and_param) _
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtCon (_loc, Ast.ViVirtual,
                                     (Ast.IdLid (_loc, i)), ot) :
                                    'class_type_quot))));
                           ([ Gram.Sself; Gram.Skeyword ":"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (ct2 : 'class_type_quot) _
                                  (ct1 : 'class_type_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtCol (_loc, ct1, ct2) :
                                    'class_type_quot))));
                           ([ Gram.Sself; Gram.Skeyword "="; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (ct2 : 'class_type_quot) _
                                  (ct1 : 'class_type_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtEq (_loc, ct1, ct2) :
                                    'class_type_quot))));
                           ([ Gram.Sself; Gram.Skeyword "and"; Gram.Sself ],
                            (Gram.Action.mk
                               (fun (ct2 : 'class_type_quot) _
                                  (ct1 : 'class_type_quot)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.CtAnd (_loc, ct1, ct2) :
                                    'class_type_quot)))) ]) ]))
                    ());
               Gram.extend
                 (class_str_item_quot : 'class_str_item_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.CrNil _loc : 'class_str_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_str_item :
                                     'class_str_item Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'class_str_item) (_loc : Gram.Loc.t)
                                  -> (x : 'class_str_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_str_item :
                                     'class_str_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (x2 : 'class_str_item_quot) _
                                  (x1 : 'class_str_item) (_loc : Gram.Loc.t)
                                  ->
                                  (match x2 with
                                   | Ast.CrNil _ -> x1
                                   | _ -> Ast.CrSem (_loc, x1, x2) :
                                    'class_str_item_quot)))) ]) ]))
                    ());
               Gram.extend
                 (class_sig_item_quot : 'class_sig_item_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.CgNil _loc : 'class_sig_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_sig_item :
                                     'class_sig_item Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'class_sig_item) (_loc : Gram.Loc.t)
                                  -> (x : 'class_sig_item_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (class_sig_item :
                                     'class_sig_item Gram.Entry.t));
                              Gram.Snterm
                                (Gram.Entry.obj (semi : 'semi Gram.Entry.t));
                              Gram.Sself ],
                            (Gram.Action.mk
                               (fun (x2 : 'class_sig_item_quot) _
                                  (x1 : 'class_sig_item) (_loc : Gram.Loc.t)
                                  ->
                                  (match x2 with
                                   | Ast.CgNil _ -> x1
                                   | _ -> Ast.CgSem (_loc, x1, x2) :
                                    'class_sig_item_quot)))) ]) ]))
                    ());
               Gram.extend
                 (with_constr_quot : 'with_constr_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([],
                            (Gram.Action.mk
                               (fun (_loc : Gram.Loc.t) ->
                                  (Ast.WcNil _loc : 'with_constr_quot))));
                           ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (with_constr : 'with_constr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'with_constr) (_loc : Gram.Loc.t) ->
                                  (x : 'with_constr_quot)))) ]) ]))
                    ());
               Gram.extend (rec_flag_quot : 'rec_flag_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_rec : 'opt_rec Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'opt_rec) (_loc : Gram.Loc.t) ->
                                  (x : 'rec_flag_quot)))) ]) ]))
                    ());
               Gram.extend
                 (direction_flag_quot : 'direction_flag_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (direction_flag :
                                     'direction_flag Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'direction_flag) (_loc : Gram.Loc.t)
                                  -> (x : 'direction_flag_quot)))) ]) ]))
                    ());
               Gram.extend
                 (mutable_flag_quot : 'mutable_flag_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_mutable : 'opt_mutable Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'opt_mutable) (_loc : Gram.Loc.t) ->
                                  (x : 'mutable_flag_quot)))) ]) ]))
                    ());
               Gram.extend
                 (private_flag_quot : 'private_flag_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_private : 'opt_private Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'opt_private) (_loc : Gram.Loc.t) ->
                                  (x : 'private_flag_quot)))) ]) ]))
                    ());
               Gram.extend
                 (virtual_flag_quot : 'virtual_flag_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_virtual : 'opt_virtual Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'opt_virtual) (_loc : Gram.Loc.t) ->
                                  (x : 'virtual_flag_quot)))) ]) ]))
                    ());
               Gram.extend
                 (row_var_flag_quot : 'row_var_flag_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_dot_dot : 'opt_dot_dot Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'opt_dot_dot) (_loc : Gram.Loc.t) ->
                                  (x : 'row_var_flag_quot)))) ]) ]))
                    ());
               Gram.extend
                 (override_flag_quot : 'override_flag_quot Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj
                                   (opt_override :
                                     'opt_override Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (x : 'opt_override) (_loc : Gram.Loc.t)
                                  -> (x : 'override_flag_quot)))) ]) ]))
                    ());
               Gram.extend (patt_eoi : 'patt_eoi Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                              Gram.Stoken
                                (((function | EOI -> true | _ -> false),
                                  "EOI")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) (x : 'patt)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | EOI -> (x : 'patt_eoi)
                                  | _ -> assert false))) ]) ]))
                    ());
               Gram.extend (expr_eoi : 'expr_eoi Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                              Gram.Stoken
                                (((function | EOI -> true | _ -> false),
                                  "EOI")) ],
                            (Gram.Action.mk
                               (fun (__camlp4_0 : Gram.Token.t) (x : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  match __camlp4_0 with
                                  | EOI -> (x : 'expr_eoi)
                                  | _ -> assert false))) ]) ]))
                    ()))
          in apply ()
          
      end
      
    let _ = let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
      
  end
  
module Camlp4QuotationCommon =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Nicolas Pouillard: initial version
 *)
    module Id =
      struct
        let name = "Camlp4QuotationCommon"
          
        let version = Sys.ocaml_version
          
      end
      
    module Make
      (Syntax : Sig.Camlp4Syntax)
      (TheAntiquotSyntax : Sig.Parser(Syntax.Ast).SIMPLE) =
      struct
        open Sig
          
        include Syntax
          
        (* Be careful an AntiquotSyntax module appears here *)
        module MetaLocHere = Ast.Meta.MetaLoc
          
        module MetaLoc =
          struct
            module Ast = Ast
              
            let loc_name = ref None
              
            let meta_loc_expr _loc loc =
              match !loc_name with
              | None -> Ast.ExId (_loc, (Ast.IdLid (_loc, !Loc.name)))
              | Some "here" -> MetaLocHere.meta_loc_expr _loc loc
              | Some x -> Ast.ExId (_loc, (Ast.IdLid (_loc, x)))
              
            let meta_loc_patt _loc _ = Ast.PaAny _loc
              
          end
          
        module MetaAst = Ast.Meta.Make(MetaLoc)
          
        module ME = MetaAst.Expr
          
        module MP = MetaAst.Patt
          
        let is_antiquot s =
          let len = String.length s
          in (len > 2) && ((s.[0] = '\\') && (s.[1] = '$'))
          
        let handle_antiquot_in_string s term parse loc decorate =
          if is_antiquot s
          then
            (let pos = String.index s ':' in
             let name = String.sub s 2 (pos - 2)
             and code =
               String.sub s (pos + 1) (((String.length s) - pos) - 1)
             in decorate name (parse loc code))
          else term
          
        let antiquot_expander =
          object
            inherit Ast.map as super
            method patt =
              function
              | (Ast.PaAnt (_loc, s) | Ast.PaStr (_loc, s) as p) ->
                  let mloc _loc = MetaLoc.meta_loc_patt _loc _loc
                  in
                    handle_antiquot_in_string s p TheAntiquotSyntax.
                      parse_patt _loc
                      (fun n p ->
                         match n with
                         | "antisig_item" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "SgAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antistr_item" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "StAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antictyp" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "TyAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antipatt" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "PaAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antiexpr" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "ExAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antimodule_type" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "MtAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antimodule_expr" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "MeAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "anticlass_type" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CtAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "anticlass_expr" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CeAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "anticlass_sig_item" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CgAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "anticlass_str_item" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CrAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antiwith_constr" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "WcAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antibinding" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "BiAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antirec_binding" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "RbAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antimatch_case" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "McAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antimodule_binding" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "MbAnt")))))),
                                  (mloc _loc))),
                               p)
                         | "antiident" ->
                             Ast.PaApp (_loc,
                               (Ast.PaApp (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "IdAnt")))))),
                                  (mloc _loc))),
                               p)
                         | _ -> p)
              | p -> super#patt p
            method expr =
              function
              | (Ast.ExAnt (_loc, s) | Ast.ExStr (_loc, s) as e) ->
                  let mloc _loc = MetaLoc.meta_loc_expr _loc _loc
                  in
                    handle_antiquot_in_string s e TheAntiquotSyntax.
                      parse_expr _loc
                      (fun n e ->
                         match n with
                         | "`int" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdLid (_loc, "string_of_int")))),
                               e)
                         | "`int32" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Int32")),
                                     (Ast.IdLid (_loc, "to_string")))))),
                               e)
                         | "`int64" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Int64")),
                                     (Ast.IdLid (_loc, "to_string")))))),
                               e)
                         | "`nativeint" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Nativeint")),
                                     (Ast.IdLid (_loc, "to_string")))))),
                               e)
                         | "`flo" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Camlp4_import")),
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Oprint")),
                                        (Ast.IdLid (_loc, "float_repres")))))))),
                               e)
                         | "`str" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "safe_string_escaped")))))),
                               e)
                         | "`chr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Char")),
                                     (Ast.IdLid (_loc, "escaped")))))),
                               e)
                         | "`bool" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "IdUid")))))),
                                  (mloc _loc))),
                               (Ast.ExIfe (_loc, e,
                                  (Ast.ExStr (_loc, "True")),
                                  (Ast.ExStr (_loc, "False")))))
                         | "liststr_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "stSem_of_list")))))),
                               e)
                         | "listsig_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "sgSem_of_list")))))),
                               e)
                         | "listclass_sig_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "cgSem_of_list")))))),
                               e)
                         | "listclass_str_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "crSem_of_list")))))),
                               e)
                         | "listmodule_expr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "meApp_of_list")))))),
                               e)
                         | "listmodule_type" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "mtApp_of_list")))))),
                               e)
                         | "listmodule_binding" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "mbAnd_of_list")))))),
                               e)
                         | "listbinding" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "biAnd_of_list")))))),
                               e)
                         | "listbinding;" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "biSem_of_list")))))),
                               e)
                         | "listrec_binding" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "rbSem_of_list")))))),
                               e)
                         | "listclass_type" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "ctAnd_of_list")))))),
                               e)
                         | "listclass_expr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "ceAnd_of_list")))))),
                               e)
                         | "listident" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "idAcc_of_list")))))),
                               e)
                         | "listctypand" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "tyAnd_of_list")))))),
                               e)
                         | "listctyp;" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "tySem_of_list")))))),
                               e)
                         | "listctyp*" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "tySta_of_list")))))),
                               e)
                         | "listctyp|" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "tyOr_of_list")))))),
                               e)
                         | "listctyp," ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "tyCom_of_list")))))),
                               e)
                         | "listctyp&" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "tyAmp_of_list")))))),
                               e)
                         | "listwith_constr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "wcAnd_of_list")))))),
                               e)
                         | "listmatch_case" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "mcOr_of_list")))))),
                               e)
                         | "listpatt," ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "paCom_of_list")))))),
                               e)
                         | "listpatt;" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "paSem_of_list")))))),
                               e)
                         | "listexpr," ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "exCom_of_list")))))),
                               e)
                         | "listexpr;" ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Ast")),
                                     (Ast.IdLid (_loc, "exSem_of_list")))))),
                               e)
                         | "antisig_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "SgAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antistr_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "StAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antictyp" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "TyAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antipatt" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "PaAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antiexpr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "ExAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antimodule_type" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "MtAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antimodule_expr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "MeAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "anticlass_type" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CtAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "anticlass_expr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CeAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "anticlass_sig_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CgAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "anticlass_str_item" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "CrAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antiwith_constr" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "WcAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antibinding" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "BiAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antirec_binding" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "RbAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antimatch_case" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "McAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antimodule_binding" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "MbAnt")))))),
                                  (mloc _loc))),
                               e)
                         | "antiident" ->
                             Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Ast")),
                                        (Ast.IdUid (_loc, "IdAnt")))))),
                                  (mloc _loc))),
                               e)
                         | _ -> e)
              | e -> super#expr e
          end
          
        let add_quotation name entry mexpr mpatt =
          let entry_eoi = Gram.Entry.mk (Gram.Entry.name entry) in
          let parse_quot_string entry loc s =
            let q = !Camlp4_config.antiquotations in
            let () = Camlp4_config.antiquotations := true in
            let res = Gram.parse_string entry loc s in
            let () = Camlp4_config.antiquotations := q in res in
          let expand_expr loc loc_name_opt s =
            let ast = parse_quot_string entry_eoi loc s in
            let () = MetaLoc.loc_name := loc_name_opt in
            let meta_ast = mexpr loc ast in
            let exp_ast = antiquot_expander#expr meta_ast in exp_ast in
          let expand_str_item loc loc_name_opt s =
            let exp_ast = expand_expr loc loc_name_opt s
            in Ast.StExp (loc, exp_ast) in
          let expand_patt _loc loc_name_opt s =
            let ast = parse_quot_string entry_eoi _loc s in
            let meta_ast = mpatt _loc ast in
            let exp_ast = antiquot_expander#patt meta_ast
            in
              match loc_name_opt with
              | None -> exp_ast
              | Some name ->
                  let rec subst_first_loc =
                    (function
                     | Ast.PaApp (_loc,
                         (Ast.PaId (_,
                            (Ast.IdAcc (_, (Ast.IdUid (_, "Ast")),
                               (Ast.IdUid (_, u)))))),
                         _) ->
                         Ast.PaApp (_loc,
                           (Ast.PaId (_loc,
                              (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Ast")),
                                 (Ast.IdUid (_loc, u)))))),
                           (Ast.PaId (_loc, (Ast.IdLid (_loc, name)))))
                     | Ast.PaApp (_loc, a, b) ->
                         Ast.PaApp (_loc, (subst_first_loc a), b)
                     | p -> p)
                  in subst_first_loc exp_ast
          in
            (Gram.extend (entry_eoi : 'entry_eoi Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (entry : 'entry Gram.Entry.t));
                            Gram.Stoken
                              (((function | EOI -> true | _ -> false), "EOI")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t) (x : 'entry)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | EOI -> (x : 'entry_eoi)
                                | _ -> assert false))) ]) ]))
                  ());
             Quotation.add name Quotation.DynAst.expr_tag expand_expr;
             Quotation.add name Quotation.DynAst.patt_tag expand_patt;
             Quotation.add name Quotation.DynAst.str_item_tag expand_str_item)
          
        let _ =
          add_quotation "sig_item" sig_item_quot ME.meta_sig_item MP.
            meta_sig_item
          
        let _ =
          add_quotation "str_item" str_item_quot ME.meta_str_item MP.
            meta_str_item
          
        let _ = add_quotation "ctyp" ctyp_quot ME.meta_ctyp MP.meta_ctyp
          
        let _ = add_quotation "patt" patt_quot ME.meta_patt MP.meta_patt
          
        let _ = add_quotation "expr" expr_quot ME.meta_expr MP.meta_expr
          
        let _ =
          add_quotation "module_type" module_type_quot ME.meta_module_type
            MP.meta_module_type
          
        let _ =
          add_quotation "module_expr" module_expr_quot ME.meta_module_expr
            MP.meta_module_expr
          
        let _ =
          add_quotation "class_type" class_type_quot ME.meta_class_type MP.
            meta_class_type
          
        let _ =
          add_quotation "class_expr" class_expr_quot ME.meta_class_expr MP.
            meta_class_expr
          
        let _ =
          add_quotation "class_sig_item" class_sig_item_quot ME.
            meta_class_sig_item MP.meta_class_sig_item
          
        let _ =
          add_quotation "class_str_item" class_str_item_quot ME.
            meta_class_str_item MP.meta_class_str_item
          
        let _ =
          add_quotation "with_constr" with_constr_quot ME.meta_with_constr
            MP.meta_with_constr
          
        let _ =
          add_quotation "binding" binding_quot ME.meta_binding MP.
            meta_binding
          
        let _ =
          add_quotation "rec_binding" rec_binding_quot ME.meta_rec_binding
            MP.meta_rec_binding
          
        let _ =
          add_quotation "match_case" match_case_quot ME.meta_match_case MP.
            meta_match_case
          
        let _ =
          add_quotation "module_binding" module_binding_quot ME.
            meta_module_binding MP.meta_module_binding
          
        let _ = add_quotation "ident" ident_quot ME.meta_ident MP.meta_ident
          
        let _ =
          add_quotation "rec_flag" rec_flag_quot ME.meta_rec_flag MP.
            meta_rec_flag
          
        let _ =
          add_quotation "private_flag" private_flag_quot ME.meta_private_flag
            MP.meta_private_flag
          
        let _ =
          add_quotation "row_var_flag" row_var_flag_quot ME.meta_row_var_flag
            MP.meta_row_var_flag
          
        let _ =
          add_quotation "mutable_flag" mutable_flag_quot ME.meta_mutable_flag
            MP.meta_mutable_flag
          
        let _ =
          add_quotation "virtual_flag" virtual_flag_quot ME.meta_virtual_flag
            MP.meta_virtual_flag
          
        let _ =
          add_quotation "override_flag" override_flag_quot ME.
            meta_override_flag MP.meta_override_flag
          
        let _ =
          add_quotation "direction_flag" direction_flag_quot ME.
            meta_direction_flag MP.meta_direction_flag
          
      end
      
  end
  
module Q =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
    module Id =
      struct
        let name = "Camlp4QuotationExpander"
          
        let version = Sys.ocaml_version
          
      end
      
    module Make (Syntax : Sig.Camlp4Syntax) =
      struct
        module M = Camlp4QuotationCommon.Make(Syntax)(Syntax.AntiquotSyntax)
          
        include M
          
      end
      
    let _ = let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
      
  end
  
module Rp =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright 1998-2006 Institut National de Recherche en Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
    module Id : Sig.Id =
      struct
        let name = "Camlp4OCamlRevisedParserParser"
          
        let version = Sys.ocaml_version
          
      end
      
    module Make (Syntax : Sig.Camlp4Syntax) =
      struct
        open Sig
          
        include Syntax
          
        type spat_comp =
          | SpTrm of Loc.t * Ast.patt * Ast.expr option
          | SpNtr of Loc.t * Ast.patt * Ast.expr
          | SpStr of Loc.t * Ast.patt
        
        type sexp_comp =
          | SeTrm of Loc.t * Ast.expr | SeNtr of Loc.t * Ast.expr
        
        let stream_expr = Gram.Entry.mk "stream_expr"
          
        let stream_begin = Gram.Entry.mk "stream_begin"
          
        let stream_end = Gram.Entry.mk "stream_end"
          
        let stream_quot = Gram.Entry.mk "stream_quot"
          
        let parser_case = Gram.Entry.mk "parser_case"
          
        let parser_case_list = Gram.Entry.mk "parser_case_list"
          
        let strm_n = "__strm"
          
        let peek_fun _loc =
          Ast.ExId (_loc,
            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
               (Ast.IdLid (_loc, "peek")))))
          
        let junk_fun _loc =
          Ast.ExId (_loc,
            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
               (Ast.IdLid (_loc, "junk")))))
          
        (* Parsers. *)
        (* In syntax generated, many cases are optimisations. *)
        let rec pattern_eq_expression p e =
          match (p, e) with
          | (Ast.PaId (_, (Ast.IdLid (_, a))),
             Ast.ExId (_, (Ast.IdLid (_, b)))) -> a = b
          | (Ast.PaId (_, (Ast.IdUid (_, a))),
             Ast.ExId (_, (Ast.IdUid (_, b)))) -> a = b
          | (Ast.PaApp (_, p1, p2), Ast.ExApp (_, e1, e2)) ->
              (pattern_eq_expression p1 e1) && (pattern_eq_expression p2 e2)
          | _ -> false
          
        let is_raise e =
          match e with
          | Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "raise")))), _) ->
              true
          | _ -> false
          
        let is_raise_failure e =
          match e with
          | Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "raise")))),
              (Ast.ExId (_,
                 (Ast.IdAcc (_, (Ast.IdUid (_, "Stream")),
                    (Ast.IdUid (_, "Failure")))))))
              -> true
          | _ -> false
          
        let rec handle_failure e =
          match e with
          | Ast.ExTry (_, _,
              (Ast.McArr (_,
                 (Ast.PaId (_,
                    (Ast.IdAcc (_, (Ast.IdUid (_, "Stream")),
                       (Ast.IdUid (_, "Failure")))))),
                 (Ast.ExNil _), e)))
              -> handle_failure e
          | Ast.ExMat (_, me, a) ->
              let rec match_case_handle_failure =
                (function
                 | Ast.McOr (_, a1, a2) ->
                     (match_case_handle_failure a1) &&
                       (match_case_handle_failure a2)
                 | Ast.McArr (_, _, (Ast.ExNil _), e) -> handle_failure e
                 | _ -> false)
              in (handle_failure me) && (match_case_handle_failure a)
          | Ast.ExLet (_, Ast.ReNil, bi, e) ->
              let rec binding_handle_failure =
                (function
                 | Ast.BiAnd (_, b1, b2) ->
                     (binding_handle_failure b1) &&
                       (binding_handle_failure b2)
                 | Ast.BiEq (_, _, e) -> handle_failure e
                 | _ -> false)
              in (binding_handle_failure bi) && (handle_failure e)
          | Ast.ExId (_, (Ast.IdLid (_, _))) | Ast.ExInt (_, _) |
              Ast.ExStr (_, _) | Ast.ExChr (_, _) | Ast.ExFun (_, _) |
              Ast.ExId (_, (Ast.IdUid (_, _))) -> true
          | Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "raise")))), e) ->
              (match e with
               | Ast.ExId (_,
                   (Ast.IdAcc (_, (Ast.IdUid (_, "Stream")),
                      (Ast.IdUid (_, "Failure")))))
                   -> false
               | _ -> true)
          | Ast.ExApp (_, f, x) ->
              (is_constr_apply f) &&
                ((handle_failure f) && (handle_failure x))
          | _ -> false
        and is_constr_apply =
          function
          | Ast.ExId (_, (Ast.IdUid (_, _))) -> true
          | Ast.ExId (_, (Ast.IdLid (_, _))) -> false
          | Ast.ExApp (_, x, _) -> is_constr_apply x
          | _ -> false
          
        let rec subst v e =
          let _loc = Ast.loc_of_expr e
          in
            match e with
            | Ast.ExId (_, (Ast.IdLid (_, x))) ->
                let x = if x = v then strm_n else x
                in Ast.ExId (_loc, (Ast.IdLid (_loc, x)))
            | Ast.ExId (_, (Ast.IdUid (_, _))) -> e
            | Ast.ExInt (_, _) -> e
            | Ast.ExChr (_, _) -> e
            | Ast.ExStr (_, _) -> e
            | Ast.ExAcc (_, _, _) -> e
            | Ast.ExLet (_, rf, bi, e) ->
                Ast.ExLet (_loc, rf, (subst_binding v bi), (subst v e))
            | Ast.ExApp (_, e1, e2) ->
                Ast.ExApp (_loc, (subst v e1), (subst v e2))
            | Ast.ExTup (_, e) -> Ast.ExTup (_loc, (subst v e))
            | Ast.ExCom (_, e1, e2) ->
                Ast.ExCom (_loc, (subst v e1), (subst v e2))
            | _ -> raise Not_found
        and subst_binding v =
          function
          | Ast.BiAnd (_loc, b1, b2) ->
              Ast.BiAnd (_loc, (subst_binding v b1), (subst_binding v b2))
          | Ast.BiEq (_loc, (Ast.PaId (_, (Ast.IdLid (_, v')))), e) ->
              Ast.BiEq (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, v')))),
                (if v = v' then e else subst v e))
          | _ -> raise Not_found
          
        let stream_pattern_component skont ckont =
          function
          | SpTrm (_loc, p, None) ->
              Ast.ExMat (_loc,
                (Ast.ExApp (_loc, (peek_fun _loc),
                   (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
                (Ast.McOr (_loc,
                   (Ast.McArr (_loc,
                      (Ast.PaApp (_loc,
                         (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))), p)),
                      (Ast.ExNil _loc),
                      (Ast.ExSeq (_loc,
                         (Ast.ExSem (_loc,
                            (Ast.ExApp (_loc, (junk_fun _loc),
                               (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
                            skont)))))),
                   (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                      ckont)))))
          | SpTrm (_loc, p, (Some w)) ->
              Ast.ExMat (_loc,
                (Ast.ExApp (_loc, (peek_fun _loc),
                   (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
                (Ast.McOr (_loc,
                   (Ast.McArr (_loc,
                      (Ast.PaApp (_loc,
                         (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))), p)),
                      w,
                      (Ast.ExSeq (_loc,
                         (Ast.ExSem (_loc,
                            (Ast.ExApp (_loc, (junk_fun _loc),
                               (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
                            skont)))))),
                   (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                      ckont)))))
          | SpNtr (_loc, p, e) ->
              let e =
                (match e with
                 | Ast.ExFun (_,
                     (Ast.McArr (_,
                        (Ast.PaTyc (_, (Ast.PaId (_, (Ast.IdLid (_, v)))),
                           (Ast.TyApp (_,
                              (Ast.TyId (_,
                                 (Ast.IdAcc (_, (Ast.IdUid (_, "Stream")),
                                    (Ast.IdLid (_, "t")))))),
                              (Ast.TyAny _))))),
                        (Ast.ExNil _), e)))
                     when v = strm_n -> e
                 | _ ->
                     Ast.ExApp (_loc, e,
                       (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n))))))
              in
                if pattern_eq_expression p skont
                then
                  if is_raise_failure ckont
                  then e
                  else
                    if handle_failure e
                    then e
                    else
                      Ast.ExTry (_loc, e,
                        (Ast.McArr (_loc,
                           (Ast.PaId (_loc,
                              (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                                 (Ast.IdUid (_loc, "Failure")))))),
                           (Ast.ExNil _loc), ckont)))
                else
                  if is_raise_failure ckont
                  then
                    Ast.ExLet (_loc, Ast.ReNil, (Ast.BiEq (_loc, p, e)),
                      skont)
                  else
                    if
                      pattern_eq_expression
                        (Ast.PaApp (_loc,
                           (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))), p))
                        skont
                    then
                      Ast.ExTry (_loc,
                        (Ast.ExApp (_loc,
                           (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))), e)),
                        (Ast.McArr (_loc,
                           (Ast.PaId (_loc,
                              (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                                 (Ast.IdUid (_loc, "Failure")))))),
                           (Ast.ExNil _loc), ckont)))
                    else
                      if is_raise ckont
                      then
                        (let tst =
                           if handle_failure e
                           then e
                           else
                             Ast.ExTry (_loc, e,
                               (Ast.McArr (_loc,
                                  (Ast.PaId (_loc,
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Stream")),
                                        (Ast.IdUid (_loc, "Failure")))))),
                                  (Ast.ExNil _loc), ckont)))
                         in
                           Ast.ExLet (_loc, Ast.ReNil,
                             (Ast.BiEq (_loc, p, tst)), skont))
                      else
                        Ast.ExMat (_loc,
                          (Ast.ExTry (_loc,
                             (Ast.ExApp (_loc,
                                (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))),
                                e)),
                             (Ast.McArr (_loc,
                                (Ast.PaId (_loc,
                                   (Ast.IdAcc (_loc,
                                      (Ast.IdUid (_loc, "Stream")),
                                      (Ast.IdUid (_loc, "Failure")))))),
                                (Ast.ExNil _loc),
                                (Ast.ExId (_loc, (Ast.IdUid (_loc, "None")))))))),
                          (Ast.McOr (_loc,
                             (Ast.McArr (_loc,
                                (Ast.PaApp (_loc,
                                   (Ast.PaId (_loc,
                                      (Ast.IdUid (_loc, "Some")))),
                                   p)),
                                (Ast.ExNil _loc), skont)),
                             (Ast.McArr (_loc, (Ast.PaAny _loc),
                                (Ast.ExNil _loc), ckont)))))
          | SpStr (_loc, p) ->
              (try
                 match p with
                 | Ast.PaId (_, (Ast.IdLid (_, v))) -> subst v skont
                 | _ -> raise Not_found
               with
               | Not_found ->
                   Ast.ExLet (_loc, Ast.ReNil,
                     (Ast.BiEq (_loc, p,
                        (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
                     skont))
          
        let rec stream_pattern _loc epo e ekont =
          function
          | [] ->
              (match epo with
               | Some ep ->
                   Ast.ExLet (_loc, Ast.ReNil,
                     (Ast.BiEq (_loc, ep,
                        (Ast.ExApp (_loc,
                           (Ast.ExId (_loc,
                              (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                                 (Ast.IdLid (_loc, "count")))))),
                           (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))))),
                     e)
               | _ -> e)
          | (spc, err) :: spcl ->
              let skont =
                let ekont err =
                  let str =
                    (match err with
                     | Some estr -> estr
                     | _ -> Ast.ExStr (_loc, ""))
                  in
                    Ast.ExApp (_loc,
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
                      (Ast.ExApp (_loc,
                         (Ast.ExId (_loc,
                            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                               (Ast.IdUid (_loc, "Error")))))),
                         str)))
                in stream_pattern _loc epo e ekont spcl in
              let ckont = ekont err
              in stream_pattern_component skont ckont spc
          
        let stream_patterns_term _loc ekont tspel =
          let pel =
            List.fold_right
              (fun (p, w, _loc, spcl, epo, e) acc ->
                 let p =
                   Ast.PaApp (_loc,
                     (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))), p) in
                 let e =
                   let ekont err =
                     let str =
                       match err with
                       | Some estr -> estr
                       | _ -> Ast.ExStr (_loc, "")
                     in
                       Ast.ExApp (_loc,
                         (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
                         (Ast.ExApp (_loc,
                            (Ast.ExId (_loc,
                               (Ast.IdAcc (_loc,
                                  (Ast.IdUid (_loc, "Stream")),
                                  (Ast.IdUid (_loc, "Error")))))),
                            str))) in
                   let skont = stream_pattern _loc epo e ekont spcl
                   in
                     Ast.ExSeq (_loc,
                       (Ast.ExSem (_loc,
                          (Ast.ExApp (_loc, (junk_fun _loc),
                             (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
                          skont)))
                 in
                   match w with
                   | Some w ->
                       Ast.McOr (_loc, (Ast.McArr (_loc, p, w, e)), acc)
                   | None ->
                       Ast.McOr (_loc,
                         (Ast.McArr (_loc, p, (Ast.ExNil _loc), e)), acc))
              tspel (Ast.McNil _loc)
          in
            Ast.ExMat (_loc,
              (Ast.ExApp (_loc, (peek_fun _loc),
                 (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
              (Ast.McOr (_loc, pel,
                 (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                    (ekont ()))))))
          
        let rec group_terms =
          function
          | ((SpTrm (_loc, p, w), None) :: spcl, epo, e) :: spel ->
              let (tspel, spel) = group_terms spel
              in (((p, w, _loc, spcl, epo, e) :: tspel), spel)
          | spel -> ([], spel)
          
        let rec parser_cases _loc =
          function
          | [] ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                      (Ast.IdUid (_loc, "Failure")))))))
          | spel ->
              (match group_terms spel with
               | ([], (spcl, epo, e) :: spel) ->
                   stream_pattern _loc epo e
                     (fun _ -> parser_cases _loc spel) spcl
               | (tspel, spel) ->
                   stream_patterns_term _loc
                     (fun _ -> parser_cases _loc spel) tspel)
          
        let cparser _loc bpo pc =
          let e = parser_cases _loc pc in
          let e =
            match bpo with
            | Some bp ->
                Ast.ExLet (_loc, Ast.ReNil,
                  (Ast.BiEq (_loc, bp,
                     (Ast.ExApp (_loc,
                        (Ast.ExId (_loc,
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                              (Ast.IdLid (_loc, "count")))))),
                        (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))))),
                  e)
            | None -> e in
          let p =
            Ast.PaTyc (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, strm_n)))),
              (Ast.TyApp (_loc,
                 (Ast.TyId (_loc,
                    (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                       (Ast.IdLid (_loc, "t")))))),
                 (Ast.TyAny _loc))))
          in Ast.ExFun (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e)))
          
        let cparser_match _loc me bpo pc =
          let pc = parser_cases _loc pc in
          let e =
            match bpo with
            | Some bp ->
                Ast.ExLet (_loc, Ast.ReNil,
                  (Ast.BiEq (_loc, bp,
                     (Ast.ExApp (_loc,
                        (Ast.ExId (_loc,
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                              (Ast.IdLid (_loc, "count")))))),
                        (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))))),
                  pc)
            | None -> pc in
          let me =
            match me with
            | (Ast.ExSem (_loc, _, _) as e) -> Ast.ExSeq (_loc, e)
            | e -> e
          in
            match me with
            | Ast.ExId (_, (Ast.IdLid (_, x))) when x = strm_n -> e
            | _ ->
                Ast.ExLet (_loc, Ast.ReNil,
                  (Ast.BiEq (_loc,
                     (Ast.PaTyc (_loc,
                        (Ast.PaId (_loc, (Ast.IdLid (_loc, strm_n)))),
                        (Ast.TyApp (_loc,
                           (Ast.TyId (_loc,
                              (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                                 (Ast.IdLid (_loc, "t")))))),
                           (Ast.TyAny _loc))))),
                     me)),
                  e)
          
        (* streams *)
        let rec not_computing =
          function
          | Ast.ExId (_, (Ast.IdLid (_, _))) |
              Ast.ExId (_, (Ast.IdUid (_, _))) | Ast.ExInt (_, _) |
              Ast.ExFlo (_, _) | Ast.ExChr (_, _) | Ast.ExStr (_, _) -> true
          | Ast.ExApp (_, x, y) ->
              (is_cons_apply_not_computing x) && (not_computing y)
          | _ -> false
        and is_cons_apply_not_computing =
          function
          | Ast.ExId (_, (Ast.IdUid (_, _))) -> true
          | Ast.ExId (_, (Ast.IdLid (_, _))) -> false
          | Ast.ExApp (_, x, y) ->
              (is_cons_apply_not_computing x) && (not_computing y)
          | _ -> false
          
        let slazy _loc e =
          match e with
          | Ast.ExApp (_, f, (Ast.ExId (_, (Ast.IdUid (_, "()"))))) ->
              (match f with
               | Ast.ExId (_, (Ast.IdLid (_, _))) -> f
               | _ ->
                   Ast.ExFun (_loc,
                     (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), e))))
          | _ ->
              Ast.ExFun (_loc,
                (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), e)))
          
        let rec cstream gloc =
          function
          | [] ->
              let _loc = gloc
              in
                Ast.ExId (_loc,
                  (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                     (Ast.IdLid (_loc, "sempty")))))
          | [ SeTrm (_loc, e) ] ->
              if not_computing e
              then
                Ast.ExApp (_loc,
                  (Ast.ExId (_loc,
                     (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                        (Ast.IdLid (_loc, "ising")))))),
                  e)
              else
                Ast.ExApp (_loc,
                  (Ast.ExId (_loc,
                     (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                        (Ast.IdLid (_loc, "lsing")))))),
                  (slazy _loc e))
          | SeTrm (_loc, e) :: secl ->
              if not_computing e
              then
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                           (Ast.IdLid (_loc, "icons")))))),
                     e)),
                  (cstream gloc secl))
              else
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                           (Ast.IdLid (_loc, "lcons")))))),
                     (slazy _loc e))),
                  (cstream gloc secl))
          | [ SeNtr (_loc, e) ] ->
              if not_computing e
              then e
              else
                Ast.ExApp (_loc,
                  (Ast.ExId (_loc,
                     (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                        (Ast.IdLid (_loc, "slazy")))))),
                  (slazy _loc e))
          | SeNtr (_loc, e) :: secl ->
              if not_computing e
              then
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                           (Ast.IdLid (_loc, "iapp")))))),
                     e)),
                  (cstream gloc secl))
              else
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Stream")),
                           (Ast.IdLid (_loc, "lapp")))))),
                     (slazy _loc e))),
                  (cstream gloc secl))
          
        (* Syntax extensions in Revised Syntax grammar *)
        let _ =
          let _ = (expr : 'expr Gram.Entry.t)
          and _ = (parser_case_list : 'parser_case_list Gram.Entry.t)
          and _ = (parser_case : 'parser_case Gram.Entry.t)
          and _ = (stream_quot : 'stream_quot Gram.Entry.t)
          and _ = (stream_end : 'stream_end Gram.Entry.t)
          and _ = (stream_begin : 'stream_begin Gram.Entry.t)
          and _ = (stream_expr : 'stream_expr Gram.Entry.t) in
          let grammar_entry_create = Gram.Entry.mk in
          let stream_patt : 'stream_patt Gram.Entry.t =
            grammar_entry_create "stream_patt"
          and stream_expr_comp : 'stream_expr_comp Gram.Entry.t =
            grammar_entry_create "stream_expr_comp"
          and stream_expr_comp_list : 'stream_expr_comp_list Gram.Entry.t =
            grammar_entry_create "stream_expr_comp_list"
          and parser_ipatt : 'parser_ipatt Gram.Entry.t =
            grammar_entry_create "parser_ipatt"
          and stream_patt_comp : 'stream_patt_comp Gram.Entry.t =
            grammar_entry_create "stream_patt_comp"
          and stream_patt_comp_err_list :
            'stream_patt_comp_err_list Gram.Entry.t =
            grammar_entry_create "stream_patt_comp_err_list"
          and stream_patt_comp_err : 'stream_patt_comp_err Gram.Entry.t =
            grammar_entry_create "stream_patt_comp_err"
          in
            (Gram.extend (expr : 'expr Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.Level "top")),
                    [ (None, None,
                       [ ([ Gram.Skeyword "match";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (sequence : 'sequence Gram.Entry.t));
                            Gram.Skeyword "with"; Gram.Skeyword "parser";
                            Gram.Sopt
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (parser_ipatt :
                                      'parser_ipatt Gram.Entry.t)));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (parser_case_list :
                                   'parser_case_list Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (pcl : 'parser_case_list)
                                (po : 'parser_ipatt option) _ _
                                (e : 'sequence) _ (_loc : Gram.Loc.t) ->
                                (cparser_match _loc e po pcl : 'expr))));
                         ([ Gram.Skeyword "parser";
                            Gram.Sopt
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (parser_ipatt :
                                      'parser_ipatt Gram.Entry.t)));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (parser_case_list :
                                   'parser_case_list Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (pcl : 'parser_case_list)
                                (po : 'parser_ipatt option) _
                                (_loc : Gram.Loc.t) ->
                                (cparser _loc po pcl : 'expr)))) ]) ]))
                  ());
             Gram.extend (parser_case_list : 'parser_case_list Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (parser_case : 'parser_case Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (pc : 'parser_case) (_loc : Gram.Loc.t) ->
                                ([ pc ] : 'parser_case_list))));
                         ([ Gram.Skeyword "[";
                            Gram.Slist0sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj
                                     (parser_case :
                                       'parser_case Gram.Entry.t))),
                              (Gram.Skeyword "|"));
                            Gram.Skeyword "]" ],
                          (Gram.Action.mk
                             (fun _ (pcl : 'parser_case list) _
                                (_loc : Gram.Loc.t) ->
                                (pcl : 'parser_case_list)))) ]) ]))
                  ());
             Gram.extend (parser_case : 'parser_case Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_begin : 'stream_begin Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt : 'stream_patt Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_end : 'stream_end Gram.Entry.t));
                            Gram.Sopt
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (parser_ipatt :
                                      'parser_ipatt Gram.Entry.t)));
                            Gram.Skeyword "->";
                            Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'expr) _ (po : 'parser_ipatt option) _
                                (sp : 'stream_patt) _ (_loc : Gram.Loc.t) ->
                                ((sp, po, e) : 'parser_case)))) ]) ]))
                  ());
             Gram.extend (stream_begin : 'stream_begin Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "[:" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (() : 'stream_begin)))) ]) ]))
                  ());
             Gram.extend (stream_end : 'stream_end Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword ":]" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) -> (() : 'stream_end)))) ]) ]))
                  ());
             Gram.extend (stream_quot : 'stream_quot Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "`" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (() : 'stream_quot)))) ]) ]))
                  ());
             Gram.extend (stream_expr : 'stream_expr Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                (e : 'stream_expr)))) ]) ]))
                  ());
             Gram.extend (stream_patt : 'stream_patt Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([],
                          (Gram.Action.mk
                             (fun (_loc : Gram.Loc.t) -> ([] : 'stream_patt))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt_comp :
                                   'stream_patt_comp Gram.Entry.t));
                            Gram.Skeyword ";";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt_comp_err_list :
                                   'stream_patt_comp_err_list Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (sp : 'stream_patt_comp_err_list) _
                                (spc : 'stream_patt_comp) (_loc : Gram.Loc.t)
                                -> ((spc, None) :: sp : 'stream_patt))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt_comp :
                                   'stream_patt_comp Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (spc : 'stream_patt_comp)
                                (_loc : Gram.Loc.t) ->
                                ([ (spc, None) ] : 'stream_patt)))) ]) ]))
                  ());
             Gram.extend
               (stream_patt_comp_err : 'stream_patt_comp_err Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt_comp :
                                   'stream_patt_comp Gram.Entry.t));
                            Gram.Sopt
                              (Gram.srules stream_patt_comp_err
                                 [ ([ Gram.Skeyword "??";
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (stream_expr :
                                             'stream_expr Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun (e : 'stream_expr) _
                                          (_loc : Gram.Loc.t) -> (e : 'e__14)))) ]) ],
                          (Gram.Action.mk
                             (fun (eo : 'e__14 option)
                                (spc : 'stream_patt_comp) (_loc : Gram.Loc.t)
                                -> ((spc, eo) : 'stream_patt_comp_err)))) ]) ]))
                  ());
             Gram.extend
               (stream_patt_comp_err_list :
                 'stream_patt_comp_err_list Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt_comp_err :
                                   'stream_patt_comp_err Gram.Entry.t));
                            Gram.Skeyword ";"; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (sp : 'stream_patt_comp_err_list) _
                                (spc : 'stream_patt_comp_err)
                                (_loc : Gram.Loc.t) ->
                                (spc :: sp : 'stream_patt_comp_err_list))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt_comp_err :
                                   'stream_patt_comp_err Gram.Entry.t));
                            Gram.Skeyword ";" ],
                          (Gram.Action.mk
                             (fun _ (spc : 'stream_patt_comp_err)
                                (_loc : Gram.Loc.t) ->
                                ([ spc ] : 'stream_patt_comp_err_list))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_patt_comp_err :
                                   'stream_patt_comp_err Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (spc : 'stream_patt_comp_err)
                                (_loc : Gram.Loc.t) ->
                                ([ spc ] : 'stream_patt_comp_err_list)))) ]) ]))
                  ());
             Gram.extend (stream_patt_comp : 'stream_patt_comp Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (p : 'patt) (_loc : Gram.Loc.t) ->
                                (SpStr (_loc, p) : 'stream_patt_comp))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                            Gram.Skeyword "=";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_expr : 'stream_expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'stream_expr) _ (p : 'patt)
                                (_loc : Gram.Loc.t) ->
                                (SpNtr (_loc, p, e) : 'stream_patt_comp))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_quot : 'stream_quot Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj (patt : 'patt Gram.Entry.t));
                            Gram.Sopt
                              (Gram.srules stream_patt_comp
                                 [ ([ Gram.Skeyword "when";
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (stream_expr :
                                             'stream_expr Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun (e : 'stream_expr) _
                                          (_loc : Gram.Loc.t) -> (e : 'e__15)))) ]) ],
                          (Gram.Action.mk
                             (fun (eo : 'e__15 option) (p : 'patt) _
                                (_loc : Gram.Loc.t) ->
                                (SpTrm (_loc, p, eo) : 'stream_patt_comp)))) ]) ]))
                  ());
             Gram.extend (parser_ipatt : 'parser_ipatt Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "_" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (Ast.PaAny _loc : 'parser_ipatt))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                (Ast.PaId (_loc, (Ast.IdLid (_loc, i))) :
                                  'parser_ipatt)))) ]) ]))
                  ());
             Gram.extend (expr : 'expr Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.Level "simple")),
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_begin : 'stream_begin Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_expr_comp_list :
                                   'stream_expr_comp_list Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_end : 'stream_end Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (sel : 'stream_expr_comp_list) _
                                (_loc : Gram.Loc.t) ->
                                (cstream _loc sel : 'expr))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_begin : 'stream_begin Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_end : 'stream_end Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ _ (_loc : Gram.Loc.t) ->
                                (cstream _loc [] : 'expr)))) ]) ]))
                  ());
             Gram.extend
               (stream_expr_comp_list : 'stream_expr_comp_list Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_expr_comp :
                                   'stream_expr_comp Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (se : 'stream_expr_comp)
                                (_loc : Gram.Loc.t) ->
                                ([ se ] : 'stream_expr_comp_list))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_expr_comp :
                                   'stream_expr_comp Gram.Entry.t));
                            Gram.Skeyword ";" ],
                          (Gram.Action.mk
                             (fun _ (se : 'stream_expr_comp)
                                (_loc : Gram.Loc.t) ->
                                ([ se ] : 'stream_expr_comp_list))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_expr_comp :
                                   'stream_expr_comp Gram.Entry.t));
                            Gram.Skeyword ";"; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (sel : 'stream_expr_comp_list) _
                                (se : 'stream_expr_comp) (_loc : Gram.Loc.t)
                                -> (se :: sel : 'stream_expr_comp_list)))) ]) ]))
                  ());
             Gram.extend (stream_expr_comp : 'stream_expr_comp Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_expr : 'stream_expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'stream_expr) (_loc : Gram.Loc.t) ->
                                (SeNtr (_loc, e) : 'stream_expr_comp))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_quot : 'stream_quot Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (stream_expr : 'stream_expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'stream_expr) _ (_loc : Gram.Loc.t) ->
                                (SeTrm (_loc, e) : 'stream_expr_comp)))) ]) ]))
                  ()))
          
      end
      
    module M = Register.OCamlSyntaxExtension(Id)(Make)
      
  end
  
module G =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
    module Id =
      struct let name = "Camlp4GrammarParser"
                let version = Sys.ocaml_version
                  
      end
      
    module Make (Syntax : Sig.Camlp4Syntax) =
      struct
        open Sig
          
        include Syntax
          
        module MetaLoc = Ast.Meta.MetaGhostLoc
          
        module MetaAst = Ast.Meta.Make(MetaLoc)
          
        module PP = Camlp4.Printers.OCaml.Make(Syntax)
          
        let pp = new PP.printer ~comments: false ()
          
        let string_of_patt patt =
          let buf = Buffer.create 42 in
          let () = Format.bprintf buf "%a@?" pp#patt patt in
          let str = Buffer.contents buf
          in if str = "" then assert false else str
          
        let split_ext = ref false
          
        type loc = Loc.t
        
        type 'e name = { expr : 'e; tvar : string; loc : loc }
        
        type styp =
          | STlid of loc * string
          | STapp of loc * styp * styp
          | STquo of loc * string
          | STself of loc * string
          | STtok of loc
          | STstring_tok of loc
          | STtyp of Ast.ctyp
        
        type ('e, 'p) text =
          | TXmeta of loc * string * (('e, 'p) text) list * 'e * styp
          | TXlist of loc * bool * ('e, 'p) symbol * (('e, 'p) symbol) option
          | TXnext of loc
          | TXnterm of loc * 'e name * string option
          | TXopt of loc * ('e, 'p) text
          | TXtry of loc * ('e, 'p) text
          | TXrules of loc * (((('e, 'p) text) list) * 'e) list
          | TXself of loc
          | TXkwd of loc * string
          | TXtok of loc * 'e * string
          and (** The first is the match function expr,
             the second is the string description.
             The description string will be used for
             grammar insertion and left factoring.
             Keep this string normalized and well comparable. *)
          ('e, 'p) entry =
          { name : 'e name; pos : 'e option; levels : (('e, 'p) level) list
          }
          and ('e, 'p) level =
          { label : string option; assoc : 'e option;
            rules : (('e, 'p) rule) list
          }
          and ('e, 'p) rule =
          { prod : (('e, 'p) symbol) list; action : 'e option
          }
          and ('e, 'p) symbol =
          { used : string list; text : ('e, 'p) text; styp : styp;
            pattern : 'p option
          }
        
        type used = | Unused | UsedScanned | UsedNotScanned
        
        let _loc = Loc.ghost
          
        let gm = "Camlp4Grammar__"
          
        let mark_used modif ht n =
          try
            let rll = Hashtbl.find_all ht n
            in
              List.iter
                (fun (r, _) ->
                   if !r == Unused
                   then (r := UsedNotScanned; modif := true)
                   else ())
                rll
          with | Not_found -> ()
          
        let rec mark_symbol modif ht symb =
          List.iter (fun e -> mark_used modif ht e) symb.used
          
        let check_use nl el =
          let ht = Hashtbl.create 301 in
          let modif = ref false
          in
            (List.iter
               (fun e ->
                  let u =
                    match e.name.expr with
                    | Ast.ExId (_, (Ast.IdLid (_, _))) -> Unused
                    | _ -> UsedNotScanned
                  in Hashtbl.add ht e.name.tvar ((ref u), e))
               el;
             List.iter
               (fun n ->
                  try
                    let rll = Hashtbl.find_all ht n.tvar
                    in List.iter (fun (r, _) -> r := UsedNotScanned) rll
                  with | _ -> ())
               nl;
             modif := true;
             while !modif do modif := false;
               Hashtbl.iter
                 (fun _ (r, e) ->
                    if !r = UsedNotScanned
                    then
                      (r := UsedScanned;
                       List.iter
                         (fun level ->
                            let rules = level.rules
                            in
                              List.iter
                                (fun rule ->
                                   List.iter
                                     (fun s -> mark_symbol modif ht s)
                                     rule.prod)
                                rules)
                         e.levels)
                    else ())
                 ht
               done;
             Hashtbl.iter
               (fun s (r, e) ->
                  if !r = Unused
                  then
                    print_warning e.name.loc
                      ("Unused local entry \"" ^ (s ^ "\""))
                  else ())
               ht)
          
        let new_type_var =
          let i = ref 0 in fun () -> (incr i; "e__" ^ (string_of_int !i))
          
        let used_of_rule_list rl =
          List.fold_left
            (fun nl r -> List.fold_left (fun nl s -> s.used @ nl) nl r.prod)
            [] rl
          
        let retype_rule_list_without_patterns _loc rl =
          try
            List.map
              (function
               | (* ...; [ "foo" ]; ... ==> ...; (x = [ "foo" ] -> Gram.Token.extract_string x); ... *)
                   {
                     prod = [ ({ pattern = None; styp = STtok _ } as s) ];
                     action = None
                   } ->
                   {
                     prod =
                       [ {
                           (s)
                           with
                           pattern =
                             Some (Ast.PaId (_loc, (Ast.IdLid (_loc, "x"))));
                         } ];
                     action =
                       Some
                         (Ast.ExApp (_loc,
                            (Ast.ExId (_loc,
                               (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Token")),
                                     (Ast.IdLid (_loc, "extract_string")))))))),
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "x"))))));
                   }
               | (* ...; [ symb ]; ... ==> ...; (x = [ symb ] -> x); ... *)
                   { prod = [ ({ pattern = None } as s) ]; action = None } ->
                   {
                     prod =
                       [ {
                           (s)
                           with
                           pattern =
                             Some (Ast.PaId (_loc, (Ast.IdLid (_loc, "x"))));
                         } ];
                     action = Some (Ast.ExId (_loc, (Ast.IdLid (_loc, "x"))));
                   }
               | (* ...; ([] -> a); ... *)
                   ({ prod = []; action = Some _ } as r) -> r
               | _ -> raise Exit)
              rl
          with | Exit -> rl
          
        let meta_action = ref false
          
        let mklistexp _loc =
          let rec loop top =
            function
            | [] -> Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))
            | e1 :: el ->
                let _loc =
                  if top then _loc else Loc.merge (Ast.loc_of_expr e1) _loc
                in
                  Ast.ExApp (_loc,
                    (Ast.ExApp (_loc,
                       (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e1)),
                    (loop false el))
          in loop true
          
        let mklistpat _loc =
          let rec loop top =
            function
            | [] -> Ast.PaId (_loc, (Ast.IdUid (_loc, "[]")))
            | p1 :: pl ->
                let _loc =
                  if top then _loc else Loc.merge (Ast.loc_of_patt p1) _loc
                in
                  Ast.PaApp (_loc,
                    (Ast.PaApp (_loc,
                       (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))), p1)),
                    (loop false pl))
          in loop true
          
        let rec expr_fa al =
          function
          | Ast.ExApp (_, f, a) -> expr_fa (a :: al) f
          | f -> (f, al)
          
        let rec make_ctyp styp tvar =
          match styp with
          | STlid (_loc, s) -> Ast.TyId (_loc, (Ast.IdLid (_loc, s)))
          | STapp (_loc, t1, t2) ->
              Ast.TyApp (_loc, (make_ctyp t1 tvar), (make_ctyp t2 tvar))
          | STquo (_loc, s) -> Ast.TyQuo (_loc, s)
          | STself (_loc, x) ->
              if tvar = ""
              then
                Loc.raise _loc
                  (Stream.Error
                     ("'" ^ (x ^ "' illegal in anonymous entry level")))
              else Ast.TyQuo (_loc, tvar)
          | STtok _loc ->
              Ast.TyId (_loc,
                (Ast.IdAcc (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                      (Ast.IdUid (_loc, "Token")))),
                   (Ast.IdLid (_loc, "t")))))
          | STstring_tok _loc ->
              Ast.TyId (_loc, (Ast.IdLid (_loc, "string")))
          | STtyp t -> t
          
        let make_ctyp_patt styp tvar patt =
          let styp =
            match styp with | STstring_tok _loc -> STtok _loc | t -> t
          in
            match make_ctyp styp tvar with
            | Ast.TyAny _ -> patt
            | t ->
                let _loc = Ast.loc_of_patt patt in Ast.PaTyc (_loc, patt, t)
          
        let make_ctyp_expr styp tvar expr =
          match make_ctyp styp tvar with
          | Ast.TyAny _ -> expr
          | t -> let _loc = Ast.loc_of_expr expr in Ast.ExTyc (_loc, expr, t)
          
        let text_of_action _loc psl rtvar act tvar =
          let locid = Ast.PaId (_loc, (Ast.IdLid (_loc, !Loc.name))) in
          let act =
            match act with
            | Some act -> act
            | None -> Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) in
          let (tok_match_pl, act, _) =
            List.fold_left
              (fun (((tok_match_pl, act, i) as accu)) ->
                 function
                 | { pattern = None } -> accu
                 | { pattern = Some p } when Ast.is_irrefut_patt p -> accu
                 | {
                     pattern =
                       Some
                         (Ast.PaAli (_,
                            (Ast.PaApp (_, _, (Ast.PaTup (_, (Ast.PaAny _))))),
                            (Ast.PaId (_, (Ast.IdLid (_, s))))))
                     } ->
                     (tok_match_pl,
                      (Ast.ExLet (_loc, Ast.ReNil,
                         (Ast.BiEq (_loc,
                            (Ast.PaId (_loc, (Ast.IdLid (_loc, s)))),
                            (Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Token")),
                                        (Ast.IdLid (_loc, "extract_string")))))))),
                               (Ast.ExId (_loc, (Ast.IdLid (_loc, s)))))))),
                         act)),
                      i)
                 | { pattern = Some p; text = TXtok (_, _, _) } ->
                     let id = "__camlp4_" ^ (string_of_int i)
                     in
                       ((Some
                           (match tok_match_pl with
                            | None ->
                                ((Ast.ExId (_loc, (Ast.IdLid (_loc, id)))),
                                 p)
                            | Some ((tok_pl, match_pl)) ->
                                ((Ast.ExCom (_loc,
                                    (Ast.ExId (_loc, (Ast.IdLid (_loc, id)))),
                                    tok_pl)),
                                 (Ast.PaCom (_loc, p, match_pl))))),
                        act, (succ i))
                 | _ -> accu)
              (None, act, 0) psl in
          let e =
            let e1 = Ast.ExTyc (_loc, act, (Ast.TyQuo (_loc, rtvar))) in
            let e2 =
              match tok_match_pl with
              | None -> e1
              | Some ((Ast.ExCom (_, t1, t2), Ast.PaCom (_, p1, p2))) ->
                  Ast.ExMat (_loc,
                    (Ast.ExTup (_loc, (Ast.ExCom (_loc, t1, t2)))),
                    (Ast.McOr (_loc,
                       (Ast.McArr (_loc,
                          (Ast.PaTup (_loc, (Ast.PaCom (_loc, p1, p2)))),
                          (Ast.ExNil _loc), e1)),
                       (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                          (Ast.ExAsf _loc))))))
              | Some ((tok, match_)) ->
                  Ast.ExMat (_loc, tok,
                    (Ast.McOr (_loc,
                       (Ast.McArr (_loc, match_, (Ast.ExNil _loc), e1)),
                       (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                          (Ast.ExAsf _loc))))))
            in
              Ast.ExFun (_loc,
                (Ast.McArr (_loc,
                   (Ast.PaTyc (_loc, locid,
                      (Ast.TyId (_loc,
                         (Ast.IdAcc (_loc,
                            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                               (Ast.IdUid (_loc, "Loc")))),
                            (Ast.IdLid (_loc, "t")))))))),
                   (Ast.ExNil _loc), e2))) in
          let (txt, _) =
            List.fold_left
              (fun (txt, i) s ->
                 match s.pattern with
                 | None | Some (Ast.PaAny _) ->
                     ((Ast.ExFun (_loc,
                         (Ast.McArr (_loc, (Ast.PaAny _loc),
                            (Ast.ExNil _loc), txt)))),
                      i)
                 | Some
                     (Ast.PaAli (_,
                        (Ast.PaApp (_, _, (Ast.PaTup (_, (Ast.PaAny _))))),
                        p))
                     ->
                     let p = make_ctyp_patt s.styp tvar p
                     in
                       ((Ast.ExFun (_loc,
                           (Ast.McArr (_loc, p, (Ast.ExNil _loc), txt)))),
                        i)
                 | Some p when Ast.is_irrefut_patt p ->
                     let p = make_ctyp_patt s.styp tvar p
                     in
                       ((Ast.ExFun (_loc,
                           (Ast.McArr (_loc, p, (Ast.ExNil _loc), txt)))),
                        i)
                 | Some _ ->
                     let p =
                       make_ctyp_patt s.styp tvar
                         (Ast.PaId (_loc,
                            (Ast.IdLid (_loc,
                               ("__camlp4_" ^ (string_of_int i))))))
                     in
                       ((Ast.ExFun (_loc,
                           (Ast.McArr (_loc, p, (Ast.ExNil _loc), txt)))),
                        (succ i)))
              (e, 0) psl in
          let txt =
            if !meta_action
            then
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Obj")),
                      (Ast.IdLid (_loc, "magic")))))),
                (MetaAst.Expr.meta_expr _loc txt))
            else txt
          in
            Ast.ExApp (_loc,
              (Ast.ExId (_loc,
                 (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                    (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Action")),
                       (Ast.IdLid (_loc, "mk")))))))),
              txt)
          
        let srules loc t rl tvar =
          List.map
            (fun r ->
               let sl = List.map (fun s -> s.text) r.prod in
               let ac = text_of_action loc r.prod t r.action tvar in (sl, ac))
            rl
          
        let rec make_expr entry tvar =
          function
          | TXmeta (_loc, n, tl, e, t) ->
              let el =
                List.fold_right
                  (fun t el ->
                     Ast.ExApp (_loc,
                       (Ast.ExApp (_loc,
                          (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                          (make_expr entry "" t))),
                       el))
                  tl (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))
              in
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExApp (_loc,
                        (Ast.ExId (_loc,
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                              (Ast.IdUid (_loc, "Smeta")))))),
                        (Ast.ExStr (_loc, n)))),
                     el)),
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Action")),
                              (Ast.IdLid (_loc, "mk")))))))),
                     (make_ctyp_expr t tvar e))))
          | TXlist (_loc, min, t, ts) ->
              let txt = make_expr entry "" t.text
              in
                (match (min, ts) with
                 | (false, None) ->
                     Ast.ExApp (_loc,
                       (Ast.ExId (_loc,
                          (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                             (Ast.IdUid (_loc, "Slist0")))))),
                       txt)
                 | (true, None) ->
                     Ast.ExApp (_loc,
                       (Ast.ExId (_loc,
                          (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                             (Ast.IdUid (_loc, "Slist1")))))),
                       txt)
                 | (false, Some s) ->
                     let x = make_expr entry tvar s.text
                     in
                       Ast.ExApp (_loc,
                         (Ast.ExApp (_loc,
                            (Ast.ExId (_loc,
                               (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                  (Ast.IdUid (_loc, "Slist0sep")))))),
                            txt)),
                         x)
                 | (true, Some s) ->
                     let x = make_expr entry tvar s.text
                     in
                       Ast.ExApp (_loc,
                         (Ast.ExApp (_loc,
                            (Ast.ExId (_loc,
                               (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                  (Ast.IdUid (_loc, "Slist1sep")))))),
                            txt)),
                         x))
          | TXnext _loc ->
              Ast.ExId (_loc,
                (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                   (Ast.IdUid (_loc, "Snext")))))
          | TXnterm (_loc, n, lev) ->
              (match lev with
               | Some lab ->
                   Ast.ExApp (_loc,
                     (Ast.ExApp (_loc,
                        (Ast.ExId (_loc,
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                              (Ast.IdUid (_loc, "Snterml")))))),
                        (Ast.ExApp (_loc,
                           (Ast.ExId (_loc,
                              (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                 (Ast.IdAcc (_loc,
                                    (Ast.IdUid (_loc, "Entry")),
                                    (Ast.IdLid (_loc, "obj")))))))),
                           (Ast.ExTyc (_loc, n.expr,
                              (Ast.TyApp (_loc,
                                 (Ast.TyId (_loc,
                                    (Ast.IdAcc (_loc,
                                       (Ast.IdAcc (_loc,
                                          (Ast.IdUid (_loc, gm)),
                                          (Ast.IdUid (_loc, "Entry")))),
                                       (Ast.IdLid (_loc, "t")))))),
                                 (Ast.TyQuo (_loc, n.tvar)))))))))),
                     (Ast.ExStr (_loc, lab)))
               | None ->
                   if n.tvar = tvar
                   then
                     Ast.ExId (_loc,
                       (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                          (Ast.IdUid (_loc, "Sself")))))
                   else
                     Ast.ExApp (_loc,
                       (Ast.ExId (_loc,
                          (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                             (Ast.IdUid (_loc, "Snterm")))))),
                       (Ast.ExApp (_loc,
                          (Ast.ExId (_loc,
                             (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                (Ast.IdAcc (_loc,
                                   (Ast.IdUid (_loc, "Entry")),
                                   (Ast.IdLid (_loc, "obj")))))))),
                          (Ast.ExTyc (_loc, n.expr,
                             (Ast.TyApp (_loc,
                                (Ast.TyId (_loc,
                                   (Ast.IdAcc (_loc,
                                      (Ast.IdAcc (_loc,
                                         (Ast.IdUid (_loc, gm)),
                                         (Ast.IdUid (_loc, "Entry")))),
                                      (Ast.IdLid (_loc, "t")))))),
                                (Ast.TyQuo (_loc, n.tvar))))))))))
          | TXopt (_loc, t) ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                      (Ast.IdUid (_loc, "Sopt")))))),
                (make_expr entry "" t))
          | TXtry (_loc, t) ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                      (Ast.IdUid (_loc, "Stry")))))),
                (make_expr entry "" t))
          | TXrules (_loc, rl) ->
              Ast.ExApp (_loc,
                (Ast.ExApp (_loc,
                   (Ast.ExId (_loc,
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                         (Ast.IdLid (_loc, "srules")))))),
                   entry.expr)),
                (make_expr_rules _loc entry rl ""))
          | TXself _loc ->
              Ast.ExId (_loc,
                (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                   (Ast.IdUid (_loc, "Sself")))))
          | TXkwd (_loc, kwd) ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                      (Ast.IdUid (_loc, "Skeyword")))))),
                (Ast.ExStr (_loc, kwd)))
          | TXtok (_loc, match_fun, descr) ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                      (Ast.IdUid (_loc, "Stoken")))))),
                (Ast.ExTup (_loc,
                   (Ast.ExCom (_loc, match_fun,
                      (Ast.ExStr (_loc, (Ast.safe_string_escaped descr))))))))
        and make_expr_rules _loc n rl tvar =
          List.fold_left
            (fun txt (sl, ac) ->
               let sl =
                 List.fold_right
                   (fun t txt ->
                      let x = make_expr n tvar t
                      in
                        Ast.ExApp (_loc,
                          (Ast.ExApp (_loc,
                             (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), x)),
                          txt))
                   sl (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))
               in
                 Ast.ExApp (_loc,
                   (Ast.ExApp (_loc,
                      (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                      (Ast.ExTup (_loc, (Ast.ExCom (_loc, sl, ac)))))),
                   txt))
            (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))) rl
          
        let expr_of_delete_rule _loc n sl =
          let sl =
            List.fold_right
              (fun s e ->
                 Ast.ExApp (_loc,
                   (Ast.ExApp (_loc,
                      (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                      (make_expr n "" s.text))),
                   e))
              sl (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))
          in ((n.expr), sl)
          
        let rec tvar_of_ident =
          function
          | Ast.IdLid (_, x) | Ast.IdUid (_, x) -> x
          | Ast.IdAcc (_, (Ast.IdUid (_, x)), xs) ->
              x ^ ("__" ^ (tvar_of_ident xs))
          | _ -> failwith "internal error in the Grammar extension"
          
        let mk_name _loc i =
          { expr = Ast.ExId (_loc, i); tvar = tvar_of_ident i; loc = _loc; }
          
        let slist loc min sep symb = TXlist (loc, min, symb, sep)
          
        (*
  value sstoken _loc s =
    let n = mk_name _loc <:ident< $lid:"a_" ^ s$ >> in
    TXnterm _loc n None
  ;

  value mk_symbol p s t =
    {used = []; text = s; styp = t; pattern=Some p};

  value sslist _loc min sep s =
    let rl =
      let r1 =
        let prod =
          let n = mk_name _loc <:ident< a_list >> in
          [mk_symbol <:patt< a >> (TXnterm _loc n None) (STquo _loc "a_list")]
        in
        let act = <:expr< a >> in
        {prod = prod; action = Some act}
      in
      let r2 =
        let prod =
          [mk_symbol <:patt< a >> (slist _loc min sep s)
            (STapp _loc (STlid _loc "list") s.styp)]
        in
        let act = <:expr< Qast.List a >> in
        {prod = prod; action = Some act}
      in
      [r1; r2]
    in
    let used =
      match sep with
      [ Some symb -> symb.used @ s.used
      | None -> s.used ]
    in
    let used = ["a_list" :: used] in
    let text = TXrules _loc (srules _loc "a_list" rl "") in
    let styp = STquo _loc "a_list" in
    {used = used; text = text; styp = styp; pattern = None}
  ;

  value ssopt _loc s =
    let rl =
      let r1 =
        let prod =
          let n = mk_name _loc <:ident< a_opt >> in
          [mk_symbol <:patt< a >> (TXnterm _loc n None) (STquo _loc "a_opt")]
        in
        let act = <:expr< a >> in
        {prod = prod; action = Some act}
      in
      let r2 =
        let s =
          match s.text with
          [ TXkwd _loc _ | TXtok _loc _ _ ->
              let rl =
                [{prod = [{ (s) with pattern = Some <:patt< x >> }];
                  action = Some <:expr< Qast.Str (Token.extract_string x) >>}]
              in
              let t = new_type_var () in
              {used = []; text = TXrules _loc (srules _loc t rl "");
              styp = STquo _loc t; pattern = None}
          | _ -> s ]
        in
        let prod =
          [mk_symbol <:patt< a >> (TXopt _loc s.text)
            (STapp _loc (STlid _loc "option") s.styp)]
        in
        let act = <:expr< Qast.Option a >> in
        {prod = prod; action = Some act}
      in
      [r1; r2]
    in
    let used = ["a_opt" :: s.used] in
    let text = TXrules _loc (srules _loc "a_opt" rl "") in
    let styp = STquo _loc "a_opt" in
    {used = used; text = text; styp = styp; pattern = None}
  ;
  *)
        let text_of_entry _loc e =
          let ent =
            let x = e.name in
            let _loc = e.name.loc
            in
              Ast.ExTyc (_loc, x.expr,
                (Ast.TyApp (_loc,
                   (Ast.TyId (_loc,
                      (Ast.IdAcc (_loc,
                         (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                            (Ast.IdUid (_loc, "Entry")))),
                         (Ast.IdLid (_loc, "t")))))),
                   (Ast.TyQuo (_loc, x.tvar))))) in
          let pos =
            match e.pos with
            | Some pos ->
                Ast.ExApp (_loc,
                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))), pos)
            | None -> Ast.ExId (_loc, (Ast.IdUid (_loc, "None"))) in
          let txt =
            List.fold_right
              (fun level txt ->
                 let lab =
                   match level.label with
                   | Some lab ->
                       Ast.ExApp (_loc,
                         (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))),
                         (Ast.ExStr (_loc, lab)))
                   | None -> Ast.ExId (_loc, (Ast.IdUid (_loc, "None"))) in
                 let ass =
                   match level.assoc with
                   | Some ass ->
                       Ast.ExApp (_loc,
                         (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))), ass)
                   | None -> Ast.ExId (_loc, (Ast.IdUid (_loc, "None"))) in
                 let txt =
                   let rl =
                     srules _loc e.name.tvar level.rules e.name.tvar in
                   let e = make_expr_rules _loc e.name rl e.name.tvar
                   in
                     Ast.ExApp (_loc,
                       (Ast.ExApp (_loc,
                          (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                          (Ast.ExTup (_loc,
                             (Ast.ExCom (_loc, lab,
                                (Ast.ExCom (_loc, ass, e)))))))),
                       txt)
                 in txt)
              e.levels (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))
          in (ent, pos, txt)
          
        let let_in_of_extend _loc gram gl el args =
          match gl with
          | None -> args
          | Some nl ->
              (check_use nl el;
               let ll =
                 let same_tvar e n = e.name.tvar = n.tvar
                 in
                   List.fold_right
                     (fun e ll ->
                        match e.name.expr with
                        | Ast.ExId (_, (Ast.IdLid (_, _))) ->
                            if List.exists (same_tvar e) nl
                            then ll
                            else
                              if List.exists (same_tvar e) ll
                              then ll
                              else e.name :: ll
                        | _ -> ll)
                     el [] in
               let local_binding_of_name { expr = e; tvar = x; loc = _loc } =
                 let i =
                   (match e with
                    | Ast.ExId (_, (Ast.IdLid (_, i))) -> i
                    | _ -> failwith "internal error in the Grammar extension")
                 in
                   Ast.BiEq (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, i)))),
                     (Ast.ExTyc (_loc,
                        (Ast.ExApp (_loc,
                           (Ast.ExId (_loc,
                              (Ast.IdLid (_loc, "grammar_entry_create")))),
                           (Ast.ExStr (_loc, i)))),
                        (Ast.TyApp (_loc,
                           (Ast.TyId (_loc,
                              (Ast.IdAcc (_loc,
                                 (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                    (Ast.IdUid (_loc, "Entry")))),
                                 (Ast.IdLid (_loc, "t")))))),
                           (Ast.TyQuo (_loc, x))))))) in
               let expr_of_name { expr = e; tvar = x; loc = _loc } =
                 Ast.ExTyc (_loc, e,
                   (Ast.TyApp (_loc,
                      (Ast.TyId (_loc,
                         (Ast.IdAcc (_loc,
                            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                               (Ast.IdUid (_loc, "Entry")))),
                            (Ast.IdLid (_loc, "t")))))),
                      (Ast.TyQuo (_loc, x))))) in
               let e =
                 (match ll with
                  | [] -> args
                  | x :: xs ->
                      let locals =
                        List.fold_right
                          (fun name acc ->
                             Ast.BiAnd (_loc, acc,
                               (local_binding_of_name name)))
                          xs (local_binding_of_name x) in
                      let entry_mk =
                        (match gram with
                         | Some g ->
                             Ast.ExApp (_loc,
                               (Ast.ExId (_loc,
                                  (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                     (Ast.IdAcc (_loc,
                                        (Ast.IdUid (_loc, "Entry")),
                                        (Ast.IdLid (_loc, "mk")))))))),
                               (Ast.ExId (_loc, g)))
                         | None ->
                             Ast.ExId (_loc,
                               (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                  (Ast.IdAcc (_loc,
                                     (Ast.IdUid (_loc, "Entry")),
                                     (Ast.IdLid (_loc, "mk"))))))))
                      in
                        Ast.ExLet (_loc, Ast.ReNil,
                          (Ast.BiEq (_loc,
                             (Ast.PaId (_loc,
                                (Ast.IdLid (_loc, "grammar_entry_create")))),
                             entry_mk)),
                          (Ast.ExLet (_loc, Ast.ReNil, locals, args))))
               in
                 (match nl with
                  | [] -> e
                  | x :: xs ->
                      let globals =
                        List.fold_right
                          (fun name acc ->
                             Ast.BiAnd (_loc, acc,
                               (Ast.BiEq (_loc, (Ast.PaAny _loc),
                                  (expr_of_name name)))))
                          xs
                          (Ast.BiEq (_loc, (Ast.PaAny _loc),
                             (expr_of_name x)))
                      in Ast.ExLet (_loc, Ast.ReNil, globals, e)))
          
        class subst gmod =
          object inherit Ast.map as super
            method ident =
              function
              | Ast.IdUid (_, x) when x = gm -> gmod
              | x -> super#ident x
          end
          
        let subst_gmod ast gmod = (new subst gmod)#expr ast
          
        let text_of_functorial_extend _loc gmod gram gl el =
          let args =
            let el =
              List.map
                (fun e ->
                   let (ent, pos, txt) = text_of_entry e.name.loc e in
                   let e =
                     Ast.ExApp (_loc,
                       (Ast.ExApp (_loc,
                          (Ast.ExId (_loc,
                             (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                                (Ast.IdLid (_loc, "extend")))))),
                          ent)),
                       (Ast.ExApp (_loc,
                          (Ast.ExFun (_loc,
                             (Ast.McArr (_loc,
                                (Ast.PaId (_loc, (Ast.IdUid (_loc, "()")))),
                                (Ast.ExNil _loc),
                                (Ast.ExTup (_loc,
                                   (Ast.ExCom (_loc, pos, txt)))))))),
                          (Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))))))
                   in
                     if !split_ext
                     then
                       Ast.ExLet (_loc, Ast.ReNil,
                         (Ast.BiEq (_loc,
                            (Ast.PaId (_loc, (Ast.IdLid (_loc, "aux")))),
                            (Ast.ExFun (_loc,
                               (Ast.McArr (_loc,
                                  (Ast.PaId (_loc, (Ast.IdUid (_loc, "()")))),
                                  (Ast.ExNil _loc), e)))))),
                         (Ast.ExApp (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "aux")))),
                            (Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))))))
                     else e)
                el
            in
              match el with
              | [] -> Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))
              | [ e ] -> e
              | e :: el ->
                  Ast.ExSeq (_loc,
                    (List.fold_left (fun acc x -> Ast.ExSem (_loc, acc, x)) e
                       el))
          in subst_gmod (let_in_of_extend _loc gram gl el args) gmod
          
        let wildcarder =
          object (self)
            inherit Ast.map as super
            method patt =
              function
              | Ast.PaId (_loc, (Ast.IdLid (_, _))) -> Ast.PaAny _loc
              | Ast.PaAli (_, p, _) -> self#patt p
              | p -> super#patt p
          end
          
        let mk_tok _loc p t =
          let p' = wildcarder#patt p in
          let match_fun =
            if Ast.is_irrefut_patt p'
            then
              Ast.ExFun (_loc,
                (Ast.McArr (_loc, p', (Ast.ExNil _loc),
                   (Ast.ExId (_loc, (Ast.IdUid (_loc, "True")))))))
            else
              Ast.ExFun (_loc,
                (Ast.McOr (_loc,
                   (Ast.McArr (_loc, p', (Ast.ExNil _loc),
                      (Ast.ExId (_loc, (Ast.IdUid (_loc, "True")))))),
                   (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                      (Ast.ExId (_loc, (Ast.IdUid (_loc, "False"))))))))) in
          let descr = string_of_patt p' in
          let text = TXtok (_loc, match_fun, descr)
          in { used = []; text = text; styp = t; pattern = Some p; }
          
        let symbol = Gram.Entry.mk "symbol"
          
        let check_not_tok s =
          match s with
          | { text = TXtok (_loc, _, _) } ->
              Loc.raise _loc
                (Stream.Error
                   ("Deprecated syntax, use a sub rule. " ^
                      "LIST0 STRING becomes LIST0 [ x = STRING -> x ]"))
          | _ -> ()
          
        let _ = Camlp4_config.antiquotations := true
          
        let _ =
          let _ = (expr : 'expr Gram.Entry.t)
          and _ = (symbol : 'symbol Gram.Entry.t) in
          let grammar_entry_create = Gram.Entry.mk in
          let extend_header : 'extend_header Gram.Entry.t =
            grammar_entry_create "extend_header"
          and semi_sep : 'semi_sep Gram.Entry.t =
            grammar_entry_create "semi_sep"
          and string : 'string Gram.Entry.t = grammar_entry_create "string"
          and name : 'name Gram.Entry.t = grammar_entry_create "name"
          and comma_patt : 'comma_patt Gram.Entry.t =
            grammar_entry_create "comma_patt"
          and pattern : 'pattern Gram.Entry.t =
            grammar_entry_create "pattern"
          and psymbol : 'psymbol Gram.Entry.t =
            grammar_entry_create "psymbol"
          and rule : 'rule Gram.Entry.t = grammar_entry_create "rule"
          and rule_list : 'rule_list Gram.Entry.t =
            grammar_entry_create "rule_list"
          and assoc : 'assoc Gram.Entry.t = grammar_entry_create "assoc"
          and level : 'level Gram.Entry.t = grammar_entry_create "level"
          and level_list : 'level_list Gram.Entry.t =
            grammar_entry_create "level_list"
          and position : 'position Gram.Entry.t =
            grammar_entry_create "position"
          and entry : 'entry Gram.Entry.t = grammar_entry_create "entry"
          and global : 'global Gram.Entry.t = grammar_entry_create "global"
          and t_qualid : 't_qualid Gram.Entry.t =
            grammar_entry_create "t_qualid"
          and qualid : 'qualid Gram.Entry.t = grammar_entry_create "qualid"
          and qualuid : 'qualuid Gram.Entry.t =
            grammar_entry_create "qualuid"
          and delete_rule_body : 'delete_rule_body Gram.Entry.t =
            grammar_entry_create "delete_rule_body"
          and extend_body : 'extend_body Gram.Entry.t =
            grammar_entry_create "extend_body"
          in
            (Gram.extend (expr : 'expr Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.After "top")),
                    [ (None, None,
                       [ ([ Gram.Skeyword "GEXTEND" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (Loc.raise _loc
                                   (Stream.Error
                                      "Deprecated syntax, use EXTEND MyGramModule ... END instead") :
                                  'expr))));
                         ([ Gram.Skeyword "GDELETE_RULE" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (Loc.raise _loc
                                   (Stream.Error
                                      "Deprecated syntax, use DELETE_RULE MyGramModule ... END instead") :
                                  'expr))));
                         ([ Gram.Skeyword "DELETE_RULE";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (delete_rule_body :
                                   'delete_rule_body Gram.Entry.t));
                            Gram.Skeyword "END" ],
                          (Gram.Action.mk
                             (fun _ (e : 'delete_rule_body) _
                                (_loc : Gram.Loc.t) -> (e : 'expr))));
                         ([ Gram.Skeyword "EXTEND";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (extend_body : 'extend_body Gram.Entry.t));
                            Gram.Skeyword "END" ],
                          (Gram.Action.mk
                             (fun _ (e : 'extend_body) _ (_loc : Gram.Loc.t)
                                -> (e : 'expr)))) ]) ]))
                  ());
             Gram.extend (extend_header : 'extend_header Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (qualuid : 'qualuid Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (g : 'qualuid) (_loc : Gram.Loc.t) ->
                                ((None, g) : 'extend_header))));
                         ([ Gram.Skeyword "(";
                            Gram.Snterm
                              (Gram.Entry.obj (qualid : 'qualid Gram.Entry.t));
                            Gram.Skeyword ":";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (t_qualid : 't_qualid Gram.Entry.t));
                            Gram.Skeyword ")" ],
                          (Gram.Action.mk
                             (fun _ (t : 't_qualid) _ (i : 'qualid) _
                                (_loc : Gram.Loc.t) ->
                                (((Some i), t) : 'extend_header)))) ]) ]))
                  ());
             Gram.extend (extend_body : 'extend_body Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (extend_header :
                                   'extend_header Gram.Entry.t));
                            Gram.Sopt
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (global : 'global Gram.Entry.t)));
                            Gram.Slist1
                              (Gram.srules extend_body
                                 [ ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (entry : 'entry Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi_sep :
                                             'semi_sep Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (e : 'entry)
                                          (_loc : Gram.Loc.t) -> (e : 'e__16)))) ]) ],
                          (Gram.Action.mk
                             (fun (el : 'e__16 list)
                                (global_list : 'global option)
                                ((gram, g) : 'extend_header)
                                (_loc : Gram.Loc.t) ->
                                (text_of_functorial_extend _loc g gram
                                   global_list el :
                                  'extend_body)))) ]) ]))
                  ());
             Gram.extend (delete_rule_body : 'delete_rule_body Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (qualuid : 'qualuid Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj (name : 'name Gram.Entry.t));
                            Gram.Skeyword ":";
                            Gram.Slist0sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj
                                     (symbol : 'symbol Gram.Entry.t))),
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (semi_sep : 'semi_sep Gram.Entry.t)))) ],
                          (Gram.Action.mk
                             (fun (sl : 'symbol list) _ (n : 'name)
                                (g : 'qualuid) (_loc : Gram.Loc.t) ->
                                (let (e, b) = expr_of_delete_rule _loc n sl
                                 in
                                   subst_gmod
                                     (Ast.ExApp (_loc,
                                        (Ast.ExApp (_loc,
                                           (Ast.ExId (_loc,
                                              (Ast.IdAcc (_loc,
                                                 (Ast.IdUid (_loc, gm)),
                                                 (Ast.IdLid (_loc,
                                                    "delete_rule")))))),
                                           e)),
                                        b))
                                     g :
                                  'delete_rule_body)))) ]) ]))
                  ());
             Gram.extend (qualuid : 'qualuid Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.srules qualuid
                              [ ([ Gram.Stoken
                                     (((function
                                        | UIDENT "GLOBAL" -> true
                                        | _ -> false),
                                       "UIDENT \"GLOBAL\"")) ],
                                 (Gram.Action.mk
                                    (fun (__camlp4_0 : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       match __camlp4_0 with
                                       | UIDENT "GLOBAL" -> (() : 'e__17)
                                       | _ -> assert false)));
                                ([ Gram.Stoken
                                     (((function
                                        | LIDENT ((_)) -> true
                                        | _ -> false),
                                       "LIDENT ((_))")) ],
                                 (Gram.Action.mk
                                    (fun (__camlp4_0 : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       match __camlp4_0 with
                                       | LIDENT ((_)) -> (() : 'e__17)
                                       | _ -> assert false))) ] ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (Loc.raise _loc
                                   (Stream.Error
                                      "Deprecated syntax, the grammar module is expected") :
                                  'qualuid)))) ]);
                      (None, None,
                       [ ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _")) ],
                          (Gram.Action.mk
                             (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i
                                 in Ast.IdUid (_loc, i) : 'qualuid))));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Skeyword "."; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (xs : 'qualuid) _ (x : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                (let x = Gram.Token.extract_string x
                                 in
                                   Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)),
                                     xs) :
                                  'qualuid)))) ]) ]))
                  ());
             Gram.extend (qualuid : 'qualuid Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.srules qualuid
                              [ ([ Gram.Stoken
                                     (((function
                                        | UIDENT "GLOBAL" -> true
                                        | _ -> false),
                                       "UIDENT \"GLOBAL\"")) ],
                                 (Gram.Action.mk
                                    (fun (__camlp4_0 : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       match __camlp4_0 with
                                       | UIDENT "GLOBAL" -> (() : 'e__18)
                                       | _ -> assert false)));
                                ([ Gram.Stoken
                                     (((function
                                        | LIDENT ((_)) -> true
                                        | _ -> false),
                                       "LIDENT ((_))")) ],
                                 (Gram.Action.mk
                                    (fun (__camlp4_0 : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       match __camlp4_0 with
                                       | LIDENT ((_)) -> (() : 'e__18)
                                       | _ -> assert false))) ] ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (Loc.raise _loc
                                   (Stream.Error
                                      "Deprecated syntax, the grammar module is expected") :
                                  'qualuid)))) ]);
                      (None, None,
                       [ ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _")) ],
                          (Gram.Action.mk
                             (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i
                                 in Ast.IdUid (_loc, i) : 'qualuid))));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Skeyword "."; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (xs : 'qualuid) _ (x : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                (let x = Gram.Token.extract_string x
                                 in
                                   Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)),
                                     xs) :
                                  'qualuid)))) ]) ]))
                  ());
             Gram.extend (qualid : 'qualid Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function | LIDENT ((_)) -> true | _ -> false),
                                "LIDENT _")) ],
                          (Gram.Action.mk
                             (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i
                                 in Ast.IdLid (_loc, i) : 'qualid))));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _")) ],
                          (Gram.Action.mk
                             (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i
                                 in Ast.IdUid (_loc, i) : 'qualid))));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Skeyword "."; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (xs : 'qualid) _ (x : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                (let x = Gram.Token.extract_string x
                                 in
                                   Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)),
                                     xs) :
                                  'qualid)))) ]) ]))
                  ());
             Gram.extend (t_qualid : 't_qualid Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function
                                 | LIDENT _ | UIDENT _ -> true
                                 | _ -> false),
                                "LIDENT _ | UIDENT _")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | LIDENT _ | UIDENT _ ->
                                    (Loc.raise _loc
                                       (Stream.Error
                                          ("Wrong EXTEND header, the grammar type must finish by 't', "
                                             ^
                                             "like in EXTEND (g : Gram.t) ... END")) :
                                      't_qualid)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Skeyword ".";
                            Gram.Stoken
                              (((function | LIDENT "t" -> true | _ -> false),
                                "LIDENT \"t\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t) _
                                (x : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | LIDENT "t" ->
                                    (let x = Gram.Token.extract_string x
                                     in Ast.IdUid (_loc, x) : 't_qualid)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Skeyword "."; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (xs : 't_qualid) _ (x : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                (let x = Gram.Token.extract_string x
                                 in
                                   Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)),
                                     xs) :
                                  't_qualid)))) ]) ]))
                  ());
             Gram.extend (global : 'global Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function
                                 | UIDENT "GLOBAL" -> true
                                 | _ -> false),
                                "UIDENT \"GLOBAL\""));
                            Gram.Skeyword ":";
                            Gram.Slist1
                              (Gram.Snterm
                                 (Gram.Entry.obj (name : 'name Gram.Entry.t)));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (semi_sep : 'semi_sep Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (sl : 'name list) _
                                (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "GLOBAL" -> (sl : 'global)
                                | _ -> assert false))) ]) ]))
                  ());
             Gram.extend (entry : 'entry Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (name : 'name Gram.Entry.t));
                            Gram.Skeyword ":";
                            Gram.Sopt
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (position : 'position Gram.Entry.t)));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (level_list : 'level_list Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (ll : 'level_list) (pos : 'position option)
                                _ (n : 'name) (_loc : Gram.Loc.t) ->
                                ({ name = n; pos = pos; levels = ll; } :
                                  'entry)))) ]) ]))
                  ());
             Gram.extend (position : 'position Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function
                                 | UIDENT "LEVEL" -> true
                                 | _ -> false),
                                "UIDENT \"LEVEL\""));
                            Gram.Snterm
                              (Gram.Entry.obj (string : 'string Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (n : 'string) (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "LEVEL" ->
                                    (Ast.ExApp (_loc,
                                       (Ast.ExId (_loc,
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Camlp4")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Sig")),
                                                (Ast.IdAcc (_loc,
                                                   (Ast.IdUid (_loc,
                                                      "Grammar")),
                                                   (Ast.IdUid (_loc, "Level")))))))))),
                                       n) :
                                      'position)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "AFTER" -> true
                                 | _ -> false),
                                "UIDENT \"AFTER\""));
                            Gram.Snterm
                              (Gram.Entry.obj (string : 'string Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (n : 'string) (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "AFTER" ->
                                    (Ast.ExApp (_loc,
                                       (Ast.ExId (_loc,
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Camlp4")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Sig")),
                                                (Ast.IdAcc (_loc,
                                                   (Ast.IdUid (_loc,
                                                      "Grammar")),
                                                   (Ast.IdUid (_loc, "After")))))))))),
                                       n) :
                                      'position)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "BEFORE" -> true
                                 | _ -> false),
                                "UIDENT \"BEFORE\""));
                            Gram.Snterm
                              (Gram.Entry.obj (string : 'string Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (n : 'string) (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "BEFORE" ->
                                    (Ast.ExApp (_loc,
                                       (Ast.ExId (_loc,
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Camlp4")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Sig")),
                                                (Ast.IdAcc (_loc,
                                                   (Ast.IdUid (_loc,
                                                      "Grammar")),
                                                   (Ast.IdUid (_loc,
                                                      "Before")))))))))),
                                       n) :
                                      'position)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "LAST" -> true
                                 | _ -> false),
                                "UIDENT \"LAST\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "LAST" ->
                                    (Ast.ExId (_loc,
                                       (Ast.IdAcc (_loc,
                                          (Ast.IdUid (_loc, "Camlp4")),
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Sig")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Grammar")),
                                                (Ast.IdUid (_loc, "Last"))))))))) :
                                      'position)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "FIRST" -> true
                                 | _ -> false),
                                "UIDENT \"FIRST\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "FIRST" ->
                                    (Ast.ExId (_loc,
                                       (Ast.IdAcc (_loc,
                                          (Ast.IdUid (_loc, "Camlp4")),
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Sig")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Grammar")),
                                                (Ast.IdUid (_loc, "First"))))))))) :
                                      'position)
                                | _ -> assert false))) ]) ]))
                  ());
             Gram.extend (level_list : 'level_list Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "[";
                            Gram.Slist0sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj
                                     (level : 'level Gram.Entry.t))),
                              (Gram.Skeyword "|"));
                            Gram.Skeyword "]" ],
                          (Gram.Action.mk
                             (fun _ (ll : 'level list) _ (_loc : Gram.Loc.t)
                                -> (ll : 'level_list)))) ]) ]))
                  ());
             Gram.extend (level : 'level Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Sopt
                              (Gram.srules level
                                 [ ([ Gram.Stoken
                                        (((function
                                           | STRING ((_)) -> true
                                           | _ -> false),
                                          "STRING _")) ],
                                    (Gram.Action.mk
                                       (fun (x : Gram.Token.t)
                                          (_loc : Gram.Loc.t) ->
                                          (let x =
                                             Gram.Token.extract_string x
                                           in x : 'e__19)))) ]);
                            Gram.Sopt
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (assoc : 'assoc Gram.Entry.t)));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (rule_list : 'rule_list Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (rules : 'rule_list) (ass : 'assoc option)
                                (lab : 'e__19 option) (_loc : Gram.Loc.t) ->
                                ({ label = lab; assoc = ass; rules = rules; } :
                                  'level)))) ]) ]))
                  ());
             Gram.extend (assoc : 'assoc Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function
                                 | UIDENT "NONA" -> true
                                 | _ -> false),
                                "UIDENT \"NONA\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "NONA" ->
                                    (Ast.ExId (_loc,
                                       (Ast.IdAcc (_loc,
                                          (Ast.IdUid (_loc, "Camlp4")),
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Sig")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Grammar")),
                                                (Ast.IdUid (_loc, "NonA"))))))))) :
                                      'assoc)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "RIGHTA" -> true
                                 | _ -> false),
                                "UIDENT \"RIGHTA\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "RIGHTA" ->
                                    (Ast.ExId (_loc,
                                       (Ast.IdAcc (_loc,
                                          (Ast.IdUid (_loc, "Camlp4")),
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Sig")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Grammar")),
                                                (Ast.IdUid (_loc, "RightA"))))))))) :
                                      'assoc)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "LEFTA" -> true
                                 | _ -> false),
                                "UIDENT \"LEFTA\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "LEFTA" ->
                                    (Ast.ExId (_loc,
                                       (Ast.IdAcc (_loc,
                                          (Ast.IdUid (_loc, "Camlp4")),
                                          (Ast.IdAcc (_loc,
                                             (Ast.IdUid (_loc, "Sig")),
                                             (Ast.IdAcc (_loc,
                                                (Ast.IdUid (_loc, "Grammar")),
                                                (Ast.IdUid (_loc, "LeftA"))))))))) :
                                      'assoc)
                                | _ -> assert false))) ]) ]))
                  ());
             Gram.extend (rule_list : 'rule_list Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "[";
                            Gram.Slist1sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj (rule : 'rule Gram.Entry.t))),
                              (Gram.Skeyword "|"));
                            Gram.Skeyword "]" ],
                          (Gram.Action.mk
                             (fun _ (rules : 'rule list) _
                                (_loc : Gram.Loc.t) ->
                                (retype_rule_list_without_patterns _loc rules :
                                  'rule_list))));
                         ([ Gram.Skeyword "["; Gram.Skeyword "]" ],
                          (Gram.Action.mk
                             (fun _ _ (_loc : Gram.Loc.t) ->
                                ([] : 'rule_list)))) ]) ]))
                  ());
             Gram.extend (rule : 'rule Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Slist0sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj
                                     (psymbol : 'psymbol Gram.Entry.t))),
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (semi_sep : 'semi_sep Gram.Entry.t)))) ],
                          (Gram.Action.mk
                             (fun (psl : 'psymbol list) (_loc : Gram.Loc.t)
                                -> ({ prod = psl; action = None; } : 'rule))));
                         ([ Gram.Slist0sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj
                                     (psymbol : 'psymbol Gram.Entry.t))),
                              (Gram.Snterm
                                 (Gram.Entry.obj
                                    (semi_sep : 'semi_sep Gram.Entry.t))));
                            Gram.Skeyword "->";
                            Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (act : 'expr) _ (psl : 'psymbol list)
                                (_loc : Gram.Loc.t) ->
                                ({ prod = psl; action = Some act; } : 'rule)))) ]) ]))
                  ());
             Gram.extend (psymbol : 'psymbol Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (symbol : 'symbol Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (s : 'symbol) (_loc : Gram.Loc.t) ->
                                (s : 'psymbol))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (pattern : 'pattern Gram.Entry.t));
                            Gram.Skeyword "=";
                            Gram.Snterm
                              (Gram.Entry.obj (symbol : 'symbol Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (s : 'symbol) _ (p : 'pattern)
                                (_loc : Gram.Loc.t) ->
                                (match s.pattern with
                                 | Some
                                     (Ast.PaApp (_,
                                        (Ast.PaId (_, (Ast.IdUid (_, u)))),
                                        (Ast.PaTup (_, (Ast.PaAny _)))))
                                     ->
                                     mk_tok _loc
                                       (Ast.PaApp (_loc,
                                          (Ast.PaId (_loc,
                                             (Ast.IdUid (_loc, u)))),
                                          p))
                                       s.styp
                                 | _ -> { (s) with pattern = Some p; } :
                                  'psymbol))));
                         ([ Gram.Stoken
                              (((function | LIDENT ((_)) -> true | _ -> false),
                                "LIDENT _"));
                            Gram.Sopt
                              (Gram.srules psymbol
                                 [ ([ Gram.Stoken
                                        (((function
                                           | UIDENT "LEVEL" -> true
                                           | _ -> false),
                                          "UIDENT \"LEVEL\""));
                                      Gram.Stoken
                                        (((function
                                           | STRING ((_)) -> true
                                           | _ -> false),
                                          "STRING _")) ],
                                    (Gram.Action.mk
                                       (fun (s : Gram.Token.t)
                                          (__camlp4_0 : Gram.Token.t)
                                          (_loc : Gram.Loc.t) ->
                                          match __camlp4_0 with
                                          | UIDENT "LEVEL" ->
                                              (let s =
                                                 Gram.Token.extract_string s
                                               in s : 'e__20)
                                          | _ -> assert false))) ]) ],
                          (Gram.Action.mk
                             (fun (lev : 'e__20 option) (i : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i in
                                 let name =
                                   mk_name _loc (Ast.IdLid (_loc, i)) in
                                 let text = TXnterm (_loc, name, lev) in
                                 let styp = STquo (_loc, i)
                                 in
                                   {
                                     used = [ i ];
                                     text = text;
                                     styp = styp;
                                     pattern = None;
                                   } :
                                  'psymbol))));
                         ([ Gram.Stoken
                              (((function | LIDENT ((_)) -> true | _ -> false),
                                "LIDENT _"));
                            Gram.Skeyword "=";
                            Gram.Snterm
                              (Gram.Entry.obj (symbol : 'symbol Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (s : 'symbol) _ (p : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                (let p = Gram.Token.extract_string p
                                 in
                                   match s.pattern with
                                   | Some
                                       ((Ast.PaApp (_,
                                           (Ast.PaId (_, (Ast.IdUid (_, u)))),
                                           (Ast.PaTup (_, (Ast.PaAny _))))
                                         as p'))
                                       ->
                                       let match_fun =
                                         Ast.ExFun (_loc,
                                           (Ast.McOr (_loc,
                                              (Ast.McArr (_loc, p',
                                                 (Ast.ExNil _loc),
                                                 (Ast.ExId (_loc,
                                                    (Ast.IdUid (_loc, "True")))))),
                                              (Ast.McArr (_loc,
                                                 (Ast.PaAny _loc),
                                                 (Ast.ExNil _loc),
                                                 (Ast.ExId (_loc,
                                                    (Ast.IdUid (_loc,
                                                       "False"))))))))) in
                                       let p' =
                                         Ast.PaAli (_loc, p',
                                           (Ast.PaId (_loc,
                                              (Ast.IdLid (_loc, p))))) in
                                       let descr = u ^ " _" in
                                       let text =
                                         TXtok (_loc, match_fun, descr)
                                       in
                                         {
                                           (s)
                                           with
                                           text = text;
                                           pattern = Some p';
                                         }
                                   | _ ->
                                       {
                                         (s)
                                         with
                                         pattern =
                                           Some
                                             (Ast.PaId (_loc,
                                                (Ast.IdLid (_loc, p))));
                                       } :
                                  'psymbol)))) ]) ]))
                  ());
             Gram.extend (symbol : 'symbol Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ ((Some "top"), (Some Camlp4.Sig.Grammar.NonA),
                       [ ([ Gram.Stoken
                              (((function | UIDENT "TRY" -> true | _ -> false),
                                "UIDENT \"TRY\""));
                            Gram.Sself ],
                          (Gram.Action.mk
                             (fun (s : 'symbol) (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "TRY" ->
                                    (let text = TXtry (_loc, s.text)
                                     in
                                       {
                                         used = s.used;
                                         text = text;
                                         styp = s.styp;
                                         pattern = None;
                                       } :
                                      'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function | UIDENT "OPT" -> true | _ -> false),
                                "UIDENT \"OPT\""));
                            Gram.Sself ],
                          (Gram.Action.mk
                             (fun (s : 'symbol) (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "OPT" ->
                                    (let () = check_not_tok s in
                                     let styp =
                                       STapp (_loc, (STlid (_loc, "option")),
                                         s.styp) in
                                     let text = TXopt (_loc, s.text)
                                     in
                                       {
                                         used = s.used;
                                         text = text;
                                         styp = styp;
                                         pattern = None;
                                       } :
                                      'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "LIST1" -> true
                                 | _ -> false),
                                "UIDENT \"LIST1\""));
                            Gram.Sself;
                            Gram.Sopt
                              (Gram.srules symbol
                                 [ ([ Gram.Stoken
                                        (((function
                                           | UIDENT "SEP" -> true
                                           | _ -> false),
                                          "UIDENT \"SEP\""));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (symbol : 'symbol Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun (t : 'symbol)
                                          (__camlp4_0 : Gram.Token.t)
                                          (_loc : Gram.Loc.t) ->
                                          match __camlp4_0 with
                                          | UIDENT "SEP" -> (t : 'e__22)
                                          | _ -> assert false))) ]) ],
                          (Gram.Action.mk
                             (fun (sep : 'e__22 option) (s : 'symbol)
                                (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "LIST1" ->
                                    (let () = check_not_tok s in
                                     let used =
                                       (match sep with
                                        | Some symb -> symb.used @ s.used
                                        | None -> s.used) in
                                     let styp =
                                       STapp (_loc, (STlid (_loc, "list")),
                                         s.styp) in
                                     let text = slist _loc true sep s
                                     in
                                       {
                                         used = used;
                                         text = text;
                                         styp = styp;
                                         pattern = None;
                                       } :
                                      'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "LIST0" -> true
                                 | _ -> false),
                                "UIDENT \"LIST0\""));
                            Gram.Sself;
                            Gram.Sopt
                              (Gram.srules symbol
                                 [ ([ Gram.Stoken
                                        (((function
                                           | UIDENT "SEP" -> true
                                           | _ -> false),
                                          "UIDENT \"SEP\""));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (symbol : 'symbol Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun (t : 'symbol)
                                          (__camlp4_0 : Gram.Token.t)
                                          (_loc : Gram.Loc.t) ->
                                          match __camlp4_0 with
                                          | UIDENT "SEP" -> (t : 'e__21)
                                          | _ -> assert false))) ]) ],
                          (Gram.Action.mk
                             (fun (sep : 'e__21 option) (s : 'symbol)
                                (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "LIST0" ->
                                    (let () = check_not_tok s in
                                     let used =
                                       (match sep with
                                        | Some symb -> symb.used @ s.used
                                        | None -> s.used) in
                                     let styp =
                                       STapp (_loc, (STlid (_loc, "list")),
                                         s.styp) in
                                     let text = slist _loc false sep s
                                     in
                                       {
                                         used = used;
                                         text = text;
                                         styp = styp;
                                         pattern = None;
                                       } :
                                      'symbol)
                                | _ -> assert false))) ]);
                      (None, None,
                       [ ([ Gram.Skeyword "("; Gram.Sself; Gram.Skeyword ")" ],
                          (Gram.Action.mk
                             (fun _ (s_t : 'symbol) _ (_loc : Gram.Loc.t) ->
                                (s_t : 'symbol))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj (name : 'name Gram.Entry.t));
                            Gram.Sopt
                              (Gram.srules symbol
                                 [ ([ Gram.Stoken
                                        (((function
                                           | UIDENT "LEVEL" -> true
                                           | _ -> false),
                                          "UIDENT \"LEVEL\""));
                                      Gram.Stoken
                                        (((function
                                           | STRING ((_)) -> true
                                           | _ -> false),
                                          "STRING _")) ],
                                    (Gram.Action.mk
                                       (fun (s : Gram.Token.t)
                                          (__camlp4_0 : Gram.Token.t)
                                          (_loc : Gram.Loc.t) ->
                                          match __camlp4_0 with
                                          | UIDENT "LEVEL" ->
                                              (let s =
                                                 Gram.Token.extract_string s
                                               in s : 'e__24)
                                          | _ -> assert false))) ]) ],
                          (Gram.Action.mk
                             (fun (lev : 'e__24 option) (n : 'name)
                                (_loc : Gram.Loc.t) ->
                                ({
                                   used = [ n.tvar ];
                                   text = TXnterm (_loc, n, lev);
                                   styp = STquo (_loc, n.tvar);
                                   pattern = None;
                                 } : 'symbol))));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Skeyword ".";
                            Gram.Snterm
                              (Gram.Entry.obj (qualid : 'qualid Gram.Entry.t));
                            Gram.Sopt
                              (Gram.srules symbol
                                 [ ([ Gram.Stoken
                                        (((function
                                           | UIDENT "LEVEL" -> true
                                           | _ -> false),
                                          "UIDENT \"LEVEL\""));
                                      Gram.Stoken
                                        (((function
                                           | STRING ((_)) -> true
                                           | _ -> false),
                                          "STRING _")) ],
                                    (Gram.Action.mk
                                       (fun (s : Gram.Token.t)
                                          (__camlp4_0 : Gram.Token.t)
                                          (_loc : Gram.Loc.t) ->
                                          match __camlp4_0 with
                                          | UIDENT "LEVEL" ->
                                              (let s =
                                                 Gram.Token.extract_string s
                                               in s : 'e__23)
                                          | _ -> assert false))) ]) ],
                          (Gram.Action.mk
                             (fun (lev : 'e__23 option) (il : 'qualid) _
                                (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i in
                                 let n =
                                   mk_name _loc
                                     (Ast.IdAcc (_loc, (Ast.IdUid (_loc, i)),
                                        il))
                                 in
                                   {
                                     used = [ n.tvar ];
                                     text = TXnterm (_loc, n, lev);
                                     styp = STquo (_loc, n.tvar);
                                     pattern = None;
                                   } :
                                  'symbol))));
                         ([ Gram.Stoken
                              (((function | STRING ((_)) -> true | _ -> false),
                                "STRING _")) ],
                          (Gram.Action.mk
                             (fun (s : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let s = Gram.Token.extract_string s
                                 in
                                   {
                                     used = [];
                                     text = TXkwd (_loc, s);
                                     styp = STtok _loc;
                                     pattern = None;
                                   } :
                                  'symbol))));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Stoken
                              (((function
                                 | ANTIQUOT ("", _) -> true
                                 | _ -> false),
                                "ANTIQUOT (\"\", _)")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (x : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | ANTIQUOT ("", s) ->
                                    (let x = Gram.Token.extract_string x in
                                     let e =
                                       AntiquotSyntax.parse_expr _loc s in
                                     let match_fun =
                                       Ast.ExFun (_loc,
                                         (Ast.McOr (_loc,
                                            (Ast.McArr (_loc,
                                               (Ast.PaApp (_loc,
                                                  (Ast.PaId (_loc,
                                                     (Ast.IdUid (_loc, x)))),
                                                  (Ast.PaId (_loc,
                                                     (Ast.IdLid (_loc,
                                                        "camlp4_x")))))),
                                               (Ast.ExApp (_loc,
                                                  (Ast.ExApp (_loc,
                                                     (Ast.ExId (_loc,
                                                        (Ast.IdLid (_loc,
                                                           "=")))),
                                                     (Ast.ExId (_loc,
                                                        (Ast.IdLid (_loc,
                                                           "camlp4_x")))))),
                                                  e)),
                                               (Ast.ExId (_loc,
                                                  (Ast.IdUid (_loc, "True")))))),
                                            (Ast.McArr (_loc,
                                               (Ast.PaAny _loc),
                                               (Ast.ExNil _loc),
                                               (Ast.ExId (_loc,
                                                  (Ast.IdUid (_loc, "False"))))))))) in
                                     let descr = "$" ^ (x ^ (" " ^ s)) in
                                     let text =
                                       TXtok (_loc, match_fun, descr) in
                                     let p =
                                       Ast.PaApp (_loc,
                                         (Ast.PaId (_loc,
                                            (Ast.IdUid (_loc, x)))),
                                         (Ast.PaTup (_loc, (Ast.PaAny _loc))))
                                     in
                                       {
                                         used = [];
                                         text = text;
                                         styp = STtok _loc;
                                         pattern = Some p;
                                       } :
                                      'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _"));
                            Gram.Stoken
                              (((function | STRING ((_)) -> true | _ -> false),
                                "STRING _")) ],
                          (Gram.Action.mk
                             (fun (s : Gram.Token.t) (x : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                (let s = Gram.Token.extract_string s in
                                 let x = Gram.Token.extract_string x
                                 in
                                   mk_tok _loc
                                     (Ast.PaApp (_loc,
                                        (Ast.PaId (_loc,
                                           (Ast.IdUid (_loc, x)))),
                                        (Ast.PaStr (_loc, s))))
                                     (STtok _loc) :
                                  'symbol))));
                         ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _")) ],
                          (Gram.Action.mk
                             (fun (x : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let x = Gram.Token.extract_string x
                                 in
                                   mk_tok _loc
                                     (Ast.PaApp (_loc,
                                        (Ast.PaId (_loc,
                                           (Ast.IdUid (_loc, x)))),
                                        (Ast.PaTup (_loc, (Ast.PaAny _loc)))))
                                     (STstring_tok _loc) :
                                  'symbol))));
                         ([ Gram.Skeyword "`";
                            Gram.Snterm
                              (Gram.Entry.obj (patt : 'patt Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (p : 'patt) _ (_loc : Gram.Loc.t) ->
                                (mk_tok _loc p (STtok _loc) : 'symbol))));
                         ([ Gram.Skeyword "[";
                            Gram.Slist0sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj (rule : 'rule Gram.Entry.t))),
                              (Gram.Skeyword "|"));
                            Gram.Skeyword "]" ],
                          (Gram.Action.mk
                             (fun _ (rl : 'rule list) _ (_loc : Gram.Loc.t)
                                ->
                                (let rl =
                                   retype_rule_list_without_patterns _loc rl in
                                 let t = new_type_var ()
                                 in
                                   {
                                     used = used_of_rule_list rl;
                                     text =
                                       TXrules (_loc, (srules _loc t rl ""));
                                     styp = STquo (_loc, t);
                                     pattern = None;
                                   } :
                                  'symbol))));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "NEXT" -> true
                                 | _ -> false),
                                "UIDENT \"NEXT\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "NEXT" ->
                                    ({
                                       used = [];
                                       text = TXnext _loc;
                                       styp = STself (_loc, "NEXT");
                                       pattern = None;
                                     } : 'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "SELF" -> true
                                 | _ -> false),
                                "UIDENT \"SELF\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "SELF" ->
                                    ({
                                       used = [];
                                       text = TXself _loc;
                                       styp = STself (_loc, "SELF");
                                       pattern = None;
                                     } : 'symbol)
                                | _ -> assert false))) ]) ]))
                  ());
             Gram.extend (pattern : 'pattern Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "("; Gram.Sself; Gram.Skeyword ",";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (comma_patt : 'comma_patt Gram.Entry.t));
                            Gram.Skeyword ")" ],
                          (Gram.Action.mk
                             (fun _ (p2 : 'comma_patt) _ (p1 : 'pattern) _
                                (_loc : Gram.Loc.t) ->
                                (Ast.PaTup (_loc, (Ast.PaCom (_loc, p1, p2))) :
                                  'pattern))));
                         ([ Gram.Skeyword "("; Gram.Sself; Gram.Skeyword ")" ],
                          (Gram.Action.mk
                             (fun _ (p : 'pattern) _ (_loc : Gram.Loc.t) ->
                                (p : 'pattern))));
                         ([ Gram.Skeyword "_" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (Ast.PaAny _loc : 'pattern))));
                         ([ Gram.Stoken
                              (((function | LIDENT ((_)) -> true | _ -> false),
                                "LIDENT _")) ],
                          (Gram.Action.mk
                             (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i
                                 in Ast.PaId (_loc, (Ast.IdLid (_loc, i))) :
                                  'pattern)))) ]) ]))
                  ());
             Gram.extend (comma_patt : 'comma_patt Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (pattern : 'pattern Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (p : 'pattern) (_loc : Gram.Loc.t) ->
                                (p : 'comma_patt))));
                         ([ Gram.Sself; Gram.Skeyword ","; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (p2 : 'comma_patt) _ (p1 : 'comma_patt)
                                (_loc : Gram.Loc.t) ->
                                (Ast.PaCom (_loc, p1, p2) : 'comma_patt)))) ]) ]))
                  ());
             Gram.extend (name : 'name Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (qualid : 'qualid Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (il : 'qualid) (_loc : Gram.Loc.t) ->
                                (mk_name _loc il : 'name)))) ]) ]))
                  ());
             Gram.extend (string : 'string Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function
                                 | ANTIQUOT ("", _) -> true
                                 | _ -> false),
                                "ANTIQUOT (\"\", _)")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | ANTIQUOT ("", s) ->
                                    (AntiquotSyntax.parse_expr _loc s :
                                      'string)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function | STRING ((_)) -> true | _ -> false),
                                "STRING _")) ],
                          (Gram.Action.mk
                             (fun (s : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let s = Gram.Token.extract_string s
                                 in Ast.ExStr (_loc, s) : 'string)))) ]) ]))
                  ());
             Gram.extend (semi_sep : 'semi_sep Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword ";" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) -> (() : 'semi_sep)))) ]) ]))
                  ()))
          
        (*
  EXTEND Gram
    symbol: LEVEL "top"
      [ NONA
        [ min = [ UIDENT "SLIST0" -> False | UIDENT "SLIST1" -> True ];
          s = SELF; sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
            sslist _loc min sep s
        | UIDENT "SOPT"; s = SELF ->
            ssopt _loc s ] ]
    ;
  END;
  *)
        let sfold _loc n foldfun f e s =
          let styp = STquo (_loc, (new_type_var ())) in
          let e =
            Ast.ExApp (_loc,
              (Ast.ExApp (_loc,
                 (Ast.ExId (_loc,
                    (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                       (Ast.IdLid (_loc, foldfun)))))),
                 f)),
              e) in
          let t =
            STapp (_loc,
              (STapp (_loc,
                 (STtyp
                    (Ast.TyApp (_loc,
                       (Ast.TyId (_loc,
                          (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                             (Ast.IdLid (_loc, "fold")))))),
                       (Ast.TyAny _loc)))),
                 s.styp)),
              styp)
          in
            {
              used = s.used;
              text = TXmeta (_loc, n, [ s.text ], e, t);
              styp = styp;
              pattern = None;
            }
          
        let sfoldsep _loc n foldfun f e s sep =
          let styp = STquo (_loc, (new_type_var ())) in
          let e =
            Ast.ExApp (_loc,
              (Ast.ExApp (_loc,
                 (Ast.ExId (_loc,
                    (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                       (Ast.IdLid (_loc, foldfun)))))),
                 f)),
              e) in
          let t =
            STapp (_loc,
              (STapp (_loc,
                 (STtyp
                    (Ast.TyApp (_loc,
                       (Ast.TyId (_loc,
                          (Ast.IdAcc (_loc, (Ast.IdUid (_loc, gm)),
                             (Ast.IdLid (_loc, "foldsep")))))),
                       (Ast.TyAny _loc)))),
                 s.styp)),
              styp)
          in
            {
              used = s.used @ sep.used;
              text = TXmeta (_loc, n, [ s.text; sep.text ], e, t);
              styp = styp;
              pattern = None;
            }
          
        let _ =
          let _ = (symbol : 'symbol Gram.Entry.t) in
          let grammar_entry_create = Gram.Entry.mk in
          let simple_expr : 'simple_expr Gram.Entry.t =
            grammar_entry_create "simple_expr"
          in
            (Gram.extend (symbol : 'symbol Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.Level "top")),
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function
                                 | UIDENT "FOLD1" -> true
                                 | _ -> false),
                                "UIDENT \"FOLD1\""));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Sself;
                            Gram.Stoken
                              (((function | UIDENT "SEP" -> true | _ -> false),
                                "UIDENT \"SEP\""));
                            Gram.Sself ],
                          (Gram.Action.mk
                             (fun (sep : 'symbol) (__camlp4_1 : Gram.Token.t)
                                (s : 'symbol) (e : 'simple_expr)
                                (f : 'simple_expr)
                                (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match (__camlp4_1, __camlp4_0) with
                                | (UIDENT "SEP", UIDENT "FOLD1") ->
                                    (sfoldsep _loc "FOLD1 SEP" "sfold1sep" f
                                       e s sep :
                                      'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "FOLD0" -> true
                                 | _ -> false),
                                "UIDENT \"FOLD0\""));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Sself;
                            Gram.Stoken
                              (((function | UIDENT "SEP" -> true | _ -> false),
                                "UIDENT \"SEP\""));
                            Gram.Sself ],
                          (Gram.Action.mk
                             (fun (sep : 'symbol) (__camlp4_1 : Gram.Token.t)
                                (s : 'symbol) (e : 'simple_expr)
                                (f : 'simple_expr)
                                (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match (__camlp4_1, __camlp4_0) with
                                | (UIDENT "SEP", UIDENT "FOLD0") ->
                                    (sfoldsep _loc "FOLD0 SEP" "sfold0sep" f
                                       e s sep :
                                      'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "FOLD1" -> true
                                 | _ -> false),
                                "UIDENT \"FOLD1\""));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Sself ],
                          (Gram.Action.mk
                             (fun (s : 'symbol) (e : 'simple_expr)
                                (f : 'simple_expr)
                                (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "FOLD1" ->
                                    (sfold _loc "FOLD1" "sfold1" f e s :
                                      'symbol)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | UIDENT "FOLD0" -> true
                                 | _ -> false),
                                "UIDENT \"FOLD0\""));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (simple_expr : 'simple_expr Gram.Entry.t));
                            Gram.Sself ],
                          (Gram.Action.mk
                             (fun (s : 'symbol) (e : 'simple_expr)
                                (f : 'simple_expr)
                                (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | UIDENT "FOLD0" ->
                                    (sfold _loc "FOLD0" "sfold0" f e s :
                                      'symbol)
                                | _ -> assert false))) ]) ]))
                  ());
             Gram.extend (simple_expr : 'simple_expr Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "(";
                            Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                            Gram.Skeyword ")" ],
                          (Gram.Action.mk
                             (fun _ (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                (e : 'simple_expr))));
                         ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (a_LIDENT : 'a_LIDENT Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (i : 'a_LIDENT) (_loc : Gram.Loc.t) ->
                                (Ast.ExId (_loc, (Ast.IdLid (_loc, i))) :
                                  'simple_expr)))) ]) ]))
                  ()))
          
        let _ =
          Options.add "-split_ext" (Arg.Set split_ext)
            "Split EXTEND by functions to turn around a PowerPC problem."
          
        let _ =
          Options.add "-split_gext" (Arg.Set split_ext)
            "Old name for the option -split_ext."
          
        let _ =
          Options.add "-meta_action" (Arg.Set meta_action) "Undocumented"
          
      end
      
    (* FIXME *)
    module M = Register.OCamlSyntaxExtension(Id)(Make)
      
  end
  
module M =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 * - Aleksey Nogin: extra features and bug fixes.
 * - Christopher Conway: extra feature (-D<uident>=)
 * - Jean-vincent Loddo: definitions inside IFs.
 *)
    module Id =
      struct let name = "Camlp4MacroParser"
                let version = Sys.ocaml_version
                  
      end
      
    (*
Added statements:

  At toplevel (structure item):

     DEFINE <uident>
     DEFINE <uident> = <expression>
     DEFINE <uident> (<parameters>) = <expression>
     IFDEF <uident> THEN <structure_items> [ ELSE <structure_items> ] (END | ENDIF)
     IFNDEF <uident> THEN <structure_items> [ ELSE <structure_items> ] (END | ENDIF)
     INCLUDE <string>

  At toplevel (signature item):

     DEFINE <uident>
     IFDEF <uident> THEN <signature_items> [ ELSE <signature_items> ] (END | ENDIF)
     IFNDEF <uident> THEN <signature_items> [ ELSE <signature_items> ] (END | ENDIF)
     INCLUDE <string>

  In expressions:

     IFDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     IFNDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     DEFINE <lident> = <expression> IN <expression>
     __FILE__
     __LOCATION__
     LOCATION_OF <parameter>

  In patterns:

     IFDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)
     IFNDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)

  As Camlp4 options:

     -D<uident> or -D<uident>=expr   define <uident> with optional value <expr>
     -U<uident>                      undefine it
     -I<dir>                         add <dir> to the search path for INCLUDE'd files

  After having used a DEFINE <uident> followed by "= <expression>", you
  can use it in expressions *and* in patterns. If the expression defining
  the macro cannot be used as a pattern, there is an error message if
  it is used in a pattern.

  You can also define a local macro in an expression usigng the DEFINE ... IN form.
  Note that local macros have lowercase names and can not take parameters.

  If a macro is defined to = NOTHING, and then used as an argument to a function,
  this will be equivalent to function taking one less argument. Similarly,
  passing NOTHING as an argument to a macro is equivalent to "erasing" the
  corresponding parameter from the macro body.

  The toplevel statement INCLUDE <string> can be used to include a
  file containing macro definitions and also any other toplevel items.
  The included files are looked up in directories passed in via the -I
  option, falling back to the current directory.

  The expression __FILE__ returns the current compiled file name.
  The expression __LOCATION__ returns the current location of itself.
  If used inside a macro, it returns the location where the macro is
  called.
  The expression (LOCATION_OF parameter) returns the location of the given
  macro parameter. It cannot be used outside a macro definition.

*)
    open Camlp4
      
    module Make (Syntax : Sig.Camlp4Syntax) =
      struct
        open Sig
          
        include Syntax
          
        type 'a item_or_def =
          | SdStr of 'a
          | SdDef of string * ((string list) * Ast.expr) option
          | SdUnd of string
          | SdITE of bool * ('a item_or_def) list * ('a item_or_def) list
          | SdLazy of 'a Lazy.t
        
        let rec list_remove x =
          function
          | (y, _) :: l when y = x -> l
          | d :: l -> d :: (list_remove x l)
          | [] -> []
          
        let defined = ref []
          
        let is_defined i = List.mem_assoc i !defined
          
        let bad_patt _loc =
          Loc.raise _loc
            (Failure
               "this macro cannot be used in a pattern (see its definition)")
          
        let substp _loc env =
          let rec loop =
            function
            | Ast.ExApp (_, e1, e2) -> Ast.PaApp (_loc, (loop e1), (loop e2))
            | Ast.ExNil _ -> Ast.PaNil _loc
            | Ast.ExId (_, (Ast.IdLid (_, x))) ->
                (try List.assoc x env
                 with | Not_found -> Ast.PaId (_loc, (Ast.IdLid (_loc, x))))
            | Ast.ExId (_, (Ast.IdUid (_, x))) ->
                (try List.assoc x env
                 with | Not_found -> Ast.PaId (_loc, (Ast.IdUid (_loc, x))))
            | Ast.ExInt (_, x) -> Ast.PaInt (_loc, x)
            | Ast.ExStr (_, s) -> Ast.PaStr (_loc, s)
            | Ast.ExTup (_, x) -> Ast.PaTup (_loc, (loop x))
            | Ast.ExCom (_, x1, x2) -> Ast.PaCom (_loc, (loop x1), (loop x2))
            | Ast.ExRec (_, bi, (Ast.ExNil _)) ->
                let rec substbi =
                  (function
                   | Ast.RbSem (_, b1, b2) ->
                       Ast.PaSem (_loc, (substbi b1), (substbi b2))
                   | Ast.RbEq (_, i, e) -> Ast.PaEq (_loc, i, (loop e))
                   | _ -> bad_patt _loc)
                in Ast.PaRec (_loc, (substbi bi))
            | _ -> bad_patt _loc
          in loop
          
        class reloc _loc =
          object inherit Ast.map as super method loc = fun _ -> _loc end
          
        (* method _Loc_t _ = _loc; *)
        class subst _loc env =
          object inherit reloc _loc as super
            method expr =
              function
              | (Ast.ExId (_, (Ast.IdLid (_, x))) |
                   Ast.ExId (_, (Ast.IdUid (_, x)))
                 as e) ->
                  (try List.assoc x env with | Not_found -> super#expr e)
              | (Ast.ExApp (_loc,
                   (Ast.ExId (_, (Ast.IdUid (_, "LOCATION_OF")))),
                   (Ast.ExId (_, (Ast.IdLid (_, x))))) |
                   Ast.ExApp (_loc,
                     (Ast.ExId (_, (Ast.IdUid (_, "LOCATION_OF")))),
                     (Ast.ExId (_, (Ast.IdUid (_, x)))))
                 as e) ->
                  (try
                     let loc = Ast.loc_of_expr (List.assoc x env) in
                     let (a, b, c, d, e, f, g, h) = Loc.to_tuple loc
                     in
                       Ast.ExApp (_loc,
                         (Ast.ExId (_loc,
                            (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Loc")),
                               (Ast.IdLid (_loc, "of_tuple")))))),
                         (Ast.ExTup (_loc,
                            (Ast.ExCom (_loc,
                               (Ast.ExStr (_loc, (Ast.safe_string_escaped a))),
                               (Ast.ExCom (_loc,
                                  (Ast.ExCom (_loc,
                                     (Ast.ExCom (_loc,
                                        (Ast.ExCom (_loc,
                                           (Ast.ExCom (_loc,
                                              (Ast.ExCom (_loc,
                                                 (Ast.ExInt (_loc,
                                                    (string_of_int b))),
                                                 (Ast.ExInt (_loc,
                                                    (string_of_int c))))),
                                              (Ast.ExInt (_loc,
                                                 (string_of_int d))))),
                                           (Ast.ExInt (_loc,
                                              (string_of_int e))))),
                                        (Ast.ExInt (_loc, (string_of_int f))))),
                                     (Ast.ExInt (_loc, (string_of_int g))))),
                                  (if h
                                   then
                                     Ast.ExId (_loc,
                                       (Ast.IdUid (_loc, "True")))
                                   else
                                     Ast.ExId (_loc,
                                       (Ast.IdUid (_loc, "False")))))))))))
                   with | Not_found -> super#expr e)
              | e -> super#expr e
            method patt =
              function
              | (Ast.PaId (_, (Ast.IdLid (_, x))) |
                   Ast.PaId (_, (Ast.IdUid (_, x)))
                 as p) ->
                  (try substp _loc [] (List.assoc x env)
                   with | Not_found -> super#patt p)
              | p -> super#patt p
          end
          
        let incorrect_number loc l1 l2 =
          Loc.raise loc
            (Failure
               (Printf.sprintf "expected %d parameters; found %d"
                  (List.length l2) (List.length l1)))
          
        let define eo x =
          ((match eo with
            | Some (([], e)) ->
                (Gram.extend (expr : 'expr Gram.Entry.t)
                   ((fun () ->
                       ((Some (Camlp4.Sig.Grammar.Level "simple")),
                        [ (None, None,
                           [ ([ Gram.Stoken
                                  (((function
                                     | UIDENT camlp4_x when camlp4_x = x ->
                                         true
                                     | _ -> false),
                                    "$UIDENT x")) ],
                              (Gram.Action.mk
                                 (fun (__camlp4_0 : Gram.Token.t)
                                    (_loc : Gram.Loc.t) ->
                                    match __camlp4_0 with
                                    | UIDENT ((_)) ->
                                        ((new reloc _loc)#expr e : 'expr)
                                    | _ -> assert false))) ]) ]))
                      ());
                 Gram.extend (patt : 'patt Gram.Entry.t)
                   ((fun () ->
                       ((Some (Camlp4.Sig.Grammar.Level "simple")),
                        [ (None, None,
                           [ ([ Gram.Stoken
                                  (((function
                                     | UIDENT camlp4_x when camlp4_x = x ->
                                         true
                                     | _ -> false),
                                    "$UIDENT x")) ],
                              (Gram.Action.mk
                                 (fun (__camlp4_0 : Gram.Token.t)
                                    (_loc : Gram.Loc.t) ->
                                    match __camlp4_0 with
                                    | UIDENT ((_)) ->
                                        (let p = substp _loc [] e
                                         in (new reloc _loc)#patt p : 'patt)
                                    | _ -> assert false))) ]) ]))
                      ()))
            | Some ((sl, e)) ->
                (Gram.extend (expr : 'expr Gram.Entry.t)
                   ((fun () ->
                       ((Some (Camlp4.Sig.Grammar.Level "apply")),
                        [ (None, None,
                           [ ([ Gram.Stoken
                                  (((function
                                     | UIDENT camlp4_x when camlp4_x = x ->
                                         true
                                     | _ -> false),
                                    "$UIDENT x"));
                                Gram.Sself ],
                              (Gram.Action.mk
                                 (fun (param : 'expr)
                                    (__camlp4_0 : Gram.Token.t)
                                    (_loc : Gram.Loc.t) ->
                                    match __camlp4_0 with
                                    | UIDENT ((_)) ->
                                        (let el =
                                           (match param with
                                            | Ast.ExTup (_, e) ->
                                                Ast.list_of_expr e []
                                            | e -> [ e ])
                                         in
                                           if
                                             (List.length el) =
                                               (List.length sl)
                                           then
                                             (let env = List.combine sl el
                                              in (new subst _loc env)#expr e)
                                           else incorrect_number _loc el sl :
                                          'expr)
                                    | _ -> assert false))) ]) ]))
                      ());
                 Gram.extend (patt : 'patt Gram.Entry.t)
                   ((fun () ->
                       ((Some (Camlp4.Sig.Grammar.Level "simple")),
                        [ (None, None,
                           [ ([ Gram.Stoken
                                  (((function
                                     | UIDENT camlp4_x when camlp4_x = x ->
                                         true
                                     | _ -> false),
                                    "$UIDENT x"));
                                Gram.Sself ],
                              (Gram.Action.mk
                                 (fun (param : 'patt)
                                    (__camlp4_0 : Gram.Token.t)
                                    (_loc : Gram.Loc.t) ->
                                    match __camlp4_0 with
                                    | UIDENT ((_)) ->
                                        (let pl =
                                           (match param with
                                            | Ast.PaTup (_, p) ->
                                                Ast.list_of_patt p []
                                            | p -> [ p ])
                                         in
                                           if
                                             (List.length pl) =
                                               (List.length sl)
                                           then
                                             (let env = List.combine sl pl in
                                              let p = substp _loc env e
                                              in (new reloc _loc)#patt p)
                                           else incorrect_number _loc pl sl :
                                          'patt)
                                    | _ -> assert false))) ]) ]))
                      ()))
            | None -> ());
           defined := (x, eo) :: !defined)
          
        let undef x =
          try
            ((let eo = List.assoc x !defined
              in
                match eo with
                | Some (([], _)) ->
                    (Gram.delete_rule expr
                       [ Gram.Stoken
                           (((function
                              | UIDENT camlp4_x when camlp4_x = x -> true
                              | _ -> false),
                             "$UIDENT x")) ];
                     Gram.delete_rule patt
                       [ Gram.Stoken
                           (((function
                              | UIDENT camlp4_x when camlp4_x = x -> true
                              | _ -> false),
                             "$UIDENT x")) ])
                | Some ((_, _)) ->
                    (Gram.delete_rule expr
                       [ Gram.Stoken
                           (((function
                              | UIDENT camlp4_x when camlp4_x = x -> true
                              | _ -> false),
                             "$UIDENT x"));
                         Gram.Sself ];
                     Gram.delete_rule patt
                       [ Gram.Stoken
                           (((function
                              | UIDENT camlp4_x when camlp4_x = x -> true
                              | _ -> false),
                             "$UIDENT x"));
                         Gram.Sself ])
                | None -> ());
             defined := list_remove x !defined)
          with | Struct.Grammar.Delete.Rule_not_found _ -> ()
          
        let parse_def s =
          match Gram.parse_string expr (Loc.mk "<command line>") s with
          | Ast.ExId (_, (Ast.IdUid (_, n))) -> define None n
          | Ast.ExApp (_,
              (Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "=")))),
                 (Ast.ExId (_, (Ast.IdUid (_, n)))))),
              e) -> define (Some (([], e))) n
          | _ -> invalid_arg s
          
        (* This is a list of directories to search for INCLUDE statements. *)
        let include_dirs = ref []
          
        (* Add something to the above, make sure it ends with a slash. *)
        let add_include_dir str =
          if str <> ""
          then
            (let str =
               if (String.get str ((String.length str) - 1)) = '/'
               then str
               else str ^ "/"
             in include_dirs := !include_dirs @ [ str ])
          else ()
          
        let parse_include_file rule =
          let dir_ok file dir = Sys.file_exists (dir ^ file)
          in
            fun file ->
              let file =
                try
                  (List.find (dir_ok file) (!include_dirs @ [ "./" ])) ^ file
                with | Not_found -> file in
              let ch = open_in file in
              let st = Stream.of_channel ch
              in Gram.parse rule (Loc.mk file) st
          
        let rec execute_macro nil cons =
          function
          | SdStr i -> i
          | SdDef (x, eo) -> (define eo x; nil)
          | SdUnd x -> (undef x; nil)
          | SdITE (b, l1, l2) ->
              execute_macro_list nil cons (if b then l1 else l2)
          | SdLazy l -> Lazy.force l
        and execute_macro_list nil cons =
          function
          | [] -> nil
          | hd :: tl -> (* The evaluation order is important here *)
              let il1 = execute_macro nil cons hd in
              let il2 = execute_macro_list nil cons tl in cons il1 il2
          
        (* Stack of conditionals. *)
        let stack = Stack.create ()
          
        (* Make an SdITE value by extracting the result of the test from the stack. *)
        let make_SdITE_result st1 st2 =
          let test = Stack.pop stack in SdITE (test, st1, st2)
          
        type branch = | Then | Else
        
        (* Execute macro only if it belongs to the currently active branch. *)
        let execute_macro_if_active_branch _loc nil cons branch macro_def =
          let test = Stack.top stack in
          let item =
            if (test && (branch = Then)) || ((not test) && (branch = Else))
            then execute_macro nil cons macro_def
            else (* ignore the macro *) nil
          in SdStr item
          
        let _ =
          let _ = (expr : 'expr Gram.Entry.t)
          and _ = (sig_item : 'sig_item Gram.Entry.t)
          and _ = (str_item : 'str_item Gram.Entry.t)
          and _ = (patt : 'patt Gram.Entry.t) in
          let grammar_entry_create = Gram.Entry.mk in
          let macro_def : 'macro_def Gram.Entry.t =
            grammar_entry_create "macro_def"
          and uident : 'uident Gram.Entry.t = grammar_entry_create "uident"
          and opt_macro_value : 'opt_macro_value Gram.Entry.t =
            grammar_entry_create "opt_macro_value"
          and endif : 'endif Gram.Entry.t = grammar_entry_create "endif"
          and sglist_else : 'sglist_else Gram.Entry.t =
            grammar_entry_create "sglist_else"
          and sglist_then : 'sglist_then Gram.Entry.t =
            grammar_entry_create "sglist_then"
          and smlist_else : 'smlist_else Gram.Entry.t =
            grammar_entry_create "smlist_else"
          and smlist_then : 'smlist_then Gram.Entry.t =
            grammar_entry_create "smlist_then"
          and else_expr : 'else_expr Gram.Entry.t =
            grammar_entry_create "else_expr"
          and else_macro_def_sig : 'else_macro_def_sig Gram.Entry.t =
            grammar_entry_create "else_macro_def_sig"
          and else_macro_def : 'else_macro_def Gram.Entry.t =
            grammar_entry_create "else_macro_def"
          and uident_eval_ifndef : 'uident_eval_ifndef Gram.Entry.t =
            grammar_entry_create "uident_eval_ifndef"
          and uident_eval_ifdef : 'uident_eval_ifdef Gram.Entry.t =
            grammar_entry_create "uident_eval_ifdef"
          and macro_def_sig : 'macro_def_sig Gram.Entry.t =
            grammar_entry_create "macro_def_sig"
          in
            (Gram.extend (str_item : 'str_item Gram.Entry.t)
               ((fun () ->
                   ((Some Camlp4.Sig.Grammar.First),
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (macro_def : 'macro_def Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (x : 'macro_def) (_loc : Gram.Loc.t) ->
                                (execute_macro (Ast.StNil _loc)
                                   (fun a b -> Ast.StSem (_loc, a, b)) x :
                                  'str_item)))) ]) ]))
                  ());
             Gram.extend (sig_item : 'sig_item Gram.Entry.t)
               ((fun () ->
                   ((Some Camlp4.Sig.Grammar.First),
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (macro_def_sig :
                                   'macro_def_sig Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (x : 'macro_def_sig) (_loc : Gram.Loc.t) ->
                                (execute_macro (Ast.SgNil _loc)
                                   (fun a b -> Ast.SgSem (_loc, a, b)) x :
                                  'sig_item)))) ]) ]))
                  ());
             Gram.extend (macro_def : 'macro_def Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "INCLUDE";
                            Gram.Stoken
                              (((function | STRING ((_)) -> true | _ -> false),
                                "STRING _")) ],
                          (Gram.Action.mk
                             (fun (fname : Gram.Token.t) _
                                (_loc : Gram.Loc.t) ->
                                (let fname = Gram.Token.extract_string fname
                                 in
                                   SdLazy
                                     (lazy
                                        (parse_include_file str_items fname)) :
                                  'macro_def))));
                         ([ Gram.Skeyword "IFNDEF";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (uident_eval_ifndef :
                                   'uident_eval_ifndef Gram.Entry.t));
                            Gram.Skeyword "THEN";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (smlist_then : 'smlist_then Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (else_macro_def :
                                   'else_macro_def Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (st2 : 'else_macro_def)
                                (st1 : 'smlist_then) _ _ _
                                (_loc : Gram.Loc.t) ->
                                (make_SdITE_result st1 st2 : 'macro_def))));
                         ([ Gram.Skeyword "IFDEF";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (uident_eval_ifdef :
                                   'uident_eval_ifdef Gram.Entry.t));
                            Gram.Skeyword "THEN";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (smlist_then : 'smlist_then Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (else_macro_def :
                                   'else_macro_def Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (st2 : 'else_macro_def)
                                (st1 : 'smlist_then) _ _ _
                                (_loc : Gram.Loc.t) ->
                                (make_SdITE_result st1 st2 : 'macro_def))));
                         ([ Gram.Skeyword "UNDEF";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (i : 'uident) _ (_loc : Gram.Loc.t) ->
                                (SdUnd i : 'macro_def))));
                         ([ Gram.Skeyword "DEFINE";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (opt_macro_value :
                                   'opt_macro_value Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (def : 'opt_macro_value) (i : 'uident) _
                                (_loc : Gram.Loc.t) ->
                                (SdDef (i, def) : 'macro_def)))) ]) ]))
                  ());
             Gram.extend (macro_def_sig : 'macro_def_sig Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "INCLUDE";
                            Gram.Stoken
                              (((function | STRING ((_)) -> true | _ -> false),
                                "STRING _")) ],
                          (Gram.Action.mk
                             (fun (fname : Gram.Token.t) _
                                (_loc : Gram.Loc.t) ->
                                (let fname = Gram.Token.extract_string fname
                                 in
                                   SdLazy
                                     (lazy
                                        (parse_include_file sig_items fname)) :
                                  'macro_def_sig))));
                         ([ Gram.Skeyword "IFNDEF";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (uident_eval_ifndef :
                                   'uident_eval_ifndef Gram.Entry.t));
                            Gram.Skeyword "THEN";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (sglist_then : 'sglist_then Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (else_macro_def_sig :
                                   'else_macro_def_sig Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (sg2 : 'else_macro_def_sig)
                                (sg1 : 'sglist_then) _ _ _
                                (_loc : Gram.Loc.t) ->
                                (make_SdITE_result sg1 sg2 : 'macro_def_sig))));
                         ([ Gram.Skeyword "IFDEF";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (uident_eval_ifdef :
                                   'uident_eval_ifdef Gram.Entry.t));
                            Gram.Skeyword "THEN";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (sglist_then : 'sglist_then Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (else_macro_def_sig :
                                   'else_macro_def_sig Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (sg2 : 'else_macro_def_sig)
                                (sg1 : 'sglist_then) _ _ _
                                (_loc : Gram.Loc.t) ->
                                (make_SdITE_result sg1 sg2 : 'macro_def_sig))));
                         ([ Gram.Skeyword "UNDEF";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (i : 'uident) _ (_loc : Gram.Loc.t) ->
                                (SdUnd i : 'macro_def_sig))));
                         ([ Gram.Skeyword "DEFINE";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (i : 'uident) _ (_loc : Gram.Loc.t) ->
                                (SdDef (i, None) : 'macro_def_sig)))) ]) ]))
                  ());
             Gram.extend
               (uident_eval_ifdef : 'uident_eval_ifdef Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (i : 'uident) (_loc : Gram.Loc.t) ->
                                (Stack.push (is_defined i) stack :
                                  'uident_eval_ifdef)))) ]) ]))
                  ());
             Gram.extend
               (uident_eval_ifndef : 'uident_eval_ifndef Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (i : 'uident) (_loc : Gram.Loc.t) ->
                                (Stack.push (not (is_defined i)) stack :
                                  'uident_eval_ifndef)))) ]) ]))
                  ());
             Gram.extend (else_macro_def : 'else_macro_def Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                ([] : 'else_macro_def))));
                         ([ Gram.Skeyword "ELSE";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (smlist_else : 'smlist_else Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (st : 'smlist_else) _ (_loc : Gram.Loc.t)
                                -> (st : 'else_macro_def)))) ]) ]))
                  ());
             Gram.extend
               (else_macro_def_sig : 'else_macro_def_sig Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                ([] : 'else_macro_def_sig))));
                         ([ Gram.Skeyword "ELSE";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (sglist_else : 'sglist_else Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (st : 'sglist_else) _ (_loc : Gram.Loc.t)
                                -> (st : 'else_macro_def_sig)))) ]) ]))
                  ());
             Gram.extend (else_expr : 'else_expr Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) :
                                  'else_expr))));
                         ([ Gram.Skeyword "ELSE";
                            Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                            Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                (e : 'else_expr)))) ]) ]))
                  ());
             Gram.extend (smlist_then : 'smlist_then Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Slist1
                              (Gram.srules smlist_then
                                 [ ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (str_item :
                                             'str_item Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (si : 'str_item)
                                          (_loc : Gram.Loc.t) ->
                                          (SdStr si : 'e__25))));
                                   ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (macro_def :
                                             'macro_def Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (d : 'macro_def)
                                          (_loc : Gram.Loc.t) ->
                                          (execute_macro_if_active_branch
                                             _loc (Ast.StNil _loc)
                                             (fun a b ->
                                                Ast.StSem (_loc, a, b))
                                             Then d :
                                            'e__25)))) ]) ],
                          (Gram.Action.mk
                             (fun (sml : 'e__25 list) (_loc : Gram.Loc.t) ->
                                (sml : 'smlist_then)))) ]) ]))
                  ());
             Gram.extend (smlist_else : 'smlist_else Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Slist1
                              (Gram.srules smlist_else
                                 [ ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (str_item :
                                             'str_item Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (si : 'str_item)
                                          (_loc : Gram.Loc.t) ->
                                          (SdStr si : 'e__26))));
                                   ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (macro_def :
                                             'macro_def Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (d : 'macro_def)
                                          (_loc : Gram.Loc.t) ->
                                          (execute_macro_if_active_branch
                                             _loc (Ast.StNil _loc)
                                             (fun a b ->
                                                Ast.StSem (_loc, a, b))
                                             Else d :
                                            'e__26)))) ]) ],
                          (Gram.Action.mk
                             (fun (sml : 'e__26 list) (_loc : Gram.Loc.t) ->
                                (sml : 'smlist_else)))) ]) ]))
                  ());
             Gram.extend (sglist_then : 'sglist_then Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Slist1
                              (Gram.srules sglist_then
                                 [ ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (sig_item :
                                             'sig_item Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (si : 'sig_item)
                                          (_loc : Gram.Loc.t) ->
                                          (SdStr si : 'e__27))));
                                   ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (macro_def_sig :
                                             'macro_def_sig Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (d : 'macro_def_sig)
                                          (_loc : Gram.Loc.t) ->
                                          (execute_macro_if_active_branch
                                             _loc (Ast.SgNil _loc)
                                             (fun a b ->
                                                Ast.SgSem (_loc, a, b))
                                             Then d :
                                            'e__27)))) ]) ],
                          (Gram.Action.mk
                             (fun (sgl : 'e__27 list) (_loc : Gram.Loc.t) ->
                                (sgl : 'sglist_then)))) ]) ]))
                  ());
             Gram.extend (sglist_else : 'sglist_else Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Slist1
                              (Gram.srules sglist_else
                                 [ ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (sig_item :
                                             'sig_item Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (si : 'sig_item)
                                          (_loc : Gram.Loc.t) ->
                                          (SdStr si : 'e__28))));
                                   ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (macro_def_sig :
                                             'macro_def_sig Gram.Entry.t));
                                      Gram.Snterm
                                        (Gram.Entry.obj
                                           (semi : 'semi Gram.Entry.t)) ],
                                    (Gram.Action.mk
                                       (fun _ (d : 'macro_def_sig)
                                          (_loc : Gram.Loc.t) ->
                                          (execute_macro_if_active_branch
                                             _loc (Ast.SgNil _loc)
                                             (fun a b ->
                                                Ast.SgSem (_loc, a, b))
                                             Else d :
                                            'e__28)))) ]) ],
                          (Gram.Action.mk
                             (fun (sgl : 'e__28 list) (_loc : Gram.Loc.t) ->
                                (sgl : 'sglist_else)))) ]) ]))
                  ());
             Gram.extend (endif : 'endif Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "ENDIF" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) -> (() : 'endif))));
                         ([ Gram.Skeyword "END" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) -> (() : 'endif)))) ]) ]))
                  ());
             Gram.extend (opt_macro_value : 'opt_macro_value Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([],
                          (Gram.Action.mk
                             (fun (_loc : Gram.Loc.t) ->
                                (None : 'opt_macro_value))));
                         ([ Gram.Skeyword "=";
                            Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                (Some (([], e)) : 'opt_macro_value))));
                         ([ Gram.Skeyword "(";
                            Gram.Slist1sep
                              ((Gram.srules opt_macro_value
                                  [ ([ Gram.Stoken
                                         (((function
                                            | LIDENT ((_)) -> true
                                            | _ -> false),
                                           "LIDENT _")) ],
                                     (Gram.Action.mk
                                        (fun (x : Gram.Token.t)
                                           (_loc : Gram.Loc.t) ->
                                           (let x =
                                              Gram.Token.extract_string x
                                            in x : 'e__29)))) ]),
                              (Gram.Skeyword ","));
                            Gram.Skeyword ")"; Gram.Skeyword "=";
                            Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'expr) _ _ (pl : 'e__29 list) _
                                (_loc : Gram.Loc.t) ->
                                (Some ((pl, e)) : 'opt_macro_value)))) ]) ]))
                  ());
             Gram.extend (expr : 'expr Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.Level "top")),
                    [ (None, None,
                       [ ([ Gram.Skeyword "DEFINE";
                            Gram.Stoken
                              (((function | LIDENT ((_)) -> true | _ -> false),
                                "LIDENT _"));
                            Gram.Skeyword "="; Gram.Sself;
                            Gram.Skeyword "IN"; Gram.Sself ],
                          (Gram.Action.mk
                             (fun (body : 'expr) _ (def : 'expr) _
                                (i : Gram.Token.t) _ (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i
                                 in (new subst _loc [ (i, def) ])#expr body :
                                  'expr))));
                         ([ Gram.Skeyword "IFNDEF";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t));
                            Gram.Skeyword "THEN"; Gram.Sself;
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (else_expr : 'else_expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e2 : 'else_expr) (e1 : 'expr) _
                                (i : 'uident) _ (_loc : Gram.Loc.t) ->
                                (if is_defined i then e2 else e1 : 'expr))));
                         ([ Gram.Skeyword "IFDEF";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t));
                            Gram.Skeyword "THEN"; Gram.Sself;
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (else_expr : 'else_expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e2 : 'else_expr) (e1 : 'expr) _
                                (i : 'uident) _ (_loc : Gram.Loc.t) ->
                                (if is_defined i then e1 else e2 : 'expr)))) ]) ]))
                  ());
             Gram.extend (patt : 'patt Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "IFNDEF";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t));
                            Gram.Skeyword "THEN"; Gram.Sself;
                            Gram.Skeyword "ELSE"; Gram.Sself;
                            Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (p2 : 'patt) _ (p1 : 'patt) _
                                (i : 'uident) _ (_loc : Gram.Loc.t) ->
                                (if is_defined i then p2 else p1 : 'patt))));
                         ([ Gram.Skeyword "IFDEF";
                            Gram.Snterm
                              (Gram.Entry.obj (uident : 'uident Gram.Entry.t));
                            Gram.Skeyword "THEN"; Gram.Sself;
                            Gram.Skeyword "ELSE"; Gram.Sself;
                            Gram.Snterm
                              (Gram.Entry.obj (endif : 'endif Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun _ (p2 : 'patt) _ (p1 : 'patt) _
                                (i : 'uident) _ (_loc : Gram.Loc.t) ->
                                (if is_defined i then p1 else p2 : 'patt)))) ]) ]))
                  ());
             Gram.extend (uident : 'uident Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function | UIDENT ((_)) -> true | _ -> false),
                                "UIDENT _")) ],
                          (Gram.Action.mk
                             (fun (i : Gram.Token.t) (_loc : Gram.Loc.t) ->
                                (let i = Gram.Token.extract_string i in i :
                                  'uident)))) ]) ]))
                  ());
             Gram.extend
               (* dirty hack to allow polymorphic variants using the introduced keywords. *)
               (expr : 'expr Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.Before "simple")),
                    [ (None, None,
                       [ ([ Gram.Skeyword "`";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (a_ident : 'a_ident Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (s : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                (Ast.ExVrn (_loc, s) : 'expr))));
                         ([ Gram.Skeyword "`";
                            Gram.srules expr
                              [ ([ Gram.Skeyword "IN" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30))));
                                ([ Gram.Skeyword "DEFINE" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30))));
                                ([ Gram.Skeyword "ENDIF" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30))));
                                ([ Gram.Skeyword "END" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30))));
                                ([ Gram.Skeyword "ELSE" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30))));
                                ([ Gram.Skeyword "THEN" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30))));
                                ([ Gram.Skeyword "IFNDEF" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30))));
                                ([ Gram.Skeyword "IFDEF" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__30)))) ] ],
                          (Gram.Action.mk
                             (fun (kwd : 'e__30) _ (_loc : Gram.Loc.t) ->
                                (Ast.ExVrn (_loc, kwd) : 'expr)))) ]) ]))
                  ());
             Gram.extend (* idem *) (patt : 'patt Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.Before "simple")),
                    [ (None, None,
                       [ ([ Gram.Skeyword "`";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (a_ident : 'a_ident Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (s : 'a_ident) _ (_loc : Gram.Loc.t) ->
                                (Ast.PaVrn (_loc, s) : 'patt))));
                         ([ Gram.Skeyword "`";
                            Gram.srules patt
                              [ ([ Gram.Skeyword "ENDIF" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__31))));
                                ([ Gram.Skeyword "END" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__31))));
                                ([ Gram.Skeyword "ELSE" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__31))));
                                ([ Gram.Skeyword "THEN" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__31))));
                                ([ Gram.Skeyword "IFNDEF" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__31))));
                                ([ Gram.Skeyword "IFDEF" ],
                                 (Gram.Action.mk
                                    (fun (x : Gram.Token.t)
                                       (_loc : Gram.Loc.t) ->
                                       (Gram.Token.extract_string x : 'e__31)))) ] ],
                          (Gram.Action.mk
                             (fun (kwd : 'e__31) _ (_loc : Gram.Loc.t) ->
                                (Ast.PaVrn (_loc, kwd) : 'patt)))) ]) ]))
                  ()))
          
        let _ =
          Options.add "-D" (Arg.String parse_def)
            "<string> Define for IFDEF instruction."
          
        let _ =
          Options.add "-U" (Arg.String undef)
            "<string> Undefine for IFDEF instruction."
          
        let _ =
          Options.add "-I" (Arg.String add_include_dir)
            "<string> Add a directory to INCLUDE search path."
          
      end
      
    let _ = let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
      
    module MakeNothing (AstFilters : Camlp4.Sig.AstFilters) =
      struct
        open AstFilters
          
        open Ast
          
        (* Remove NOTHING and expanse __FILE__ and __LOCATION__ *)
        let map_expr =
          function
          | Ast.ExApp (_, e, (Ast.ExId (_, (Ast.IdUid (_, "NOTHING"))))) |
              Ast.ExFun (_,
                (Ast.McArr (_, (Ast.PaId (_, (Ast.IdUid (_, "NOTHING")))),
                   (Ast.ExNil _), e)))
              -> e
          | Ast.ExId (_loc, (Ast.IdLid (_, "__FILE__"))) ->
              Ast.ExStr (_loc,
                (Ast.safe_string_escaped (Loc.file_name _loc)))
          | Ast.ExId (_loc, (Ast.IdLid (_, "__LOCATION__"))) ->
              let (a, b, c, d, e, f, g, h) = Loc.to_tuple _loc
              in
                Ast.ExApp (_loc,
                  (Ast.ExId (_loc,
                     (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Loc")),
                        (Ast.IdLid (_loc, "of_tuple")))))),
                  (Ast.ExTup (_loc,
                     (Ast.ExCom (_loc,
                        (Ast.ExStr (_loc, (Ast.safe_string_escaped a))),
                        (Ast.ExCom (_loc,
                           (Ast.ExCom (_loc,
                              (Ast.ExCom (_loc,
                                 (Ast.ExCom (_loc,
                                    (Ast.ExCom (_loc,
                                       (Ast.ExCom (_loc,
                                          (Ast.ExInt (_loc,
                                             (string_of_int b))),
                                          (Ast.ExInt (_loc,
                                             (string_of_int c))))),
                                       (Ast.ExInt (_loc, (string_of_int d))))),
                                    (Ast.ExInt (_loc, (string_of_int e))))),
                                 (Ast.ExInt (_loc, (string_of_int f))))),
                              (Ast.ExInt (_loc, (string_of_int g))))),
                           (if h
                            then Ast.ExId (_loc, (Ast.IdUid (_loc, "True")))
                            else Ast.ExId (_loc, (Ast.IdUid (_loc, "False")))))))))))
          | e -> e
          
        let _ = register_str_item_filter (Ast.map_expr map_expr)#str_item
          
      end
      
    let _ = let module M = Camlp4.Register.AstFilter(Id)(MakeNothing) in ()
      
  end
  
module D =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Nicolas Pouillard: initial version
 *)
    module Id =
      struct let name = "Camlp4DebugParser"
                let version = Sys.ocaml_version
                  
      end
      
    module Make (Syntax : Sig.Camlp4Syntax) =
      struct
        open Sig
          
        include Syntax
          
        module StringSet = Set.Make(String)
          
        let debug_mode =
          try
            let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
            let rec loop acc i =
              try
                let pos = String.index_from str i ':'
                in
                  loop (StringSet.add (String.sub str i (pos - i)) acc)
                    (pos + 1)
              with
              | Not_found ->
                  StringSet.add (String.sub str i ((String.length str) - i))
                    acc in
            let sections = loop StringSet.empty 0
            in
              if StringSet.mem "*" sections
              then (fun _ -> true)
              else (fun x -> StringSet.mem x sections)
          with | Not_found -> (fun _ -> false)
          
        let rec apply accu =
          function
          | [] -> accu
          | x :: xs ->
              let _loc = Ast.loc_of_expr x
              in apply (Ast.ExApp (_loc, accu, x)) xs
          
        let mk_debug_mode _loc =
          function
          | None ->
              Ast.ExId (_loc,
                (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Debug")),
                   (Ast.IdLid (_loc, "mode")))))
          | Some m ->
              Ast.ExId (_loc,
                (Ast.IdAcc (_loc, (Ast.IdUid (_loc, m)),
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Debug")),
                      (Ast.IdLid (_loc, "mode")))))))
          
        let mk_debug _loc m fmt section args =
          let call =
            apply
              (Ast.ExApp (_loc,
                 (Ast.ExApp (_loc,
                    (Ast.ExId (_loc,
                       (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Debug")),
                          (Ast.IdLid (_loc, "printf")))))),
                    (Ast.ExStr (_loc, section)))),
                 (Ast.ExStr (_loc, fmt))))
              args
          in
            Ast.ExIfe (_loc,
              (Ast.ExApp (_loc, (mk_debug_mode _loc m),
                 (Ast.ExStr (_loc, section)))),
              call, (Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))))
          
        let _ =
          let _ = (expr : 'expr Gram.Entry.t) in
          let grammar_entry_create = Gram.Entry.mk in
          let end_or_in : 'end_or_in Gram.Entry.t =
            grammar_entry_create "end_or_in"
          and start_debug : 'start_debug Gram.Entry.t =
            grammar_entry_create "start_debug"
          in
            (Gram.extend (expr : 'expr Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterm
                              (Gram.Entry.obj
                                 (start_debug : 'start_debug Gram.Entry.t));
                            Gram.Stoken
                              (((function | LIDENT ((_)) -> true | _ -> false),
                                "LIDENT _"));
                            Gram.Stoken
                              (((function | STRING ((_)) -> true | _ -> false),
                                "STRING _"));
                            Gram.Slist0
                              (Gram.Snterml
                                 ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                                 "."));
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (end_or_in : 'end_or_in Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (x : 'end_or_in) (args : 'expr list)
                                (fmt : Gram.Token.t) (section : Gram.Token.t)
                                (m : 'start_debug) (_loc : Gram.Loc.t) ->
                                (let fmt = Gram.Token.extract_string fmt in
                                 let section =
                                   Gram.Token.extract_string section
                                 in
                                   match (x, (debug_mode section)) with
                                   | (None, false) ->
                                       Ast.ExId (_loc,
                                         (Ast.IdUid (_loc, "()")))
                                   | (Some e, false) -> e
                                   | (None, _) ->
                                       mk_debug _loc m fmt section args
                                   | (Some e, _) ->
                                       Ast.ExLet (_loc, Ast.ReNil,
                                         (Ast.BiEq (_loc,
                                            (Ast.PaId (_loc,
                                               (Ast.IdUid (_loc, "()")))),
                                            (mk_debug _loc m fmt section args))),
                                         e) :
                                  'expr)))) ]) ]))
                  ());
             Gram.extend (end_or_in : 'end_or_in Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Skeyword "in";
                            Gram.Snterm
                              (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (e : 'expr) _ (_loc : Gram.Loc.t) ->
                                (Some e : 'end_or_in))));
                         ([ Gram.Skeyword "end" ],
                          (Gram.Action.mk
                             (fun _ (_loc : Gram.Loc.t) ->
                                (None : 'end_or_in)))) ]) ]))
                  ());
             Gram.extend (start_debug : 'start_debug Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Stoken
                              (((function
                                 | LIDENT "camlp4_debug" -> true
                                 | _ -> false),
                                "LIDENT \"camlp4_debug\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | LIDENT "camlp4_debug" ->
                                    (Some "Camlp4" : 'start_debug)
                                | _ -> assert false)));
                         ([ Gram.Stoken
                              (((function
                                 | LIDENT "debug" -> true
                                 | _ -> false),
                                "LIDENT \"debug\"")) ],
                          (Gram.Action.mk
                             (fun (__camlp4_0 : Gram.Token.t)
                                (_loc : Gram.Loc.t) ->
                                match __camlp4_0 with
                                | LIDENT "debug" -> (None : 'start_debug)
                                | _ -> assert false))) ]) ]))
                  ()))
          
      end
      
    let _ = let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
      
  end
  
module L =
  struct
    open Camlp4
      
    (* -*- camlp4r -*- *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Nao Hirokawa: initial version
 * - Nicolas Pouillard: revised syntax version
 *)
    module Id =
      struct
        let name = "Camlp4ListComprehension"
          
        let version = Sys.ocaml_version
          
      end
      
    module Make (Syntax : Sig.Camlp4Syntax) =
      struct
        open Sig
          
        include Syntax
          
        let rec loop n =
          function
          | [] -> None
          | [ (x, _) ] -> if n = 1 then Some x else None
          | _ :: l -> loop (n - 1) l
          
        let stream_peek_nth n strm = loop n (Stream.npeek n strm)
          
        (* usual trick *)
        let test_patt_lessminus =
          Gram.Entry.of_parser "test_patt_lessminus"
            (fun strm ->
               let rec skip_patt n =
                 match stream_peek_nth n strm with
                 | Some (KEYWORD "<-") -> n
                 | Some (KEYWORD ("[" | "[<")) ->
                     skip_patt ((ignore_upto "]" (n + 1)) + 1)
                 | Some (KEYWORD "(") ->
                     skip_patt ((ignore_upto ")" (n + 1)) + 1)
                 | Some (KEYWORD "{") ->
                     skip_patt ((ignore_upto "}" (n + 1)) + 1)
                 | Some (KEYWORD ("as" | "::" | "," | "_")) |
                     Some (LIDENT _ | UIDENT _) -> skip_patt (n + 1)
                 | Some _ | None -> raise Stream.Failure
               and ignore_upto end_kwd n =
                 match stream_peek_nth n strm with
                 | Some (KEYWORD prm) when prm = end_kwd -> n
                 | Some (KEYWORD ("[" | "[<")) ->
                     ignore_upto end_kwd ((ignore_upto "]" (n + 1)) + 1)
                 | Some (KEYWORD "(") ->
                     ignore_upto end_kwd ((ignore_upto ")" (n + 1)) + 1)
                 | Some (KEYWORD "{") ->
                     ignore_upto end_kwd ((ignore_upto "}" (n + 1)) + 1)
                 | Some _ -> ignore_upto end_kwd (n + 1)
                 | None -> raise Stream.Failure
               in skip_patt 1)
          
        let map _loc p e l =
          match (p, e) with
          | (Ast.PaId (_, (Ast.IdLid (_, x))),
             Ast.ExId (_, (Ast.IdLid (_, y)))) when x = y -> l
          | _ ->
              if Ast.is_irrefut_patt p
              then
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExId (_loc,
                        (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "List")),
                           (Ast.IdLid (_loc, "map")))))),
                     (Ast.ExFun (_loc,
                        (Ast.McArr (_loc, p, (Ast.ExNil _loc), e)))))),
                  l)
              else
                Ast.ExApp (_loc,
                  (Ast.ExApp (_loc,
                     (Ast.ExApp (_loc,
                        (Ast.ExId (_loc,
                           (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "List")),
                              (Ast.IdLid (_loc, "fold_right")))))),
                        (Ast.ExFun (_loc,
                           (Ast.McOr (_loc,
                              (Ast.McArr (_loc, p,
                                 (Ast.ExId (_loc, (Ast.IdUid (_loc, "True")))),
                                 (Ast.ExApp (_loc,
                                    (Ast.ExFun (_loc,
                                       (Ast.McArr (_loc,
                                          (Ast.PaId (_loc,
                                             (Ast.IdLid (_loc, "x")))),
                                          (Ast.ExNil _loc),
                                          (Ast.ExFun (_loc,
                                             (Ast.McArr (_loc,
                                                (Ast.PaId (_loc,
                                                   (Ast.IdLid (_loc, "xs")))),
                                                (Ast.ExNil _loc),
                                                (Ast.ExApp (_loc,
                                                   (Ast.ExApp (_loc,
                                                      (Ast.ExId (_loc,
                                                         (Ast.IdUid (_loc,
                                                            "::")))),
                                                      (Ast.ExId (_loc,
                                                         (Ast.IdLid (_loc,
                                                            "x")))))),
                                                   (Ast.ExId (_loc,
                                                      (Ast.IdLid (_loc, "xs")))))))))))))),
                                    e)))),
                              (Ast.McArr (_loc, (Ast.PaAny _loc),
                                 (Ast.ExNil _loc),
                                 (Ast.ExFun (_loc,
                                    (Ast.McArr (_loc,
                                       (Ast.PaId (_loc,
                                          (Ast.IdLid (_loc, "l")))),
                                       (Ast.ExNil _loc),
                                       (Ast.ExId (_loc,
                                          (Ast.IdLid (_loc, "l")))))))))))))))),
                     l)),
                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))))
          
        let filter _loc p b l =
          if Ast.is_irrefut_patt p
          then
            Ast.ExApp (_loc,
              (Ast.ExApp (_loc,
                 (Ast.ExId (_loc,
                    (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "List")),
                       (Ast.IdLid (_loc, "filter")))))),
                 (Ast.ExFun (_loc,
                    (Ast.McArr (_loc, p, (Ast.ExNil _loc), b)))))),
              l)
          else
            Ast.ExApp (_loc,
              (Ast.ExApp (_loc,
                 (Ast.ExId (_loc,
                    (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "List")),
                       (Ast.IdLid (_loc, "filter")))))),
                 (Ast.ExFun (_loc,
                    (Ast.McOr (_loc,
                       (Ast.McArr (_loc, p,
                          (Ast.ExId (_loc, (Ast.IdUid (_loc, "True")))), b)),
                       (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                          (Ast.ExId (_loc, (Ast.IdUid (_loc, "False")))))))))))),
              l)
          
        let concat _loc l =
          Ast.ExApp (_loc,
            (Ast.ExId (_loc,
               (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "List")),
                  (Ast.IdLid (_loc, "concat")))))),
            l)
          
        let rec compr _loc e =
          function
          | [ `gen ((p, l)) ] -> map _loc p e l
          | `gen ((p, l)) :: `cond b :: items ->
              compr _loc e ((`gen ((p, (filter _loc p b l)))) :: items)
          | `gen ((p, l)) :: ((`gen ((_, _)) :: _ as is)) ->
              concat _loc (map _loc p (compr _loc e is) l)
          | _ -> raise Stream.Failure
          
        let _ =
          Gram.delete_rule expr
            [ Gram.Skeyword "[";
              Gram.Snterm
                (Gram.Entry.obj
                   (sem_expr_for_list : 'sem_expr_for_list Gram.Entry.t));
              Gram.Skeyword "]" ]
          
        let is_revised =
          try
            (Gram.delete_rule expr
               [ Gram.Skeyword "[";
                 Gram.Snterm
                   (Gram.Entry.obj
                      (sem_expr_for_list : 'sem_expr_for_list Gram.Entry.t));
                 Gram.Skeyword "::";
                 Gram.Snterm (Gram.Entry.obj (expr : 'expr Gram.Entry.t));
                 Gram.Skeyword "]" ];
             true)
          with | Struct.Grammar.Delete.Rule_not_found _ -> false
          
        let comprehension_or_sem_expr_for_list =
          Gram.Entry.mk "comprehension_or_sem_expr_for_list"
          
        let _ =
          let _ = (expr : 'expr Gram.Entry.t)
          and _ =
            (comprehension_or_sem_expr_for_list :
              'comprehension_or_sem_expr_for_list Gram.Entry.t) in
          let grammar_entry_create = Gram.Entry.mk in
          let item : 'item Gram.Entry.t = grammar_entry_create "item"
          in
            (Gram.extend (expr : 'expr Gram.Entry.t)
               ((fun () ->
                   ((Some (Camlp4.Sig.Grammar.Level "simple")),
                    [ (None, None,
                       [ ([ Gram.Skeyword "[";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (comprehension_or_sem_expr_for_list :
                                   'comprehension_or_sem_expr_for_list Gram.
                                     Entry.t));
                            Gram.Skeyword "]" ],
                          (Gram.Action.mk
                             (fun _ (e : 'comprehension_or_sem_expr_for_list)
                                _ (_loc : Gram.Loc.t) -> (e : 'expr)))) ]) ]))
                  ());
             Gram.extend
               (comprehension_or_sem_expr_for_list :
                 'comprehension_or_sem_expr_for_list Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ Gram.Snterml
                              ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                              "top") ],
                          (Gram.Action.mk
                             (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                (Ast.ExApp (_loc,
                                   (Ast.ExApp (_loc,
                                      (Ast.ExId (_loc,
                                         (Ast.IdUid (_loc, "::")))),
                                      e)),
                                   (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))) :
                                  'comprehension_or_sem_expr_for_list))));
                         ([ Gram.Snterml
                              ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                              "top");
                            Gram.Skeyword "|";
                            Gram.Slist1sep
                              ((Gram.Snterm
                                  (Gram.Entry.obj (item : 'item Gram.Entry.t))),
                              (Gram.Skeyword ";")) ],
                          (Gram.Action.mk
                             (fun (l : 'item list) _ (e : 'expr)
                                (_loc : Gram.Loc.t) ->
                                (compr _loc e l :
                                  'comprehension_or_sem_expr_for_list))));
                         ([ Gram.Snterml
                              ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                              "top");
                            Gram.Skeyword ";" ],
                          (Gram.Action.mk
                             (fun _ (e : 'expr) (_loc : Gram.Loc.t) ->
                                (Ast.ExApp (_loc,
                                   (Ast.ExApp (_loc,
                                      (Ast.ExId (_loc,
                                         (Ast.IdUid (_loc, "::")))),
                                      e)),
                                   (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))) :
                                  'comprehension_or_sem_expr_for_list))));
                         ([ Gram.Snterml
                              ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                              "top");
                            Gram.Skeyword ";";
                            Gram.Snterm
                              (Gram.Entry.obj
                                 (sem_expr_for_list :
                                   'sem_expr_for_list Gram.Entry.t)) ],
                          (Gram.Action.mk
                             (fun (mk : 'sem_expr_for_list) _ (e : 'expr)
                                (_loc : Gram.Loc.t) ->
                                (Ast.ExApp (_loc,
                                   (Ast.ExApp (_loc,
                                      (Ast.ExId (_loc,
                                         (Ast.IdUid (_loc, "::")))),
                                      e)),
                                   (mk
                                      (Ast.ExId (_loc,
                                         (Ast.IdUid (_loc, "[]")))))) :
                                  'comprehension_or_sem_expr_for_list)))) ]) ]))
                  ());
             Gram.extend (item : 'item Gram.Entry.t)
               ((fun () ->
                   (None,
                    [ (None, None,
                       [ ([ (* NP: These rules rely on being on this particular order. Which should
             be improved. *)
                          Gram.Snterml
                            ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                            "top") ],
                          (Gram.Action.mk
                             (fun (e : 'expr) (_loc : Gram.Loc.t) ->
                                (`cond e : 'item))));
                         ([ Gram.Stry
                              (Gram.srules item
                                 [ ([ Gram.Snterm
                                        (Gram.Entry.obj
                                           (patt : 'patt Gram.Entry.t));
                                      Gram.Skeyword "<-" ],
                                    (Gram.Action.mk
                                       (fun _ (p : 'patt) (_loc : Gram.Loc.t)
                                          -> (p : 'e__32)))) ]);
                            Gram.Snterml
                              ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                              "top") ],
                          (Gram.Action.mk
                             (fun (e : 'expr) (p : 'e__32)
                                (_loc : Gram.Loc.t) ->
                                (`gen ((p, e)) : 'item)))) ]) ]))
                  ()))
          
        let _ =
          if is_revised
          then
            (let _ = (expr : 'expr Gram.Entry.t)
             and _ =
               (comprehension_or_sem_expr_for_list :
                 'comprehension_or_sem_expr_for_list Gram.Entry.t)
             in
               Gram.extend
                 (comprehension_or_sem_expr_for_list :
                   'comprehension_or_sem_expr_for_list Gram.Entry.t)
                 ((fun () ->
                     (None,
                      [ (None, None,
                         [ ([ Gram.Snterml
                                ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                                "top");
                              Gram.Skeyword "::";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (last : 'expr) _ (e : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdUid (_loc, "::")))),
                                        e)),
                                     last) :
                                    'comprehension_or_sem_expr_for_list))));
                           ([ Gram.Snterml
                                ((Gram.Entry.obj (expr : 'expr Gram.Entry.t)),
                                "top");
                              Gram.Skeyword ";";
                              Gram.Snterm
                                (Gram.Entry.obj
                                   (sem_expr_for_list :
                                     'sem_expr_for_list Gram.Entry.t));
                              Gram.Skeyword "::";
                              Gram.Snterm
                                (Gram.Entry.obj (expr : 'expr Gram.Entry.t)) ],
                            (Gram.Action.mk
                               (fun (last : 'expr) _
                                  (mk : 'sem_expr_for_list) _ (e : 'expr)
                                  (_loc : Gram.Loc.t) ->
                                  (Ast.ExApp (_loc,
                                     (Ast.ExApp (_loc,
                                        (Ast.ExId (_loc,
                                           (Ast.IdUid (_loc, "::")))),
                                        e)),
                                     (mk last)) :
                                    'comprehension_or_sem_expr_for_list)))) ]) ]))
                    ()))
          else ()
          
      end
      
    let _ = let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
      
  end
  
module P =
  struct
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Nicolas Pouillard: initial version
 *)
    let _ = Camlp4.Register.enable_dump_ocaml_ast_printer ()
      
  end
  
module B =
  struct
    (* camlp4r *)
    (****************************************************************************)
    (*                                                                          *)
    (*                                   OCaml                                  *)
    (*                                                                          *)
    (*                            INRIA Rocquencourt                            *)
    (*                                                                          *)
    (*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
    (*  en Automatique.  All rights reserved.  This file is distributed under   *)
    (*  the terms of the GNU Library General Public License, with the special   *)
    (*  exception on linking described in LICENSE at the top of the OCaml       *)
    (*  source tree.                                                            *)
    (*                                                                          *)
    (****************************************************************************)
    (* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
    open Camlp4
      
    open PreCast.Syntax
      
    open PreCast
      
    open Format
      
    module CleanAst = Camlp4.Struct.CleanAst.Make(Ast)
      
    module SSet = Set.Make(String)
      
    let pa_r = "Camlp4OCamlRevisedParser"
      
    let pa_rr = "Camlp4OCamlReloadedParser"
      
    let pa_o = "Camlp4OCamlParser"
      
    let pa_rp = "Camlp4OCamlRevisedParserParser"
      
    let pa_op = "Camlp4OCamlParserParser"
      
    let pa_g = "Camlp4GrammarParser"
      
    let pa_m = "Camlp4MacroParser"
      
    let pa_qb = "Camlp4QuotationCommon"
      
    let pa_q = "Camlp4QuotationExpander"
      
    let pa_rq = "Camlp4OCamlRevisedQuotationExpander"
      
    let pa_oq = "Camlp4OCamlOriginalQuotationExpander"
      
    let pa_l = "Camlp4ListComprehension"
      
    open Register
      
    let dyn_loader =
      ref (fun _ -> raise (Match_failure ("./camlp4/Camlp4Bin.ml", 45, 24)))
      
    let rcall_callback = ref (fun () -> ())
      
    let loaded_modules = ref SSet.empty
      
    let add_to_loaded_modules name =
      loaded_modules := SSet.add name !loaded_modules
      
    let (objext, libext) =
      if DynLoader.is_native then (".cmxs", ".cmxs") else (".cmo", ".cma")
      
    let rewrite_and_load n x =
      let dyn_loader = !dyn_loader () in
      let find_in_path = DynLoader.find_in_path dyn_loader in
      let real_load name =
        (add_to_loaded_modules name; DynLoader.load dyn_loader name) in
      let load =
        List.iter
          (fun n ->
             if
               (SSet.mem n !loaded_modules) ||
                 (List.mem n !Register.loaded_modules)
             then ()
             else
               (add_to_loaded_modules n;
                DynLoader.load dyn_loader (n ^ objext)))
      in
        ((match (n, (String.lowercase x)) with
          | (("Parsers" | ""),
             ("pa_r.cmo" | "r" | "ocamlr" | "ocamlrevised" |
                "camlp4ocamlrevisedparser.cmo"))
              -> load [ pa_r ]
          | (("Parsers" | ""),
             ("rr" | "reloaded" | "ocamlreloaded" |
                "camlp4ocamlreloadedparser.cmo"))
              -> load [ pa_rr ]
          | (("Parsers" | ""),
             ("pa_o.cmo" | "o" | "ocaml" | "camlp4ocamlparser.cmo")) ->
              load [ pa_r; pa_o ]
          | (("Parsers" | ""),
             ("pa_rp.cmo" | "rp" | "rparser" |
                "camlp4ocamlrevisedparserparser.cmo"))
              -> load [ pa_r; pa_rp ]
          | (("Parsers" | ""),
             ("pa_op.cmo" | "op" | "parser" | "camlp4ocamlparserparser.cmo"))
              -> load [ pa_r; pa_o; pa_rp; pa_op ]
          | (("Parsers" | ""),
             ("pa_extend.cmo" | "pa_extend_m.cmo" | "g" | "grammar" |
                "camlp4grammarparser.cmo"))
              -> load [ pa_g ]
          | (("Parsers" | ""),
             ("pa_macro.cmo" | "m" | "macro" | "camlp4macroparser.cmo")) ->
              load [ pa_m ]
          | (("Parsers" | ""), ("q" | "camlp4quotationexpander.cmo")) ->
              load [ pa_qb; pa_q ]
          | (("Parsers" | ""),
             ("q_mlast.cmo" | "rq" |
                "camlp4ocamlrevisedquotationexpander.cmo"))
              -> load [ pa_qb; pa_rq ]
          | (("Parsers" | ""),
             ("oq" | "camlp4ocamloriginalquotationexpander.cmo")) ->
              load [ pa_r; pa_o; pa_qb; pa_oq ]
          | (("Parsers" | ""), "rf") ->
              load [ pa_r; pa_rp; pa_qb; pa_q; pa_g; pa_l; pa_m ]
          | (("Parsers" | ""), "of") ->
              load
                [ pa_r; pa_o; pa_rp; pa_op; pa_qb; pa_q; pa_g; pa_l; pa_m ]
          | (("Parsers" | ""), ("comp" | "camlp4listcomprehension.cmo")) ->
              load [ pa_l ]
          | (("Filters" | ""), ("lift" | "camlp4astlifter.cmo")) ->
              load [ "Camlp4AstLifter" ]
          | (("Filters" | ""), ("exn" | "camlp4exceptiontracer.cmo")) ->
              load [ "Camlp4ExceptionTracer" ]
          | (("Filters" | ""), ("prof" | "camlp4profiler.cmo")) ->
              load [ "Camlp4Profiler" ]
          | (* map is now an alias of fold since fold handles map too *)
              (("Filters" | ""), ("map" | "camlp4mapgenerator.cmo")) ->
              load [ "Camlp4FoldGenerator" ]
          | (("Filters" | ""), ("fold" | "camlp4foldgenerator.cmo")) ->
              load [ "Camlp4FoldGenerator" ]
          | (("Filters" | ""), ("meta" | "camlp4metagenerator.cmo")) ->
              load [ "Camlp4MetaGenerator" ]
          | (("Filters" | ""), ("trash" | "camlp4trashremover.cmo")) ->
              load [ "Camlp4TrashRemover" ]
          | (("Filters" | ""), ("striploc" | "camlp4locationstripper.cmo"))
              -> load [ "Camlp4LocationStripper" ]
          | (("Printers" | ""),
             ("pr_r.cmo" | "r" | "ocamlr" | "camlp4ocamlrevisedprinter.cmo"))
              -> Register.enable_ocamlr_printer ()
          | (("Printers" | ""),
             ("pr_o.cmo" | "o" | "ocaml" | "camlp4ocamlprinter.cmo")) ->
              Register.enable_ocaml_printer ()
          | (("Printers" | ""),
             ("pr_dump.cmo" | "p" | "dumpocaml" | "camlp4ocamlastdumper.cmo"))
              -> Register.enable_dump_ocaml_ast_printer ()
          | (("Printers" | ""), ("d" | "dumpcamlp4" | "camlp4astdumper.cmo"))
              -> Register.enable_dump_camlp4_ast_printer ()
          | (("Printers" | ""), ("a" | "auto" | "camlp4autoprinter.cmo")) ->
              load [ "Camlp4AutoPrinter" ]
          | _ ->
              let y = "Camlp4" ^ (n ^ ("/" ^ (x ^ objext)))
              in real_load (try find_in_path y with | Not_found -> x));
         !rcall_callback ())
      
    let print_warning = eprintf "%a:\n%s@." Loc.print
      
    let rec parse_file dyn_loader name pa getdir =
      let directive_handler =
        Some
          (fun ast ->
             match getdir ast with
             | Some x ->
                 (match x with
                  | (_, "load", s) -> (rewrite_and_load "" s; None)
                  | (_, "directory", s) ->
                      (DynLoader.include_dir dyn_loader s; None)
                  | (_, "use", s) -> Some (parse_file dyn_loader s pa getdir)
                  | (_, "default_quotation", s) ->
                      (Quotation.default := s; None)
                  | (loc, _, _) ->
                      Loc.raise loc (Stream.Error "bad directive"))
             | None -> None) in
      let loc = Loc.mk name
      in
        (current_warning := print_warning;
         let ic = if name = "-" then stdin else open_in_bin name in
         let cs = Stream.of_channel ic in
         let clear () = if name = "-" then () else close_in ic in
         let phr =
           try pa ?directive_handler loc cs with | x -> (clear (); raise x)
         in (clear (); phr))
      
    let output_file = ref None
      
    let process dyn_loader name pa pr clean fold_filters getdir =
      let ast = parse_file dyn_loader name pa getdir in
      let ast = fold_filters (fun t filter -> filter t) ast in
      let ast = clean ast
      in pr ?input_file: (Some name) ?output_file: !output_file ast
      
    let gind =
      function
      | Ast.SgDir (loc, n, (Ast.ExStr (_, s))) -> Some ((loc, n, s))
      | _ -> None
      
    let gimd =
      function
      | Ast.StDir (loc, n, (Ast.ExStr (_, s))) -> Some ((loc, n, s))
      | _ -> None
      
    let process_intf dyn_loader name =
      process dyn_loader name CurrentParser.parse_interf CurrentPrinter.
        print_interf (new CleanAst.clean_ast)#sig_item AstFilters.
        fold_interf_filters gind
      
    let process_impl dyn_loader name =
      process dyn_loader name CurrentParser.parse_implem CurrentPrinter.
        print_implem (new CleanAst.clean_ast)#str_item AstFilters.
        fold_implem_filters gimd
      
    let just_print_the_version () =
      (printf "%s@." Camlp4_config.version; exit 0)
      
    let print_version () =
      (eprintf "Camlp4 version %s@." Camlp4_config.version; exit 0)
      
    let print_stdlib () =
      (printf "%s@." Camlp4_config.camlp4_standard_library; exit 0)
      
    let usage ini_sl ext_sl =
      (eprintf
         "\
Usage: camlp4 [load-options] [--] [other-options]\n\
Options:\n\
<file>.ml        Parse this implementation file\n\
<file>.mli       Parse this interface file\n\
<file>.%s Load this module inside the Camlp4 core@."
         (if DynLoader.is_native then "cmxs     " else "(cmo|cma)");
       Options.print_usage_list ini_sl;
       (* loop (ini_sl @ ext_sl) where rec loop =
      fun
      [ [(y, _, _) :: _] when y = "-help" -> ()
      | [_ :: sl] -> loop sl
      | [] -> eprintf "  -help         Display this list of options.@." ];    *)
       if ext_sl <> []
       then
         (eprintf "Options added by loaded object files:@.";
          Options.print_usage_list ext_sl)
       else ())
      
    let warn_noassert () =
      eprintf
        "\
camlp4 warning: option -noassert is obsolete\n\
You should give the -noassert option to the ocaml compiler instead.@."
      
    type file_kind =
      | Intf of string
      | Impl of string
      | Str of string
      | ModuleImpl of string
      | IncludeDir of string
    
    let search_stdlib = ref true
      
    let print_loaded_modules = ref false
      
    let (task, do_task) =
      let t = ref None in
      let task f x =
        let () = Camlp4_config.current_input_file := x
        in
          t :=
            Some
              (if !t = None then (fun _ -> f x) else (fun usage -> usage ())) in
      let do_task usage = match !t with | Some f -> f usage | None -> ()
      in (task, do_task)
      
    let input_file x =
      let dyn_loader = !dyn_loader ()
      in
        (!rcall_callback ();
         (match x with
          | Intf file_name -> task (process_intf dyn_loader) file_name
          | Impl file_name -> task (process_impl dyn_loader) file_name
          | Str s ->
              let (f, o) = Filename.open_temp_file "from_string" ".ml"
              in
                (output_string o s;
                 close_out o;
                 task (process_impl dyn_loader) f;
                 at_exit (fun () -> Sys.remove f))
          | ModuleImpl file_name -> rewrite_and_load "" file_name
          | IncludeDir dir -> DynLoader.include_dir dyn_loader dir);
         !rcall_callback ())
      
    let initial_spec_list =
      [ ("-I",
         (Arg.String
            (fun x ->
               input_file
                 (IncludeDir
                    (Camlp4_import.Misc.expand_directory Camlp4_config.
                       camlp4_standard_library x)))),
         "<directory>  Add directory in search patch for object files.");
        ("-where", (Arg.Unit print_stdlib),
         "Print camlp4 library directory and exit.");
        ("-nolib", (Arg.Clear search_stdlib),
         "No automatic search for object files in library directory.");
        ("-intf", (Arg.String (fun x -> input_file (Intf x))),
         "<file>  Parse <file> as an interface, whatever its extension.");
        ("-impl", (Arg.String (fun x -> input_file (Impl x))),
         "<file>  Parse <file> as an implementation, whatever its extension.");
        ("-str", (Arg.String (fun x -> input_file (Str x))),
         "<string>  Parse <string> as an implementation.");
        ("-unsafe", (Arg.Set Camlp4_config.unsafe),
         "Generate unsafe accesses to array and strings.");
        ("-noassert", (Arg.Unit warn_noassert),
         "Obsolete, do not use this option.");
        ("-verbose", (Arg.Set Camlp4_config.verbose),
         "More verbose in parsing errors.");
        ("-loc", (Arg.Set_string Loc.name),
         ("<name>   Name of the location variable (default: " ^
            (!Loc.name ^ ").")));
        ("-QD", (Arg.String (fun x -> Quotation.dump_file := Some x)),
         "<file> Dump quotation expander result in case of syntax error.");
        ("-o", (Arg.String (fun x -> output_file := Some x)),
         "<file> Output on <file> instead of standard output.");
        ("-v", (Arg.Unit print_version), "Print Camlp4 version and exit.");
        ("-version", (Arg.Unit just_print_the_version),
         "Print Camlp4 version number and exit.");
        ("-vnum", (Arg.Unit just_print_the_version),
         "Print Camlp4 version number and exit.");
        ("-no_quot", (Arg.Clear Camlp4_config.quotations),
         "Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
        ("-loaded-modules", (Arg.Set print_loaded_modules),
         "Print the list of loaded modules.");
        ("-parser", (Arg.String (rewrite_and_load "Parsers")),
         "<name>  Load the parser Camlp4Parsers/<name>.cm(o|a|xs)");
        ("-printer", (Arg.String (rewrite_and_load "Printers")),
         "<name>  Load the printer Camlp4Printers/<name>.cm(o|a|xs)");
        ("-filter", (Arg.String (rewrite_and_load "Filters")),
         "<name>  Load the filter Camlp4Filters/<name>.cm(o|a|xs)");
        ("-ignore", (Arg.String ignore), "ignore the next argument");
        ("--", (Arg.Unit ignore), "Deprecated, does nothing") ]
      
    let _ = Options.init initial_spec_list
      
    let anon_fun name =
      input_file
        (if Filename.check_suffix name ".mli"
         then Intf name
         else
           if Filename.check_suffix name ".ml"
           then Impl name
           else
             if Filename.check_suffix name objext
             then ModuleImpl name
             else
               if Filename.check_suffix name libext
               then ModuleImpl name
               else raise (Arg.Bad ("don't know what to do with " ^ name)))
      
    let main argv =
      let usage () =
        (usage initial_spec_list (Options.ext_spec_list ()); exit 0)
      in
        try
          let dynloader =
            DynLoader.mk ~ocaml_stdlib: !search_stdlib
              ~camlp4_stdlib: !search_stdlib ()
          in
            (dyn_loader := (fun () -> dynloader);
             let call_callback () =
               Register.iter_and_take_callbacks
                 (fun (name, module_callback) ->
                    let () = add_to_loaded_modules name in module_callback ())
             in
               (call_callback ();
                rcall_callback := call_callback;
                (match Options.parse anon_fun argv with
                 | [] -> ()
                 | ("-help" | "--help" | "-h" | "-?") :: _ -> usage ()
                 | s :: _ ->
                     (eprintf "%s: unknown or misused option\n" s;
                      eprintf "Use option -help for usage@.";
                      exit 2));
                do_task usage;
                call_callback ();
                if !print_loaded_modules
                then SSet.iter (eprintf "%s@.") !loaded_modules
                else ()))
        with
        | Arg.Bad s ->
            (eprintf "Error: %s\n" s;
             eprintf "Use option -help for usage@.";
             exit 2)
        | Arg.Help _ -> usage ()
        | exc -> (eprintf "@[<v0>%a@]@." ErrorHandler.print exc; exit 2)
      
    let _ = main Sys.argv
      
  end
  

