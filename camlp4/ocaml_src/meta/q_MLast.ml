(* camlp4r pa_extend.cmo pa_extend_m.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

let gram = Grammar.create (Plexer.make ());;

type ast =
    Node of string * ast list
  | List of ast list
  | Tuple of ast list
  | Option of ast option
  | Str of string
  | Chr of string
  | Bool of bool
  | Cons of ast * ast
  | Append of ast * ast
  | Record of (string * ast) list
  | Loc
  | Antiquot of MLast.loc * string
;;
let list l = List l;;

let sig_item = Grammar.Entry.create gram "signature item";;
let str_item = Grammar.Entry.create gram "structure item";;
let ctyp = Grammar.Entry.create gram "type";;
let patt = Grammar.Entry.create gram "pattern";;
let expr = Grammar.Entry.create gram "expression";;
let directive = Grammar.Entry.create gram "directive";;

let module_type = Grammar.Entry.create gram "module type";;
let module_expr = Grammar.Entry.create gram "module expression";;

let class_type = Grammar.Entry.create gram "class type";;
let class_expr = Grammar.Entry.create gram "class expr";;
let class_sig_item = Grammar.Entry.create gram "class signature item";;
let class_str_item = Grammar.Entry.create gram "class structure item";;

let antiquot k (bp, ep) x =
  let shift =
    if k = "" then String.length "$"
    else String.length "$" + String.length k + String.length ":"
  in
  Antiquot ((shift + bp, shift + ep), x)
;;

let mkumin f arg =
  match arg with
    Node ("ExInt", [Str n]) when int_of_string n > 0 ->
      let n = "-" ^ n in Node ("ExInt", [Str n])
  | Node ("ExFlo", [Str n]) when float_of_string n > 0.0 ->
      let n = "-" ^ n in Node ("ExFlo", [Str n])
  | _ -> let f = "~" ^ f in Node ("ExApp", [Node ("ExLid", [Str f]); arg])
;;

let mklistexp last =
  let rec loop top =
    function
      [] ->
        begin match last with
          Some e -> e
        | None -> Node ("ExUid", [Str "[]"])
        end
    | e1 :: el ->
        Node
          ("ExApp",
           [Node ("ExApp", [Node ("ExUid", [Str "::"]); e1]); loop false el])
  in
  loop true
;;

let mklistpat last =
  let rec loop top =
    function
      [] ->
        begin match last with
          Some p -> p
        | None -> Node ("PaUid", [Str "[]"])
        end
    | p1 :: pl ->
        Node
          ("PaApp",
           [Node ("PaApp", [Node ("PaUid", [Str "::"]); p1]); loop false pl])
  in
  loop true
;;

let neg s = string_of_int (- int_of_string s);;

let not_yet_warned = ref true;;
let warning_seq () =
  if !not_yet_warned then
    begin
      not_yet_warned := false;
      Printf.eprintf
        "\
*** warning: use of old syntax for sequences in expr quotation\n";
      flush stderr
    end
;;

Grammar.extend
  (let _ = (sig_item : 'sig_item Grammar.Entry.e)
   and _ = (str_item : 'str_item Grammar.Entry.e)
   and _ = (ctyp : 'ctyp Grammar.Entry.e)
   and _ = (patt : 'patt Grammar.Entry.e)
   and _ = (expr : 'expr Grammar.Entry.e)
   and _ = (directive : 'directive Grammar.Entry.e)
   and _ = (module_type : 'module_type Grammar.Entry.e)
   and _ = (module_expr : 'module_expr Grammar.Entry.e)
   and _ = (class_type : 'class_type Grammar.Entry.e)
   and _ = (class_expr : 'class_expr Grammar.Entry.e)
   and _ = (class_sig_item : 'class_sig_item Grammar.Entry.e)
   and _ = (class_str_item : 'class_str_item Grammar.Entry.e) in
   let grammar_entry_create s =
     Grammar.Entry.create (Grammar.of_entry sig_item) s
   in
   let rebind_exn : 'rebind_exn Grammar.Entry.e =
     grammar_entry_create "rebind_exn"
   and module_binding : 'module_binding Grammar.Entry.e =
     grammar_entry_create "module_binding"
   and module_declaration : 'module_declaration Grammar.Entry.e =
     grammar_entry_create "module_declaration"
   and with_constr : 'with_constr Grammar.Entry.e =
     grammar_entry_create "with_constr"
   and dir_param : 'dir_param Grammar.Entry.e =
     grammar_entry_create "dir_param"
   and dummy : 'dummy Grammar.Entry.e = grammar_entry_create "dummy"
   and let_binding : 'let_binding Grammar.Entry.e =
     grammar_entry_create "let_binding"
   and fun_binding : 'fun_binding Grammar.Entry.e =
     grammar_entry_create "fun_binding"
   and match_case : 'match_case Grammar.Entry.e =
     grammar_entry_create "match_case"
   and label_expr : 'label_expr Grammar.Entry.e =
     grammar_entry_create "label_expr"
   and fun_def : 'fun_def Grammar.Entry.e = grammar_entry_create "fun_def"
   and label_patt : 'label_patt Grammar.Entry.e =
     grammar_entry_create "label_patt"
   and patt_label_ident : 'patt_label_ident Grammar.Entry.e =
     grammar_entry_create "patt_label_ident"
   and ipatt : 'ipatt Grammar.Entry.e = grammar_entry_create "ipatt"
   and label_ipatt : 'label_ipatt Grammar.Entry.e =
     grammar_entry_create "label_ipatt"
   and type_declaration : 'type_declaration Grammar.Entry.e =
     grammar_entry_create "type_declaration"
   and constrain : 'constrain Grammar.Entry.e =
     grammar_entry_create "constrain"
   and type_parameter : 'type_parameter Grammar.Entry.e =
     grammar_entry_create "type_parameter"
   and row_field : 'row_field Grammar.Entry.e =
     grammar_entry_create "row_field"
   and opt_tag_list : 'opt_tag_list Grammar.Entry.e =
     grammar_entry_create "opt_tag_list"
   and constructor_declaration : 'constructor_declaration Grammar.Entry.e =
     grammar_entry_create "constructor_declaration"
   and label_declaration : 'label_declaration Grammar.Entry.e =
     grammar_entry_create "label_declaration"
   and ident : 'ident Grammar.Entry.e = grammar_entry_create "ident"
   and lident : 'lident Grammar.Entry.e = grammar_entry_create "lident"
   and uident : 'uident Grammar.Entry.e = grammar_entry_create "uident"
   and mod_ident : 'mod_ident Grammar.Entry.e =
     grammar_entry_create "mod_ident"
   and direction_flag : 'direction_flag Grammar.Entry.e =
     grammar_entry_create "direction_flag"
   and string : 'string Grammar.Entry.e = grammar_entry_create "string"
   and rec_flag : 'rec_flag Grammar.Entry.e = grammar_entry_create "rec_flag"
   and as_opt : 'as_opt Grammar.Entry.e = grammar_entry_create "as_opt"
   and when_opt : 'when_opt Grammar.Entry.e = grammar_entry_create "when_opt"
   and mutable_flag : 'mutable_flag Grammar.Entry.e =
     grammar_entry_create "mutable_flag"
   and anti_ : 'anti_ Grammar.Entry.e = grammar_entry_create "anti_"
   and anti_anti : 'anti_anti Grammar.Entry.e =
     grammar_entry_create "anti_anti"
   and anti_as : 'anti_as Grammar.Entry.e = grammar_entry_create "anti_as"
   and anti_chr : 'anti_chr Grammar.Entry.e = grammar_entry_create "anti_chr"
   and anti_exp : 'anti_exp Grammar.Entry.e = grammar_entry_create "anti_exp"
   and anti_flo : 'anti_flo Grammar.Entry.e = grammar_entry_create "anti_flo"
   and anti_int : 'anti_int Grammar.Entry.e = grammar_entry_create "anti_int"
   and anti_lid : 'anti_lid Grammar.Entry.e = grammar_entry_create "anti_lid"
   and anti_list : 'anti_list Grammar.Entry.e =
     grammar_entry_create "anti_list"
   and anti_mut : 'anti_mut Grammar.Entry.e = grammar_entry_create "anti_mut"
   and anti_opt : 'anti_opt Grammar.Entry.e = grammar_entry_create "anti_opt"
   and anti_rec : 'anti_rec Grammar.Entry.e = grammar_entry_create "anti_rec"
   and anti_str : 'anti_str Grammar.Entry.e = grammar_entry_create "anti_str"
   and anti_to : 'anti_to Grammar.Entry.e = grammar_entry_create "anti_to"
   and anti_uid : 'anti_uid Grammar.Entry.e = grammar_entry_create "anti_uid"
   and anti_when : 'anti_when Grammar.Entry.e =
     grammar_entry_create "anti_when"
   and class_declaration : 'class_declaration Grammar.Entry.e =
     grammar_entry_create "class_declaration"
   and class_fun_binding : 'class_fun_binding Grammar.Entry.e =
     grammar_entry_create "class_fun_binding"
   and class_type_parameters : 'class_type_parameters Grammar.Entry.e =
     grammar_entry_create "class_type_parameters"
   and class_fun_def : 'class_fun_def Grammar.Entry.e =
     grammar_entry_create "class_fun_def"
   and class_structure : 'class_structure Grammar.Entry.e =
     grammar_entry_create "class_structure"
   and class_self_patt_opt : 'class_self_patt_opt Grammar.Entry.e =
     grammar_entry_create "class_self_patt_opt"
   and cvalue : 'cvalue Grammar.Entry.e = grammar_entry_create "cvalue"
   and label : 'label Grammar.Entry.e = grammar_entry_create "label"
   and class_self_type_opt : 'class_self_type_opt Grammar.Entry.e =
     grammar_entry_create "class_self_type_opt"
   and class_description : 'class_description Grammar.Entry.e =
     grammar_entry_create "class_description"
   and class_type_declaration : 'class_type_declaration Grammar.Entry.e =
     grammar_entry_create "class_type_declaration"
   and field_expr_list : 'field_expr_list Grammar.Entry.e =
     grammar_entry_create "field_expr_list"
   and meth_list : 'meth_list Grammar.Entry.e =
     grammar_entry_create "meth_list"
   and field : 'field Grammar.Entry.e = grammar_entry_create "field"
   and longid : 'longid Grammar.Entry.e = grammar_entry_create "longid"
   and clty_longident : 'clty_longident Grammar.Entry.e =
     grammar_entry_create "clty_longident"
   and class_longident : 'class_longident Grammar.Entry.e =
     grammar_entry_create "class_longident"
   and virtual_flag : 'virtual_flag Grammar.Entry.e =
     grammar_entry_create "virtual_flag"
   and as_ident_opt : 'as_ident_opt Grammar.Entry.e =
     grammar_entry_create "as_ident_opt"
   and anti_virt : 'anti_virt Grammar.Entry.e =
     grammar_entry_create "anti_virt"
   in
   [Grammar.Entry.obj (module_expr : 'module_expr Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "struct");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (str_item : 'str_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (s : 'str_item) (loc : int * int) ->
                      (s : 'e__1))])],
          Gramext.action
            (fun (l : 'e__1 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (st : ast) _ (loc : int * int) ->
           (Node ("MeStr", [st]) : 'module_expr));
      [Gramext.Stoken ("", "functor"); Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e));
       Gramext.Stoken ("", ")"); Gramext.Stoken ("", "->"); Gramext.Sself],
      Gramext.action
        (fun (me : 'module_expr) _ _ (t : 'module_type) _ (i : 'uident) _ _
           (loc : int * int) ->
           (Node ("MeFun", [i; t; me]) : 'module_expr))];
     None, None,
     [[Gramext.Sself; Gramext.Sself],
      Gramext.action
        (fun (me2 : 'module_expr) (me1 : 'module_expr) (loc : int * int) ->
           (Node ("MeApp", [me1; me2]) : 'module_expr))];
     None, None,
     [[Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (me2 : 'module_expr) _ (me1 : 'module_expr) (loc : int * int) ->
           (Node ("MeAcc", [me1; me2]) : 'module_expr))];
     None, None,
     [[Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (me : 'module_expr) _ (loc : int * int) ->
           (me : 'module_expr));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (mt : 'module_type) _ (me : 'module_expr) _
           (loc : int * int) ->
           (Node ("MeTyc", [me; mt]) : 'module_expr));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) (loc : int * int) -> (a : 'module_expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_uid) (loc : int * int) ->
           (Node ("MeUid", [a]) : 'module_expr));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (Node ("MeUid", [Str i]) : 'module_expr))]];
    Grammar.Entry.obj (str_item : 'str_item Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (anti_exp : 'anti_exp Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'anti_exp) (loc : int * int) ->
           (Node ("StExp", [e]) : 'str_item));
      [Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) (loc : int * int) ->
           (Node ("StExp", [e]) : 'str_item));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'str_item));
      [Gramext.Stoken ("", "value");
       Gramext.Snterm
         (Grammar.Entry.obj (rec_flag : 'rec_flag Grammar.Entry.e));
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (let_binding : 'let_binding Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'let_binding list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (l : ast) (r : 'rec_flag) _ (loc : int * int) ->
           (Node ("StVal", [r; l]) : 'str_item));
      [Gramext.Stoken ("", "type");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (type_declaration : 'type_declaration Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'type_declaration list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (l : ast) _ (loc : int * int) ->
           (Node ("StTyp", [l]) : 'str_item));
      [Gramext.Stoken ("", "open");
       Gramext.Snterm
         (Grammar.Entry.obj (mod_ident : 'mod_ident Grammar.Entry.e))],
      Gramext.action
        (fun (m : 'mod_ident) _ (loc : int * int) ->
           (Node ("StOpn", [m]) : 'str_item));
      [Gramext.Stoken ("", "module"); Gramext.Stoken ("", "type");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e))],
      Gramext.action
        (fun (mt : 'module_type) _ (i : 'uident) _ _ (loc : int * int) ->
           (Node ("StMty", [i; mt]) : 'str_item));
      [Gramext.Stoken ("", "module");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (module_binding : 'module_binding Grammar.Entry.e))],
      Gramext.action
        (fun (mb : 'module_binding) (i : 'uident) _ (loc : int * int) ->
           (Node ("StMod", [i; mb]) : 'str_item));
      [Gramext.Stoken ("", "include");
       Gramext.Snterm
         (Grammar.Entry.obj (module_expr : 'module_expr Grammar.Entry.e))],
      Gramext.action
        (fun (me : 'module_expr) _ (loc : int * int) ->
           (Node ("StInc", [me]) : 'str_item));
      [Gramext.Stoken ("", "external");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.srules
         [[Gramext.Slist1
             (Gramext.Snterm
                (Grammar.Entry.obj (string : 'string Grammar.Entry.e)))],
          Gramext.action
            (fun (l : 'string list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (p : ast) _ (t : 'ctyp) _ (i : 'lident) _ (loc : int * int) ->
           (Node ("StExt", [i; t; p]) : 'str_item));
      [Gramext.Stoken ("", "exception");
       Gramext.Snterm
         (Grammar.Entry.obj
            (constructor_declaration :
             'constructor_declaration Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (rebind_exn : 'rebind_exn Grammar.Entry.e))],
      Gramext.action
        (fun (b : 'rebind_exn) (ctl : 'constructor_declaration) _
           (loc : int * int) ->
           (match ctl with
              Tuple [Loc; c; tl] -> Node ("StExc", [c; tl; b])
            | _ ->
                match () with
                _ -> raise (Match_failure ("meta/q_MLast.ml", 4408, 4424)) :
            'str_item));
      [Gramext.Stoken ("", "#");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (dir_param : 'dir_param Grammar.Entry.e))],
      Gramext.action
        (fun (dp : 'dir_param) (n : 'lident) _ (loc : int * int) ->
           (Node ("StDir", [n; dp]) : 'str_item));
      [Gramext.Stoken ("", "declare");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (str_item : 'str_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (s : 'str_item) (loc : int * int) ->
                      (s : 'e__2))])],
          Gramext.action
            (fun (l : 'e__2 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (st : ast) _ (loc : int * int) ->
           (Node ("StDcl", [st]) : 'str_item))]];
    Grammar.Entry.obj (rebind_exn : 'rebind_exn Grammar.Entry.e), None,
    [None, None,
     [[], Gramext.action (fun (loc : int * int) -> (List [] : 'rebind_exn));
      [Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (mod_ident : 'mod_ident Grammar.Entry.e))],
      Gramext.action
        (fun (sl : 'mod_ident) _ (loc : int * int) -> (sl : 'rebind_exn))]];
    Grammar.Entry.obj (module_binding : 'module_binding Grammar.Entry.e),
    None,
    [None, Some Gramext.RightA,
     [[Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (module_expr : 'module_expr Grammar.Entry.e))],
      Gramext.action
        (fun (me : 'module_expr) _ (loc : int * int) ->
           (me : 'module_binding));
      [Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (module_expr : 'module_expr Grammar.Entry.e))],
      Gramext.action
        (fun (me : 'module_expr) _ (mt : 'module_type) _ (loc : int * int) ->
           (Node ("MeTyc", [me; mt]) : 'module_binding));
      [Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e));
       Gramext.Stoken ("", ")"); Gramext.Sself],
      Gramext.action
        (fun (mb : 'module_binding) _ (mt : 'module_type) _ (m : 'uident) _
           (loc : int * int) ->
           (Node ("MeFun", [m; mt; mb]) : 'module_binding))]];
    Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "functor"); Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Sself; Gramext.Stoken ("", ")");
       Gramext.Stoken ("", "->"); Gramext.Sself],
      Gramext.action
        (fun (mt : 'module_type) _ _ (t : 'module_type) _ (i : 'uident) _ _
           (loc : int * int) ->
           (Node ("MtFun", [i; t; mt]) : 'module_type))];
     None, None,
     [[Gramext.Sself; Gramext.Stoken ("", "with");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (with_constr : 'with_constr Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'with_constr list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (wcl : ast) _ (mt : 'module_type) (loc : int * int) ->
           (Node ("MtWit", [mt; wcl]) : 'module_type))];
     None, None,
     [[Gramext.Stoken ("", "sig");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (sig_item : 'sig_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (s : 'sig_item) (loc : int * int) ->
                      (s : 'e__3))])],
          Gramext.action
            (fun (l : 'e__3 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (sg : ast) _ (loc : int * int) ->
           (Node ("MtSig", [sg]) : 'module_type))];
     None, None,
     [[Gramext.Sself; Gramext.Sself],
      Gramext.action
        (fun (m2 : 'module_type) (m1 : 'module_type) (loc : int * int) ->
           (Node ("MtApp", [m1; m2]) : 'module_type))];
     None, None,
     [[Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (m2 : 'module_type) _ (m1 : 'module_type) (loc : int * int) ->
           (Node ("MtAcc", [m1; m2]) : 'module_type))];
     None, None,
     [[Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (mt : 'module_type) _ (loc : int * int) ->
           (mt : 'module_type));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) (loc : int * int) -> (a : 'module_type));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_lid) (loc : int * int) ->
           (Node ("MtLid", [a]) : 'module_type));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_uid) (loc : int * int) ->
           (Node ("MtUid", [a]) : 'module_type));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (Node ("MtLid", [Str i]) : 'module_type));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (Node ("MtUid", [Str i]) : 'module_type))]];
    Grammar.Entry.obj (sig_item : 'sig_item Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'sig_item));
      [Gramext.Stoken ("", "value");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (i : 'lident) _ (loc : int * int) ->
           (Node ("SgVal", [i; t]) : 'sig_item));
      [Gramext.Stoken ("", "type");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (type_declaration : 'type_declaration Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'type_declaration list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (l : ast) _ (loc : int * int) ->
           (Node ("SgTyp", [l]) : 'sig_item));
      [Gramext.Stoken ("", "open");
       Gramext.Snterm
         (Grammar.Entry.obj (mod_ident : 'mod_ident Grammar.Entry.e))],
      Gramext.action
        (fun (m : 'mod_ident) _ (loc : int * int) ->
           (Node ("SgOpn", [m]) : 'sig_item));
      [Gramext.Stoken ("", "module"); Gramext.Stoken ("", "type");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e))],
      Gramext.action
        (fun (mt : 'module_type) _ (i : 'uident) _ _ (loc : int * int) ->
           (Node ("SgMty", [i; mt]) : 'sig_item));
      [Gramext.Stoken ("", "module");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (module_declaration : 'module_declaration Grammar.Entry.e))],
      Gramext.action
        (fun (mt : 'module_declaration) (i : 'uident) _ (loc : int * int) ->
           (Node ("SgMod", [i; mt]) : 'sig_item));
      [Gramext.Stoken ("", "include");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e))],
      Gramext.action
        (fun (mt : 'module_type) _ (loc : int * int) ->
           (Node ("SgInc", [mt]) : 'sig_item));
      [Gramext.Stoken ("", "external");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.srules
         [[Gramext.Slist1
             (Gramext.Snterm
                (Grammar.Entry.obj (string : 'string Grammar.Entry.e)))],
          Gramext.action
            (fun (l : 'string list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (p : ast) _ (t : 'ctyp) _ (i : 'lident) _ (loc : int * int) ->
           (Node ("SgExt", [i; t; p]) : 'sig_item));
      [Gramext.Stoken ("", "exception");
       Gramext.Snterm
         (Grammar.Entry.obj
            (constructor_declaration :
             'constructor_declaration Grammar.Entry.e))],
      Gramext.action
        (fun (ctl : 'constructor_declaration) _ (loc : int * int) ->
           (match ctl with
              Tuple [Loc; c; tl] -> Node ("SgExc", [c; tl])
            | _ ->
                match () with
                _ -> raise (Match_failure ("meta/q_MLast.ml", 6410, 6426)) :
            'sig_item));
      [Gramext.Stoken ("", "#");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (dir_param : 'dir_param Grammar.Entry.e))],
      Gramext.action
        (fun (dp : 'dir_param) (n : 'lident) _ (loc : int * int) ->
           (Node ("SgDir", [n; dp]) : 'sig_item));
      [Gramext.Stoken ("", "declare");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (sig_item : 'sig_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (s : 'sig_item) (loc : int * int) ->
                      (s : 'e__4))])],
          Gramext.action
            (fun (l : 'e__4 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (st : ast) _ (loc : int * int) ->
           (Node ("SgDcl", [st]) : 'sig_item))]];
    Grammar.Entry.obj
      (module_declaration : 'module_declaration Grammar.Entry.e),
    None,
    [None, Some Gramext.RightA,
     [[Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e));
       Gramext.Stoken ("", ")"); Gramext.Sself],
      Gramext.action
        (fun (mt : 'module_declaration) _ (t : 'module_type) _ (i : 'uident) _
           (loc : int * int) ->
           (Node ("MtFun", [i; t; mt]) : 'module_declaration));
      [Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e))],
      Gramext.action
        (fun (mt : 'module_type) _ (loc : int * int) ->
           (mt : 'module_declaration))]];
    Grammar.Entry.obj (with_constr : 'with_constr Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "module");
       Gramext.Snterm
         (Grammar.Entry.obj (mod_ident : 'mod_ident Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e))],
      Gramext.action
        (fun (mt : 'module_type) _ (i : 'mod_ident) _ (loc : int * int) ->
           (Node ("WcMod", [i; mt]) : 'with_constr));
      [Gramext.Stoken ("", "type");
       Gramext.Snterm
         (Grammar.Entry.obj (mod_ident : 'mod_ident Grammar.Entry.e));
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (type_parameter : 'type_parameter Grammar.Entry.e)))],
          Gramext.action
            (fun (l : 'type_parameter list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (tp : ast) (i : 'mod_ident) _ (loc : int * int) ->
           (Node ("WcTyp", [i; tp; t]) : 'with_constr))]];
    Grammar.Entry.obj (dir_param : 'dir_param Grammar.Entry.e), None,
    [None, None,
     [[],
      Gramext.action (fun (loc : int * int) -> (Option None : 'dir_param));
      [Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) (loc : int * int) -> (Option (Some e) : 'dir_param));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_opt : 'anti_opt Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_opt) (loc : int * int) -> (a : 'dir_param))]];
    Grammar.Entry.obj (expr : 'expr Grammar.Entry.e), None,
    [None, Some Gramext.RightA,
     [[Gramext.Stoken ("", "while"); Gramext.Sself; Gramext.Stoken ("", "do");
       Gramext.Stoken ("", "{");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (e : 'expr) (loc : int * int) -> (e : 'e__6))])],
          Gramext.action
            (fun (l : 'e__6 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (seq : ast) _ _ (e : 'expr) _ (loc : int * int) ->
           (Node ("ExWhi", [e; seq]) : 'expr));
      [Gramext.Stoken ("", "for");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", "="); Gramext.Sself;
       Gramext.Snterm
         (Grammar.Entry.obj
            (direction_flag : 'direction_flag Grammar.Entry.e));
       Gramext.Sself; Gramext.Stoken ("", "do"); Gramext.Stoken ("", "{");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (e : 'expr) (loc : int * int) -> (e : 'e__5))])],
          Gramext.action
            (fun (l : 'e__5 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (seq : ast) _ _ (e2 : 'expr) (df : 'direction_flag)
           (e1 : 'expr) _ (i : 'lident) _ (loc : int * int) ->
           (Node ("ExFor", [i; e1; e2; df; seq]) : 'expr));
      [Gramext.Stoken ("", "do"); Gramext.Stoken ("", "{");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'expr list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (seq : ast) _ _ (loc : int * int) ->
           (Node ("ExSeq", [seq]) : 'expr));
      [Gramext.Stoken ("", "if"); Gramext.Sself; Gramext.Stoken ("", "then");
       Gramext.Sself; Gramext.Stoken ("", "else"); Gramext.Sself],
      Gramext.action
        (fun (e3 : 'expr) _ (e2 : 'expr) _ (e1 : 'expr) _ (loc : int * int) ->
           (Node ("ExIfe", [e1; e2; e3]) : 'expr));
      [Gramext.Stoken ("", "try"); Gramext.Sself; Gramext.Stoken ("", "with");
       Gramext.Snterm (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e));
       Gramext.Stoken ("", "->"); Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) _ (p : 'ipatt) _ (x : 'expr) _ (loc : int * int) ->
           (Node ("ExTry", [x; List [Tuple [p; Option None; e]]]) : 'expr));
      [Gramext.Stoken ("", "try"); Gramext.Sself; Gramext.Stoken ("", "with");
       Gramext.Stoken ("", "[");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (match_case : 'match_case Grammar.Entry.e)),
              Gramext.Stoken ("", "|"))],
          Gramext.action
            (fun (l : 'match_case list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (l : ast) _ _ (e : 'expr) _ (loc : int * int) ->
           (Node ("ExTry", [e; l]) : 'expr));
      [Gramext.Stoken ("", "match"); Gramext.Sself;
       Gramext.Stoken ("", "with");
       Gramext.Snterm (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e));
       Gramext.Stoken ("", "->"); Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) _ (p : 'ipatt) _ (x : 'expr) _ (loc : int * int) ->
           (Node ("ExMat", [x; List [Tuple [p; Option None; e]]]) : 'expr));
      [Gramext.Stoken ("", "match"); Gramext.Sself;
       Gramext.Stoken ("", "with"); Gramext.Stoken ("", "[");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (match_case : 'match_case Grammar.Entry.e)),
              Gramext.Stoken ("", "|"))],
          Gramext.action
            (fun (l : 'match_case list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (l : ast) _ _ (e : 'expr) _ (loc : int * int) ->
           (Node ("ExMat", [e; l]) : 'expr));
      [Gramext.Stoken ("", "fun");
       Gramext.Snterm (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (fun_def : 'fun_def Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'fun_def) (p : 'ipatt) _ (loc : int * int) ->
           (Node ("ExFun", [List [Tuple [p; Option None; e]]]) : 'expr));
      [Gramext.Stoken ("", "fun"); Gramext.Stoken ("", "[");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (match_case : 'match_case Grammar.Entry.e)),
              Gramext.Stoken ("", "|"))],
          Gramext.action
            (fun (l : 'match_case list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (l : ast) _ _ (loc : int * int) ->
           (Node ("ExFun", [l]) : 'expr));
      [Gramext.Stoken ("", "let"); Gramext.Stoken ("", "module");
       Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (module_binding : 'module_binding Grammar.Entry.e));
       Gramext.Stoken ("", "in"); Gramext.Sself],
      Gramext.action
        (fun (x : 'expr) _ (mb : 'module_binding) (m : 'uident) _ _
           (loc : int * int) ->
           (Node ("ExLmd", [m; mb; x]) : 'expr));
      [Gramext.Stoken ("", "let");
       Gramext.Snterm
         (Grammar.Entry.obj (rec_flag : 'rec_flag Grammar.Entry.e));
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (let_binding : 'let_binding Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'let_binding list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "in"); Gramext.Sself],
      Gramext.action
        (fun (x : 'expr) _ (l : ast) (r : 'rec_flag) _ (loc : int * int) ->
           (Node ("ExLet", [r; l; x]) : 'expr))];
     None, Some Gramext.NonA,
     [[Gramext.Sself; Gramext.Stoken ("", ":="); Gramext.Sself;
       Gramext.Snterm (Grammar.Entry.obj (dummy : 'dummy Grammar.Entry.e))],
      Gramext.action
        (fun _ (e2 : 'expr) _ (e1 : 'expr) (loc : int * int) ->
           (Node ("ExAss", [e1; e2]) : 'expr))];
     None, Some Gramext.RightA,
     [[Gramext.Sself; Gramext.Stoken ("", "||"); Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (f : string) (e1 : 'expr) (loc : int * int) ->
           (Node
              ("ExApp", [Node ("ExApp", [Node ("ExLid", [Str f]); e1]); e2]) :
            'expr))];
     None, Some Gramext.RightA,
     [[Gramext.Sself; Gramext.Stoken ("", "&&"); Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (f : string) (e1 : 'expr) (loc : int * int) ->
           (Node
              ("ExApp", [Node ("ExApp", [Node ("ExLid", [Str f]); e1]); e2]) :
            'expr))];
     None, Some Gramext.LeftA,
     [[Gramext.Sself;
       Gramext.srules
         [[Gramext.Stoken ("", "!=")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7));
          [Gramext.Stoken ("", "==")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7));
          [Gramext.Stoken ("", "<>")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7));
          [Gramext.Stoken ("", "=")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7));
          [Gramext.Stoken ("", ">=")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7));
          [Gramext.Stoken ("", "<=")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7));
          [Gramext.Stoken ("", ">")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7));
          [Gramext.Stoken ("", "<")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__7))];
       Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (f : 'e__7) (e1 : 'expr) (loc : int * int) ->
           (Node
              ("ExApp", [Node ("ExApp", [Node ("ExLid", [Str f]); e1]); e2]) :
            'expr))];
     None, Some Gramext.RightA,
     [[Gramext.Sself;
       Gramext.srules
         [[Gramext.Stoken ("", "@")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__8));
          [Gramext.Stoken ("", "^")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__8))];
       Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (f : 'e__8) (e1 : 'expr) (loc : int * int) ->
           (Node
              ("ExApp", [Node ("ExApp", [Node ("ExLid", [Str f]); e1]); e2]) :
            'expr))];
     None, Some Gramext.LeftA,
     [[Gramext.Sself;
       Gramext.srules
         [[Gramext.Stoken ("", "-.")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__9));
          [Gramext.Stoken ("", "+.")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__9));
          [Gramext.Stoken ("", "-")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__9));
          [Gramext.Stoken ("", "+")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__9))];
       Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (f : 'e__9) (e1 : 'expr) (loc : int * int) ->
           (Node
              ("ExApp", [Node ("ExApp", [Node ("ExLid", [Str f]); e1]); e2]) :
            'expr))];
     None, Some Gramext.LeftA,
     [[Gramext.Sself;
       Gramext.srules
         [[Gramext.Stoken ("", "mod")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10));
          [Gramext.Stoken ("", "lxor")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10));
          [Gramext.Stoken ("", "lor")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10));
          [Gramext.Stoken ("", "land")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10));
          [Gramext.Stoken ("", "/.")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10));
          [Gramext.Stoken ("", "*.")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10));
          [Gramext.Stoken ("", "/")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10));
          [Gramext.Stoken ("", "*")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__10))];
       Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (f : 'e__10) (e1 : 'expr) (loc : int * int) ->
           (Node
              ("ExApp", [Node ("ExApp", [Node ("ExLid", [Str f]); e1]); e2]) :
            'expr))];
     None, Some Gramext.RightA,
     [[Gramext.Sself;
       Gramext.srules
         [[Gramext.Stoken ("", "lsr")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__11));
          [Gramext.Stoken ("", "lsl")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__11));
          [Gramext.Stoken ("", "asr")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__11));
          [Gramext.Stoken ("", "**")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__11))];
       Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (f : 'e__11) (e1 : 'expr) (loc : int * int) ->
           (Node
              ("ExApp", [Node ("ExApp", [Node ("ExLid", [Str f]); e1]); e2]) :
            'expr))];
     Some "unary minus", Some Gramext.NonA,
     [[Gramext.srules
         [[Gramext.Stoken ("", "-.")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__12));
          [Gramext.Stoken ("", "-")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__12))];
       Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) (f : 'e__12) (loc : int * int) ->
           (mkumin f e : 'expr))];
     Some "apply", Some Gramext.LeftA,
     [[Gramext.Sself; Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) (e1 : 'expr) (loc : int * int) ->
           (Node ("ExApp", [e1; e2]) : 'expr))];
     Some "label", Some Gramext.NonA,
     [[Gramext.Stoken ("", "?");
       Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) _ (loc : int * int) ->
           (Node ("ExOlb", [a; Node ("ExLid", [a])]) : 'expr));
      [Gramext.Stoken ("", "?");
       Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) _ (a : 'anti_) _ (loc : int * int) ->
           (Node ("ExOlb", [a; e]) : 'expr));
      [Gramext.Stoken ("", "~");
       Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) _ (loc : int * int) ->
           (Node ("ExLab", [a; Node ("ExLid", [a])]) : 'expr));
      [Gramext.Stoken ("", "~");
       Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) _ (a : 'anti_) _ (loc : int * int) ->
           (Node ("ExLab", [a; e]) : 'expr));
      [Gramext.Stoken ("QUESTIONIDENT", "")],
      Gramext.action
        (fun (lab : string) (loc : int * int) ->
           (Node ("ExOlb", [Str lab; Node ("ExLid", [Str lab])]) : 'expr));
      [Gramext.Stoken ("QUESTIONIDENTCOLON", ""); Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) (lab : string) (loc : int * int) ->
           (Node ("ExOlb", [Str lab; e]) : 'expr));
      [Gramext.Stoken ("TILDEIDENT", "")],
      Gramext.action
        (fun (lab : string) (loc : int * int) ->
           (Node ("ExLab", [Str lab; Node ("ExLid", [Str lab])]) : 'expr));
      [Gramext.Stoken ("TILDEIDENTCOLON", ""); Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) (lab : string) (loc : int * int) ->
           (Node ("ExLab", [Str lab; e]) : 'expr))];
     Some ".", Some Gramext.LeftA,
     [[Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (e2 : 'expr) _ (e1 : 'expr) (loc : int * int) ->
           (Node ("ExAcc", [e1; e2]) : 'expr));
      [Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Stoken ("", "[");
       Gramext.Sself; Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (e2 : 'expr) _ _ (e1 : 'expr) (loc : int * int) ->
           (Node ("ExSte", [e1; e2]) : 'expr));
      [Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Stoken ("", "(");
       Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (e2 : 'expr) _ _ (e1 : 'expr) (loc : int * int) ->
           (Node ("ExAre", [e1; e2]) : 'expr))];
     None, Some Gramext.NonA,
     [[Gramext.srules
         [[Gramext.Stoken ("", "~-.")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__13));
          [Gramext.Stoken ("", "~-")],
          Gramext.action
            (fun (op : string) (loc : int * int) -> (op : 'e__13))];
       Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) (f : 'e__13) (loc : int * int) ->
           (Node ("ExApp", [Node ("ExLid", [Str f]); e]) : 'expr))];
     Some "simple", None,
     [[Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action (fun _ (e : 'expr) _ (loc : int * int) -> (e : 'expr));
      [Gramext.Stoken ("", "(");
       Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (el : 'anti_list) _ (loc : int * int) ->
           (Node ("ExTup", [el]) : 'expr));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ",");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e)),
              Gramext.Stoken ("", ","))],
          Gramext.action
            (fun (l : 'expr list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (el : ast) _ (e : 'expr) _ (loc : int * int) ->
           (Node ("ExTup", [Cons (e, el)]) : 'expr));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (t : 'ctyp) _ (e : 'expr) _ (loc : int * int) ->
           (Node ("ExTyc", [e; t]) : 'expr));
      [Gramext.Stoken ("", "("); Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ _ (loc : int * int) -> (Node ("ExUid", [Str "()"]) : 'expr));
      [Gramext.Stoken ("", "{"); Gramext.Stoken ("", "("); Gramext.Sself;
       Gramext.Stoken ("", ")"); Gramext.Stoken ("", "with");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (label_expr : 'label_expr Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'label_expr list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (lel : ast) _ _ (e : 'expr) _ _ (loc : int * int) ->
           (Node ("ExRec", [lel; Option (Some e)]) : 'expr));
      [Gramext.Stoken ("", "{");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (label_expr : 'label_expr Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'label_expr list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (lel : ast) _ (loc : int * int) ->
           (Node ("ExRec", [lel; Option None]) : 'expr));
      [Gramext.Stoken ("", "[|");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'expr list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "|]")],
      Gramext.action
        (fun _ (el : ast) _ (loc : int * int) ->
           (Node ("ExArr", [el]) : 'expr));
      [Gramext.Stoken ("", "[");
       Gramext.Slist1sep
         (Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e)),
          Gramext.Stoken ("", ";"));
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("", "::");
              Gramext.Snterm
                (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
             Gramext.action
               (fun (e : 'expr) _ (loc : int * int) -> (e : 'e__14))]);
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (last : 'e__14 option) (el : 'expr list) _ (loc : int * int) ->
           (mklistexp last el : 'expr));
      [Gramext.Stoken ("", "["); Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ _ (loc : int * int) -> (Node ("ExUid", [Str "[]"]) : 'expr));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_anti : 'anti_anti Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_anti) (loc : int * int) ->
           (Node ("ExAnt", [a]) : 'expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_lid) (loc : int * int) ->
           (Node ("ExLid", [a]) : 'expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_uid) (loc : int * int) ->
           (Node ("ExUid", [a]) : 'expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_chr : 'anti_chr Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_chr) (loc : int * int) ->
           (Node ("ExChr", [a]) : 'expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_str : 'anti_str Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_str) (loc : int * int) ->
           (Node ("ExStr", [a]) : 'expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_flo : 'anti_flo Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_flo) (loc : int * int) ->
           (Node ("ExFlo", [a]) : 'expr));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_int : 'anti_int Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_int) (loc : int * int) ->
           (Node ("ExInt", [a]) : 'expr));
      [Gramext.Stoken ("", "`");
       Gramext.Snterm (Grammar.Entry.obj (ident : 'ident Grammar.Entry.e))],
      Gramext.action
        (fun (s : 'ident) _ (loc : int * int) ->
           (Node ("ExVrn", [s]) : 'expr));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("ExLid", [Str s]) : 'expr));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("ExUid", [Str s]) : 'expr));
      [Gramext.Stoken ("CHAR", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("ExChr", [Str s]) : 'expr));
      [Gramext.Stoken ("STRING", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("ExStr", [Str s]) : 'expr));
      [Gramext.Stoken ("FLOAT", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("ExFlo", [Str s]) : 'expr));
      [Gramext.Stoken ("INT", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("ExInt", [Str s]) : 'expr))]];
    Grammar.Entry.obj (expr : 'expr Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "while"); Gramext.Sself; Gramext.Stoken ("", "do");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (e : 'expr) (loc : int * int) -> (e : 'e__17))])],
          Gramext.action
            (fun (l : 'e__17 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "done")],
      Gramext.action
        (fun _ (seq : ast) _ (e : 'expr) _ (loc : int * int) ->
           (let _ = warning_seq () in Node ("ExWhi", [e; seq]) : 'expr));
      [Gramext.Stoken ("", "for");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", "="); Gramext.Sself;
       Gramext.Snterm
         (Grammar.Entry.obj
            (direction_flag : 'direction_flag Grammar.Entry.e));
       Gramext.Sself; Gramext.Stoken ("", "do");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (e : 'expr) (loc : int * int) -> (e : 'e__16))])],
          Gramext.action
            (fun (l : 'e__16 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "done")],
      Gramext.action
        (fun _ (seq : ast) _ (e2 : 'expr) (df : 'direction_flag) (e1 : 'expr)
           _ (i : 'lident) _ (loc : int * int) ->
           (let _ = warning_seq () in Node ("ExFor", [i; e1; e2; df; seq]) :
            'expr));
      [Gramext.Stoken ("", "do");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (e : 'expr) (loc : int * int) -> (e : 'e__15))])],
          Gramext.action
            (fun (l : 'e__15 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "return"); Gramext.Sself],
      Gramext.action
        (fun (e : 'expr) _ (seq : ast) _ (loc : int * int) ->
           (let _ = warning_seq () in Node ("ExSeq", [Append (seq, e)]) :
            'expr))]];
    Grammar.Entry.obj (dummy : 'dummy Grammar.Entry.e), None,
    [None, None,
     [[], Gramext.action (fun (loc : int * int) -> (() : 'dummy))]];
    Grammar.Entry.obj (let_binding : 'let_binding Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (fun_binding : 'fun_binding Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'fun_binding) (p : 'ipatt) (loc : int * int) ->
           (Tuple [p; e] : 'let_binding))]];
    Grammar.Entry.obj (fun_binding : 'fun_binding Grammar.Entry.e), None,
    [None, Some Gramext.RightA,
     [[Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (t : 'ctyp) _ (loc : int * int) ->
           (Node ("ExTyc", [e; t]) : 'fun_binding));
      [Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (loc : int * int) -> (e : 'fun_binding));
      [Gramext.Snterm (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e));
       Gramext.Sself],
      Gramext.action
        (fun (e : 'fun_binding) (p : 'ipatt) (loc : int * int) ->
           (Node ("ExFun", [List [Tuple [p; Option None; e]]]) :
            'fun_binding))]];
    Grammar.Entry.obj (match_case : 'match_case Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (as_opt : 'as_opt Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (when_opt : 'when_opt Grammar.Entry.e));
       Gramext.Stoken ("", "->");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (w : 'when_opt) (aso : 'as_opt) (p : 'patt)
           (loc : int * int) ->
           (let p =
              match aso with
                Option (Some p2) -> Node ("PaAli", [p; p2])
              | Option None -> p
              | _ -> Node ("PaAli", [p; aso])
            in
            Tuple [p; w; e] :
            'match_case))]];
    Grammar.Entry.obj (label_expr : 'label_expr Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj
            (patt_label_ident : 'patt_label_ident Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (i : 'patt_label_ident) (loc : int * int) ->
           (Tuple [i; e] : 'label_expr))]];
    Grammar.Entry.obj (fun_def : 'fun_def Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "->");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action (fun (e : 'expr) _ (loc : int * int) -> (e : 'fun_def));
      [Gramext.Snterm (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e));
       Gramext.Sself],
      Gramext.action
        (fun (e : 'fun_def) (p : 'ipatt) (loc : int * int) ->
           (Node ("ExFun", [List [Tuple [p; Option None; e]]]) : 'fun_def))]];
    Grammar.Entry.obj (patt : 'patt Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Sself; Gramext.Stoken ("", "|"); Gramext.Sself],
      Gramext.action
        (fun (p2 : 'patt) _ (p1 : 'patt) (loc : int * int) ->
           (Node ("PaOrp", [p1; p2]) : 'patt))];
     None, None,
     [[Gramext.Sself; Gramext.Stoken ("", ".."); Gramext.Sself],
      Gramext.action
        (fun (p2 : 'patt) _ (p1 : 'patt) (loc : int * int) ->
           (Node ("PaRng", [p1; p2]) : 'patt))];
     None, None,
     [[Gramext.Sself; Gramext.Sself],
      Gramext.action
        (fun (p2 : 'patt) (p1 : 'patt) (loc : int * int) ->
           (Node ("PaApp", [p1; p2]) : 'patt))];
     None, None,
     [[Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (p2 : 'patt) _ (p1 : 'patt) (loc : int * int) ->
           (Node ("PaAcc", [p1; p2]) : 'patt))];
     None, Some Gramext.NonA,
     [[Gramext.Stoken ("", "?"); Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (e : 'expr) _ (t : 'ctyp) _ (i : 'lident) _ _
           (loc : int * int) ->
           (let p = Node ("PaTyc", [Node ("PaLid", [i]); t]) in
            Node ("PaOlb", [i; p; Option (Some e)]) :
            'patt));
      [Gramext.Stoken ("", "?"); Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (e : 'expr) _ (i : 'lident) _ _ (loc : int * int) ->
           (Node ("PaOlb", [i; Node ("PaLid", [i]); Option (Some e)]) :
            'patt));
      [Gramext.Stoken ("", "?");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e))],
      Gramext.action
        (fun (i : 'lident) _ (loc : int * int) ->
           (Node ("PaOlb", [i; Node ("PaLid", [i]); Option None]) : 'patt));
      [Gramext.Stoken ("", "?");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Stoken ("", "("); Gramext.Sself;
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("", "=");
              Gramext.Snterm
                (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
             Gramext.action
               (fun (e : 'expr) _ (loc : int * int) -> (e : 'e__19))]);
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (e : 'e__19 option) (t : 'ctyp) _ (p : 'patt) _ _ (i : 'lident)
           _ (loc : int * int) ->
           (let p = Node ("PaTyc", [p; t]) in
            Node ("PaOlb", [i; p; Option e]) :
            'patt));
      [Gramext.Stoken ("", "?");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Stoken ("", "("); Gramext.Sself;
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("", "=");
              Gramext.Snterm
                (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
             Gramext.action
               (fun (e : 'expr) _ (loc : int * int) -> (e : 'e__18))]);
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (e : 'e__18 option) (p : 'patt) _ _ (i : 'lident) _
           (loc : int * int) ->
           (Node ("PaOlb", [i; p; Option e]) : 'patt));
      [Gramext.Stoken ("", "~");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e))],
      Gramext.action
        (fun (i : 'lident) _ (loc : int * int) ->
           (Node ("PaLab", [i; Node ("PaLid", [i])]) : 'patt));
      [Gramext.Stoken ("", "~");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Sself],
      Gramext.action
        (fun (p : 'patt) _ (i : 'lident) _ (loc : int * int) ->
           (Node ("PaLab", [i; p]) : 'patt))];
     Some "simple", None,
     [[Gramext.Stoken ("", "_")],
      Gramext.action
        (fun _ (loc : int * int) -> (Node ("PaAny", []) : 'patt));
      [Gramext.Stoken ("", "(");
       Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (pl : 'anti_list) _ (loc : int * int) ->
           (Node ("PaTup", [pl]) : 'patt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ",");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e)),
              Gramext.Stoken ("", ","))],
          Gramext.action
            (fun (l : 'patt list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (pl : ast) _ (p : 'patt) _ (loc : int * int) ->
           (Node ("PaTup", [Cons (p, pl)]) : 'patt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", "as");
       Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (p2 : 'patt) _ (p : 'patt) _ (loc : int * int) ->
           (Node ("PaAli", [p; p2]) : 'patt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (t : 'ctyp) _ (p : 'patt) _ (loc : int * int) ->
           (Node ("PaTyc", [p; t]) : 'patt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action (fun _ (p : 'patt) _ (loc : int * int) -> (p : 'patt));
      [Gramext.Stoken ("", "("); Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ _ (loc : int * int) -> (Node ("PaUid", [Str "()"]) : 'patt));
      [Gramext.Stoken ("", "{");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (label_patt : 'label_patt Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'label_patt list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (lpl : ast) _ (loc : int * int) ->
           (Node ("PaRec", [lpl]) : 'patt));
      [Gramext.Stoken ("", "[|");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'patt list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "|]")],
      Gramext.action
        (fun _ (pl : ast) _ (loc : int * int) ->
           (Node ("PaArr", [pl]) : 'patt));
      [Gramext.Stoken ("", "[");
       Gramext.Slist1sep
         (Gramext.Snterm (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e)),
          Gramext.Stoken ("", ";"));
       Gramext.Sopt
         (Gramext.srules
            [[Gramext.Stoken ("", "::");
              Gramext.Snterm
                (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e))],
             Gramext.action
               (fun (p : 'patt) _ (loc : int * int) -> (p : 'e__20))]);
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (last : 'e__20 option) (pl : 'patt list) _ (loc : int * int) ->
           (mklistpat last pl : 'patt));
      [Gramext.Stoken ("", "["); Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ _ (loc : int * int) -> (Node ("PaUid", [Str "[]"]) : 'patt));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'patt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_anti : 'anti_anti Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_anti) (loc : int * int) ->
           (Node ("PaAnt", [a]) : 'patt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_chr : 'anti_chr Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_chr) (loc : int * int) ->
           (Node ("PaChr", [a]) : 'patt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_str : 'anti_str Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_str) (loc : int * int) ->
           (Node ("PaStr", [a]) : 'patt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_flo : 'anti_flo Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_flo) (loc : int * int) ->
           (Node ("PaFlo", [a]) : 'patt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_int : 'anti_int Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_int) (loc : int * int) ->
           (Node ("PaInt", [a]) : 'patt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_uid) (loc : int * int) ->
           (Node ("PaUid", [a]) : 'patt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_lid) (loc : int * int) ->
           (Node ("PaLid", [a]) : 'patt));
      [Gramext.Stoken ("", "#");
       Gramext.Snterm
         (Grammar.Entry.obj (mod_ident : 'mod_ident Grammar.Entry.e))],
      Gramext.action
        (fun (s : 'mod_ident) _ (loc : int * int) ->
           (Node ("PaTyp", [s]) : 'patt));
      [Gramext.Stoken ("", "#");
       Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_list) _ (loc : int * int) ->
           (Node ("PaTyp", [a]) : 'patt));
      [Gramext.Stoken ("", "`");
       Gramext.Snterm (Grammar.Entry.obj (ident : 'ident Grammar.Entry.e))],
      Gramext.action
        (fun (s : 'ident) _ (loc : int * int) ->
           (Node ("PaVrn", [s]) : 'patt));
      [Gramext.Stoken ("CHAR", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("PaChr", [Chr s]) : 'patt));
      [Gramext.Stoken ("STRING", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("PaStr", [Str s]) : 'patt));
      [Gramext.Stoken ("FLOAT", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("PaFlo", [Str s]) : 'patt));
      [Gramext.Stoken ("", "-"); Gramext.Stoken ("INT", "")],
      Gramext.action
        (fun (s : string) _ (loc : int * int) ->
           (Node ("PaInt", [Str (neg s)]) : 'patt));
      [Gramext.Stoken ("INT", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) ->
           (Node ("PaInt", [Str s]) : 'patt));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (v : string) (loc : int * int) ->
           (Node ("PaUid", [Str v]) : 'patt));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (v : string) (loc : int * int) ->
           (Node ("PaLid", [Str v]) : 'patt))]];
    Grammar.Entry.obj (label_patt : 'label_patt Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj
            (patt_label_ident : 'patt_label_ident Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e))],
      Gramext.action
        (fun (p : 'patt) _ (i : 'patt_label_ident) (loc : int * int) ->
           (Tuple [i; p] : 'label_patt))]];
    Grammar.Entry.obj (patt_label_ident : 'patt_label_ident Grammar.Entry.e),
    None,
    [None, Some Gramext.LeftA,
     [[Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (p2 : 'patt_label_ident) _ (p1 : 'patt_label_ident)
           (loc : int * int) ->
           (Node ("PaAcc", [p1; p2]) : 'patt_label_ident))];
     None, Some Gramext.RightA,
     [[Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (Node ("PaLid", [Str i]) : 'patt_label_ident));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) ->
           (Node ("PaUid", [Str i]) : 'patt_label_ident));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_uid) (loc : int * int) ->
           (Node ("PaUid", [a]) : 'patt_label_ident));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_lid) (loc : int * int) ->
           (Node ("PaLid", [a]) : 'patt_label_ident));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) (loc : int * int) -> (a : 'patt_label_ident))]];
    Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "_")],
      Gramext.action
        (fun _ (loc : int * int) -> (Node ("PaAny", []) : 'ipatt));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'ipatt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_anti : 'anti_anti Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_anti) (loc : int * int) ->
           (Node ("PaAnt", [a]) : 'ipatt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_lid) (loc : int * int) ->
           (Node ("PaLid", [a]) : 'ipatt));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (v : string) (loc : int * int) ->
           (Node ("PaLid", [Str v]) : 'ipatt));
      [Gramext.Stoken ("", "(");
       Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (pl : 'anti_list) _ (loc : int * int) ->
           (Node ("PaTup", [pl]) : 'ipatt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ",");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e)),
              Gramext.Stoken ("", ","))],
          Gramext.action
            (fun (l : 'ipatt list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (pl : ast) _ (p : 'ipatt) _ (loc : int * int) ->
           (Node ("PaTup", [Cons (p, pl)]) : 'ipatt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", "as");
       Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (p2 : 'ipatt) _ (p1 : 'ipatt) _ (loc : int * int) ->
           (Node ("PaAli", [p1; p2]) : 'ipatt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (t : 'ctyp) _ (p : 'ipatt) _ (loc : int * int) ->
           (Node ("PaTyc", [p; t]) : 'ipatt));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action (fun _ (p : 'ipatt) _ (loc : int * int) -> (p : 'ipatt));
      [Gramext.Stoken ("", "("); Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ _ (loc : int * int) -> (Node ("PaUid", [Str "()"]) : 'ipatt));
      [Gramext.Stoken ("", "{");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (label_ipatt : 'label_ipatt Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'label_ipatt list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (lpl : ast) _ (loc : int * int) ->
           (Node ("PaRec", [lpl]) : 'ipatt))]];
    Grammar.Entry.obj (label_ipatt : 'label_ipatt Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj
            (patt_label_ident : 'patt_label_ident Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (ipatt : 'ipatt Grammar.Entry.e))],
      Gramext.action
        (fun (p : 'ipatt) _ (i : 'patt_label_ident) (loc : int * int) ->
           (Tuple [i; p] : 'label_ipatt))]];
    Grammar.Entry.obj (type_declaration : 'type_declaration Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (type_parameter : 'type_parameter Grammar.Entry.e)))],
          Gramext.action
            (fun (l : 'type_parameter list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (constrain : 'constrain Grammar.Entry.e)))],
          Gramext.action
            (fun (l : 'constrain list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (cl : ast) (tk : 'ctyp) _ (tpl : ast) (n : 'lident)
           (loc : int * int) ->
           (Tuple [n; tpl; tk; cl] : 'type_declaration))]];
    Grammar.Entry.obj (constrain : 'constrain Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "constraint");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t2 : 'ctyp) _ (t1 : 'ctyp) _ (loc : int * int) ->
           (Tuple [t1; t2] : 'constrain))]];
    Grammar.Entry.obj (type_parameter : 'type_parameter Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Stoken ("", "-"); Gramext.Stoken ("", "'");
       Gramext.Snterm (Grammar.Entry.obj (ident : 'ident Grammar.Entry.e))],
      Gramext.action
        (fun (i : 'ident) _ _ (loc : int * int) ->
           (Tuple [i; Tuple [Bool false; Bool true]] : 'type_parameter));
      [Gramext.Stoken ("", "+"); Gramext.Stoken ("", "'");
       Gramext.Snterm (Grammar.Entry.obj (ident : 'ident Grammar.Entry.e))],
      Gramext.action
        (fun (i : 'ident) _ _ (loc : int * int) ->
           (Tuple [i; Tuple [Bool true; Bool false]] : 'type_parameter));
      [Gramext.Stoken ("", "'");
       Gramext.Snterm (Grammar.Entry.obj (ident : 'ident Grammar.Entry.e))],
      Gramext.action
        (fun (i : 'ident) _ (loc : int * int) ->
           (Tuple [i; Tuple [Bool false; Bool false]] : 'type_parameter))]];
    Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e), None,
    [None, Some Gramext.LeftA,
     [[Gramext.Sself; Gramext.Stoken ("", "=="); Gramext.Sself],
      Gramext.action
        (fun (t2 : 'ctyp) _ (t1 : 'ctyp) (loc : int * int) ->
           (Node ("TyMan", [t1; t2]) : 'ctyp))];
     None, Some Gramext.LeftA,
     [[Gramext.Sself; Gramext.Stoken ("", "as"); Gramext.Sself],
      Gramext.action
        (fun (t2 : 'ctyp) _ (t1 : 'ctyp) (loc : int * int) ->
           (Node ("TyAli", [t1; t2]) : 'ctyp))];
     None, Some Gramext.RightA,
     [[Gramext.Sself; Gramext.Stoken ("", "->"); Gramext.Sself],
      Gramext.action
        (fun (t2 : 'ctyp) _ (t1 : 'ctyp) (loc : int * int) ->
           (Node ("TyArr", [t1; t2]) : 'ctyp))];
     None, Some Gramext.NonA,
     [[Gramext.Stoken ("", "?");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Sself],
      Gramext.action
        (fun (t : 'ctyp) _ (a : 'lident) _ (loc : int * int) ->
           (Node ("TyOlb", [a; t]) : 'ctyp));
      [Gramext.Stoken ("", "~");
       Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e));
       Gramext.Stoken ("", ":"); Gramext.Sself],
      Gramext.action
        (fun (t : 'ctyp) _ (a : 'anti_) _ (loc : int * int) ->
           (Node ("TyLab", [a; t]) : 'ctyp));
      [Gramext.Stoken ("TILDEIDENTCOLON", ""); Gramext.Stoken ("", ":");
       Gramext.Sself],
      Gramext.action
        (fun (t : 'ctyp) _ (a : string) (loc : int * int) ->
           (Node ("TyLab", [Str a; t]) : 'ctyp))];
     None, Some Gramext.LeftA,
     [[Gramext.Sself; Gramext.Sself],
      Gramext.action
        (fun (t2 : 'ctyp) (t1 : 'ctyp) (loc : int * int) ->
           (Node ("TyApp", [t1; t2]) : 'ctyp))];
     None, Some Gramext.LeftA,
     [[Gramext.Sself; Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (t2 : 'ctyp) _ (t1 : 'ctyp) (loc : int * int) ->
           (Node ("TyAcc", [t1; t2]) : 'ctyp))];
     Some "simple", None,
     [[Gramext.Stoken ("", "[|"); Gramext.Stoken ("", "<");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (row_field : 'row_field Grammar.Entry.e)),
              Gramext.Stoken ("", "|"))],
          Gramext.action
            (fun (l : 'row_field list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Snterm
         (Grammar.Entry.obj (opt_tag_list : 'opt_tag_list Grammar.Entry.e));
       Gramext.Stoken ("", "|]")],
      Gramext.action
        (fun _ (sl : 'opt_tag_list) (rfl : ast) _ _ (loc : int * int) ->
           (Node ("TyVrn", [rfl; Option (Some (Option (Some sl)))]) : 'ctyp));
      [Gramext.Stoken ("", "[|"); Gramext.Stoken ("", ">");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (row_field : 'row_field Grammar.Entry.e)),
              Gramext.Stoken ("", "|"))],
          Gramext.action
            (fun (l : 'row_field list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "|]")],
      Gramext.action
        (fun _ (rfl : ast) _ _ (loc : int * int) ->
           (Node ("TyVrn", [rfl; Option (Some (Option None))]) : 'ctyp));
      [Gramext.Stoken ("", "[|");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj (row_field : 'row_field Grammar.Entry.e)),
              Gramext.Stoken ("", "|"))],
          Gramext.action
            (fun (l : 'row_field list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "|]")],
      Gramext.action
        (fun _ (rfl : ast) _ (loc : int * int) ->
           (Node ("TyVrn", [rfl; Option None]) : 'ctyp));
      [Gramext.Stoken ("", "{");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (label_declaration : 'label_declaration Grammar.Entry.e)),
              Gramext.Stoken ("", ";"))],
          Gramext.action
            (fun (l : 'label_declaration list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "}")],
      Gramext.action
        (fun _ (ldl : ast) _ (loc : int * int) ->
           (Node ("TyRec", [ldl]) : 'ctyp));
      [Gramext.Stoken ("", "[");
       Gramext.srules
         [[Gramext.Slist0sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (constructor_declaration :
                    'constructor_declaration Grammar.Entry.e)),
              Gramext.Stoken ("", "|"))],
          Gramext.action
            (fun (l : 'constructor_declaration list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (cdl : ast) _ (loc : int * int) ->
           (Node ("TySum", [cdl]) : 'ctyp));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action (fun _ (t : 'ctyp) _ (loc : int * int) -> (t : 'ctyp));
      [Gramext.Stoken ("", "(");
       Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (tl : 'anti_list) _ (loc : int * int) ->
           (Node ("TyTup", [tl]) : 'ctyp));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", "*");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e)),
              Gramext.Stoken ("", "*"))],
          Gramext.action
            (fun (l : 'ctyp list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (tl : ast) _ (t : 'ctyp) _ (loc : int * int) ->
           (Node ("TyTup", [Cons (t, tl)]) : 'ctyp));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'ctyp));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_uid) (loc : int * int) ->
           (Node ("TyUid", [a]) : 'ctyp));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_lid) (loc : int * int) ->
           (Node ("TyLid", [a]) : 'ctyp));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (Node ("TyUid", [Str a]) : 'ctyp));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (Node ("TyLid", [Str a]) : 'ctyp));
      [Gramext.Stoken ("", "_")],
      Gramext.action
        (fun _ (loc : int * int) -> (Node ("TyAny", []) : 'ctyp));
      [Gramext.Stoken ("", "'");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'lident) _ (loc : int * int) ->
           (Node ("TyQuo", [a]) : 'ctyp))]];
    Grammar.Entry.obj (row_field : 'row_field Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "`");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", "of"); Gramext.Sopt (Gramext.Stoken ("", "&"));
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e)),
              Gramext.Stoken ("", "&"))],
          Gramext.action
            (fun (l : 'ctyp list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (l : ast) (oa : string option) _ (i : 'lident) _
           (loc : int * int) ->
           (Tuple [i; Bool (oa <> None); l] : 'row_field));
      [Gramext.Stoken ("", "`");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e))],
      Gramext.action
        (fun (i : 'lident) _ (loc : int * int) ->
           (Tuple [i; Bool true; List []] : 'row_field))]];
    Grammar.Entry.obj (opt_tag_list : 'opt_tag_list Grammar.Entry.e), None,
    [None, None,
     [[], Gramext.action (fun (loc : int * int) -> (List [] : 'opt_tag_list));
      [Gramext.Stoken ("", ">");
       Gramext.srules
         [[Gramext.Slist1
             (Gramext.Snterm
                (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e)))],
          Gramext.action
            (fun (l : 'lident list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (sl : ast) _ (loc : int * int) -> (sl : 'opt_tag_list))]];
    Grammar.Entry.obj
      (constructor_declaration : 'constructor_declaration Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e))],
      Gramext.action
        (fun (ci : 'uident) (loc : int * int) ->
           (Tuple [Loc; ci; List []] : 'constructor_declaration));
      [Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", "of");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'ctyp list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (cal : ast) _ (ci : 'uident) (loc : int * int) ->
           (Tuple [Loc; ci; cal] : 'constructor_declaration))]];
    Grammar.Entry.obj
      (label_declaration : 'label_declaration Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (mutable_flag : 'mutable_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) (mf : 'mutable_flag) _ (i : 'lident)
           (loc : int * int) ->
           (Tuple [Loc; i; mf; t] : 'label_declaration))]];
    Grammar.Entry.obj (ident : 'ident Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'ident));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action (fun (i : string) (loc : int * int) -> (Str i : 'ident));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) -> (Str i : 'ident))]];
    Grammar.Entry.obj (lident : 'lident Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'lident));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) -> (Str i : 'lident))]];
    Grammar.Entry.obj (uident : 'uident Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'uident));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) -> (Str i : 'uident))]];
    Grammar.Entry.obj (mod_ident : 'mod_ident Grammar.Entry.e), None,
    [None, Some Gramext.RightA,
     [[Gramext.Stoken ("UIDENT", ""); Gramext.Stoken ("", ".");
       Gramext.Sself],
      Gramext.action
        (fun (i : 'mod_ident) _ (m : string) (loc : int * int) ->
           (Cons (Str m, i) : 'mod_ident));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e));
       Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (i : 'mod_ident) _ (m : 'anti_uid) (loc : int * int) ->
           (Cons (m, i) : 'mod_ident));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e))],
      Gramext.action
        (fun (m : 'anti_lid) (loc : int * int) -> (List [m] : 'mod_ident));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (i : 'anti_) (loc : int * int) -> (i : 'mod_ident));
      [Gramext.Stoken ("LIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) -> (List [Str i] : 'mod_ident));
      [Gramext.Stoken ("UIDENT", "")],
      Gramext.action
        (fun (i : string) (loc : int * int) -> (List [Str i] : 'mod_ident))]];
    Grammar.Entry.obj (direction_flag : 'direction_flag Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (anti_to : 'anti_to Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_to) (loc : int * int) -> (a : 'direction_flag));
      [Gramext.Stoken ("", "downto")],
      Gramext.action
        (fun _ (loc : int * int) -> (Bool false : 'direction_flag));
      [Gramext.Stoken ("", "to")],
      Gramext.action
        (fun _ (loc : int * int) -> (Bool true : 'direction_flag))]];
    Grammar.Entry.obj (string : 'string Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_) (loc : int * int) -> (a : 'string));
      [Gramext.Stoken ("STRING", "")],
      Gramext.action
        (fun (s : string) (loc : int * int) -> (Str s : 'string))]];
    Grammar.Entry.obj (rec_flag : 'rec_flag Grammar.Entry.e), None,
    [None, None,
     [[], Gramext.action (fun (loc : int * int) -> (Bool false : 'rec_flag));
      [Gramext.Stoken ("", "rec")],
      Gramext.action (fun _ (loc : int * int) -> (Bool true : 'rec_flag));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_rec : 'anti_rec Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_rec) (loc : int * int) -> (a : 'rec_flag))]];
    Grammar.Entry.obj (as_opt : 'as_opt Grammar.Entry.e), None,
    [None, None,
     [[], Gramext.action (fun (loc : int * int) -> (Option None : 'as_opt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_as : 'anti_as Grammar.Entry.e))],
      Gramext.action (fun (a : 'anti_as) (loc : int * int) -> (a : 'as_opt));
      [Gramext.Stoken ("", "as");
       Gramext.Snterm (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e))],
      Gramext.action
        (fun (p : 'patt) _ (loc : int * int) ->
           (Option (Some p) : 'as_opt))]];
    Grammar.Entry.obj (when_opt : 'when_opt Grammar.Entry.e), None,
    [None, None,
     [[], Gramext.action (fun (loc : int * int) -> (Option None : 'when_opt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_when : 'anti_when Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_when) (loc : int * int) -> (a : 'when_opt));
      [Gramext.Stoken ("", "when");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (loc : int * int) ->
           (Option (Some e) : 'when_opt))]];
    Grammar.Entry.obj (mutable_flag : 'mutable_flag Grammar.Entry.e), None,
    [None, None,
     [[],
      Gramext.action (fun (loc : int * int) -> (Bool false : 'mutable_flag));
      [Gramext.Stoken ("", "mutable")],
      Gramext.action (fun _ (loc : int * int) -> (Bool true : 'mutable_flag));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_mut : 'anti_mut Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_mut) (loc : int * int) -> (a : 'mutable_flag))]];
    Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "" loc a : 'anti_))]];
    Grammar.Entry.obj (anti_anti : 'anti_anti Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "anti")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "anti" loc a : 'anti_anti))]];
    Grammar.Entry.obj (anti_as : 'anti_as Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "as")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "as" loc a : 'anti_as))]];
    Grammar.Entry.obj (anti_chr : 'anti_chr Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "chr")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "chr" loc a : 'anti_chr))]];
    Grammar.Entry.obj (anti_exp : 'anti_exp Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "exp")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "exp" loc a : 'anti_exp))]];
    Grammar.Entry.obj (anti_flo : 'anti_flo Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "flo")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "flo" loc a : 'anti_flo))]];
    Grammar.Entry.obj (anti_int : 'anti_int Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "int")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "int" loc a : 'anti_int))]];
    Grammar.Entry.obj (anti_lid : 'anti_lid Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "lid")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "lid" loc a : 'anti_lid))]];
    Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "list")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "list" loc a : 'anti_list))]];
    Grammar.Entry.obj (anti_mut : 'anti_mut Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "mut")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "mut" loc a : 'anti_mut))]];
    Grammar.Entry.obj (anti_opt : 'anti_opt Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "opt")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "mut" loc a : 'anti_opt))]];
    Grammar.Entry.obj (anti_rec : 'anti_rec Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "rec")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "rec" loc a : 'anti_rec))]];
    Grammar.Entry.obj (anti_str : 'anti_str Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "str")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "str" loc a : 'anti_str))]];
    Grammar.Entry.obj (anti_to : 'anti_to Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "to")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "to" loc a : 'anti_to))]];
    Grammar.Entry.obj (anti_uid : 'anti_uid Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "uid")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "uid" loc a : 'anti_uid))]];
    Grammar.Entry.obj (anti_when : 'anti_when Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "when")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "when" loc a : 'anti_when))]];
    Grammar.Entry.obj (str_item : 'str_item Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "class"); Gramext.Stoken ("", "type");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (class_type_declaration :
                    'class_type_declaration Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'class_type_declaration list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (ctd : ast) _ _ (loc : int * int) ->
           (Node ("StClt", [ctd]) : 'str_item));
      [Gramext.Stoken ("", "class");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (class_declaration : 'class_declaration Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'class_declaration list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (cd : ast) _ (loc : int * int) ->
           (Node ("StCls", [cd]) : 'str_item))]];
    Grammar.Entry.obj (sig_item : 'sig_item Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "class"); Gramext.Stoken ("", "type");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (class_type_declaration :
                    'class_type_declaration Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'class_type_declaration list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (ctd : ast) _ _ (loc : int * int) ->
           (Node ("SgClt", [ctd]) : 'sig_item));
      [Gramext.Stoken ("", "class");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (class_description : 'class_description Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'class_description list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (cd : ast) _ (loc : int * int) ->
           (Node ("SgCls", [cd]) : 'sig_item))]];
    Grammar.Entry.obj
      (class_declaration : 'class_declaration Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (virtual_flag : 'virtual_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_type_parameters : 'class_type_parameters Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_fun_binding : 'class_fun_binding Grammar.Entry.e))],
      Gramext.action
        (fun (cfb : 'class_fun_binding) (ctp : 'class_type_parameters)
           (i : 'lident) (vf : 'virtual_flag) (loc : int * int) ->
           (Record
              ["ciLoc", Loc; "ciVir", vf; "ciPrm", ctp; "ciNam", i;
               "ciExp", cfb] :
            'class_declaration))]];
    Grammar.Entry.obj
      (class_fun_binding : 'class_fun_binding Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterml
         (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e), "simple");
       Gramext.Sself],
      Gramext.action
        (fun (cfb : 'class_fun_binding) (p : 'patt) (loc : int * int) ->
           (Node ("CeFun", [p; cfb]) : 'class_fun_binding));
      [Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (class_type : 'class_type Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (class_expr : 'class_expr Grammar.Entry.e))],
      Gramext.action
        (fun (ce : 'class_expr) _ (ct : 'class_type) _ (loc : int * int) ->
           (Node ("CeTyc", [ce; ct]) : 'class_fun_binding));
      [Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (class_expr : 'class_expr Grammar.Entry.e))],
      Gramext.action
        (fun (ce : 'class_expr) _ (loc : int * int) ->
           (ce : 'class_fun_binding))]];
    Grammar.Entry.obj
      (class_type_parameters : 'class_type_parameters Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Stoken ("", "[");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (type_parameter : 'type_parameter Grammar.Entry.e)),
              Gramext.Stoken ("", ","))],
          Gramext.action
            (fun (l : 'type_parameter list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (tpl : ast) _ (loc : int * int) ->
           (Tuple [Loc; tpl] : 'class_type_parameters));
      [],
      Gramext.action
        (fun (loc : int * int) ->
           (Tuple [Loc; List []] : 'class_type_parameters))]];
    Grammar.Entry.obj (class_fun_def : 'class_fun_def Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterml
         (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e), "simple");
       Gramext.Sself],
      Gramext.action
        (fun (cfd : 'class_fun_def) (p : 'patt) (loc : int * int) ->
           (Node ("CeFun", [p; cfd]) : 'class_fun_def));
      [Gramext.Snterml
         (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e), "simple");
       Gramext.Stoken ("", "->");
       Gramext.Snterm
         (Grammar.Entry.obj (class_expr : 'class_expr Grammar.Entry.e))],
      Gramext.action
        (fun (ce : 'class_expr) _ (p : 'patt) (loc : int * int) ->
           (Node ("CeFun", [p; ce]) : 'class_fun_def))]];
    Grammar.Entry.obj (class_expr : 'class_expr Grammar.Entry.e), None,
    [Some "top", None,
     [[Gramext.Stoken ("", "let");
       Gramext.Snterm
         (Grammar.Entry.obj (rec_flag : 'rec_flag Grammar.Entry.e));
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj
                   (let_binding : 'let_binding Grammar.Entry.e)),
              Gramext.Stoken ("", "and"))],
          Gramext.action
            (fun (l : 'let_binding list) (loc : int * int) ->
               (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "in"); Gramext.Sself],
      Gramext.action
        (fun (ce : 'class_expr) _ (lb : ast) (rf : 'rec_flag) _
           (loc : int * int) ->
           (Node ("CeLet", [rf; lb; ce]) : 'class_expr));
      [Gramext.Stoken ("", "fun");
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_fun_def : 'class_fun_def Grammar.Entry.e))],
      Gramext.action
        (fun (cfd : 'class_fun_def) _ (loc : int * int) ->
           (cfd : 'class_expr))];
     Some "apply", Some Gramext.NonA,
     [[Gramext.Sself;
       Gramext.Snterml
         (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e), "simple")],
      Gramext.action
        (fun (e : 'expr) (ce : 'class_expr) (loc : int * int) ->
           (Node ("CeApp", [ce; e]) : 'class_expr))];
     Some "simple", None,
     [[Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (ce : 'class_expr) _ (loc : int * int) -> (ce : 'class_expr));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (class_type : 'class_type Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (ct : 'class_type) _ (ce : 'class_expr) _ (loc : int * int) ->
           (Node ("CeTyc", [ce; ct]) : 'class_expr));
      [Gramext.Stoken ("", "object");
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_self_patt_opt : 'class_self_patt_opt Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_structure : 'class_structure Grammar.Entry.e));
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (cf : 'class_structure) (csp : 'class_self_patt_opt) _
           (loc : int * int) ->
           (Node ("CeStr", [csp; cf]) : 'class_expr));
      [Gramext.Snterm
         (Grammar.Entry.obj
            (class_longident : 'class_longident Grammar.Entry.e))],
      Gramext.action
        (fun (ci : 'class_longident) (loc : int * int) ->
           (Node ("CeCon", [ci; List []]) : 'class_expr));
      [Gramext.Snterm
         (Grammar.Entry.obj
            (class_longident : 'class_longident Grammar.Entry.e));
       Gramext.Stoken ("", "[");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e)),
              Gramext.Stoken ("", ","))],
          Gramext.action
            (fun (l : 'ctyp list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (ctcl : ast) _ (ci : 'class_longident) (loc : int * int) ->
           (Node ("CeCon", [ci; ctcl]) : 'class_expr));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) (loc : int * int) -> (a : 'class_expr))]];
    Grammar.Entry.obj (class_structure : 'class_structure Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (class_str_item : 'class_str_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (cf : 'class_str_item) (loc : int * int) ->
                      (cf : 'e__21))])],
          Gramext.action
            (fun (l : 'e__21 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))]],
      Gramext.action
        (fun (cf : ast) (loc : int * int) -> (cf : 'class_structure))]];
    Grammar.Entry.obj
      (class_self_patt_opt : 'class_self_patt_opt Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (t : 'ctyp) _ (p : 'patt) _ (loc : int * int) ->
           (Option (Some (Node ("PaTyc", [p; t]))) : 'class_self_patt_opt));
      [Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (p : 'patt) _ (loc : int * int) ->
           (Option (Some p) : 'class_self_patt_opt));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) (loc : int * int) -> (a : 'class_self_patt_opt))]];
    Grammar.Entry.obj (class_str_item : 'class_str_item Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Stoken ("", "initializer");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (se : 'expr) _ (loc : int * int) ->
           (Node ("CrIni", [se]) : 'class_str_item));
      [Gramext.Stoken ("", "type");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t2 : 'ctyp) _ (t1 : 'ctyp) _ (loc : int * int) ->
           (Node ("CrCtr", [t1; t2]) : 'class_str_item));
      [Gramext.Stoken ("", "method");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (fun_binding : 'fun_binding Grammar.Entry.e))],
      Gramext.action
        (fun (fb : 'fun_binding) (l : 'label) _ (loc : int * int) ->
           (Node ("CrMth", [l; Bool false; fb]) : 'class_str_item));
      [Gramext.Stoken ("", "method"); Gramext.Stoken ("", "private");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (fun_binding : 'fun_binding Grammar.Entry.e))],
      Gramext.action
        (fun (fb : 'fun_binding) (l : 'label) _ _ (loc : int * int) ->
           (Node ("CrMth", [l; Bool true; fb]) : 'class_str_item));
      [Gramext.Stoken ("", "method"); Gramext.Stoken ("", "virtual");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (l : 'label) _ _ (loc : int * int) ->
           (Node ("CrVir", [l; Bool false; t]) : 'class_str_item));
      [Gramext.Stoken ("", "method"); Gramext.Stoken ("", "virtual");
       Gramext.Stoken ("", "private");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (l : 'label) _ _ _ (loc : int * int) ->
           (Node ("CrVir", [l; Bool true; t]) : 'class_str_item));
      [Gramext.Stoken ("", "value");
       Gramext.Snterm (Grammar.Entry.obj (cvalue : 'cvalue Grammar.Entry.e))],
      Gramext.action
        (fun (lab, mf, e : 'cvalue) _ (loc : int * int) ->
           (Node ("CrVal", [lab; mf; e]) : 'class_str_item));
      [Gramext.Stoken ("", "inherit");
       Gramext.Snterm
         (Grammar.Entry.obj (class_expr : 'class_expr Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj (as_ident_opt : 'as_ident_opt Grammar.Entry.e))],
      Gramext.action
        (fun (pb : 'as_ident_opt) (ce : 'class_expr) _ (loc : int * int) ->
           (Node ("CrInh", [ce; pb]) : 'class_str_item));
      [Gramext.Stoken ("", "declare");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (class_str_item : 'class_str_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (s : 'class_str_item) (loc : int * int) ->
                      (s : 'e__22))])],
          Gramext.action
            (fun (l : 'e__22 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (st : ast) _ (loc : int * int) ->
           (Node ("CrDcl", [st]) : 'class_str_item))]];
    Grammar.Entry.obj (cvalue : 'cvalue Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (mutable_flag : 'mutable_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":>");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (t : 'ctyp) _ (l : 'label) (mf : 'mutable_flag)
           (loc : int * int) ->
           (l, mf, Node ("ExCoe", [e; Option None; t]) : 'cvalue));
      [Gramext.Snterm
         (Grammar.Entry.obj (mutable_flag : 'mutable_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ":>");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (t2 : 'ctyp) _ (t1 : 'ctyp) _ (l : 'label)
           (mf : 'mutable_flag) (loc : int * int) ->
           (l, mf, Node ("ExCoe", [e; Option (Some t1); t2]) : 'cvalue));
      [Gramext.Snterm
         (Grammar.Entry.obj (mutable_flag : 'mutable_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (t : 'ctyp) _ (l : 'label) (mf : 'mutable_flag)
           (loc : int * int) ->
           (l, mf, Node ("ExTyc", [e; t]) : 'cvalue));
      [Gramext.Snterm
         (Grammar.Entry.obj (mutable_flag : 'mutable_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (l : 'label) (mf : 'mutable_flag)
           (loc : int * int) ->
           (l, mf, e : 'cvalue))]];
    Grammar.Entry.obj (label : 'label Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e))],
      Gramext.action (fun (i : 'lident) (loc : int * int) -> (i : 'label))]];
    Grammar.Entry.obj (class_type : 'class_type Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "object");
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_self_type_opt : 'class_self_type_opt Grammar.Entry.e));
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (class_sig_item : 'class_sig_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (csf : 'class_sig_item) (loc : int * int) ->
                      (csf : 'e__23))])],
          Gramext.action
            (fun (l : 'e__23 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (csf : ast) (cst : 'class_self_type_opt) _ (loc : int * int) ->
           (Node ("CtSig", [cst; csf]) : 'class_type));
      [Gramext.Snterm
         (Grammar.Entry.obj
            (clty_longident : 'clty_longident Grammar.Entry.e))],
      Gramext.action
        (fun (id : 'clty_longident) (loc : int * int) ->
           (Node ("CtCon", [id; List []]) : 'class_type));
      [Gramext.Snterm
         (Grammar.Entry.obj
            (clty_longident : 'clty_longident Grammar.Entry.e));
       Gramext.Stoken ("", "[");
       Gramext.srules
         [[Gramext.Slist1sep
             (Gramext.Snterm
                (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e)),
              Gramext.Stoken ("", ","))],
          Gramext.action
            (fun (l : 'ctyp list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "]")],
      Gramext.action
        (fun _ (tl : ast) _ (id : 'clty_longident) (loc : int * int) ->
           (Node ("CtCon", [id; tl]) : 'class_type));
      [Gramext.Stoken ("", "[");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "]"); Gramext.Stoken ("", "->"); Gramext.Sself],
      Gramext.action
        (fun (ct : 'class_type) _ _ (t : 'ctyp) _ (loc : int * int) ->
           (Node ("CtFun", [t; ct]) : 'class_type));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) (loc : int * int) -> (a : 'class_type))]];
    Grammar.Entry.obj
      (class_self_type_opt : 'class_self_type_opt Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Stoken ("", "(");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (t : 'ctyp) _ (loc : int * int) ->
           (Option (Some t) : 'class_self_type_opt));
      [Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_) (loc : int * int) -> (a : 'class_self_type_opt))]];
    Grammar.Entry.obj (class_sig_item : 'class_sig_item Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Stoken ("", "type");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t2 : 'ctyp) _ (t1 : 'ctyp) _ (loc : int * int) ->
           (Node ("CgCtr", [t1; t2]) : 'class_sig_item));
      [Gramext.Stoken ("", "method");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (l : 'label) _ (loc : int * int) ->
           (Node ("CgMth", [l; Bool false; t]) : 'class_sig_item));
      [Gramext.Stoken ("", "method"); Gramext.Stoken ("", "private");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (l : 'label) _ _ (loc : int * int) ->
           (Node ("CgMth", [l; Bool true; t]) : 'class_sig_item));
      [Gramext.Stoken ("", "method"); Gramext.Stoken ("", "virtual");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (l : 'label) _ _ (loc : int * int) ->
           (Node ("CgVir", [l; Bool false; t]) : 'class_sig_item));
      [Gramext.Stoken ("", "method"); Gramext.Stoken ("", "virtual");
       Gramext.Stoken ("", "private");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (l : 'label) _ _ _ (loc : int * int) ->
           (Node ("CgVir", [l; Bool true; t]) : 'class_sig_item));
      [Gramext.Stoken ("", "value");
       Gramext.Snterm
         (Grammar.Entry.obj (mutable_flag : 'mutable_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (l : 'label) (mf : 'mutable_flag) _
           (loc : int * int) ->
           (Node ("CgVal", [l; mf; t]) : 'class_sig_item));
      [Gramext.Stoken ("", "inherit");
       Gramext.Snterm
         (Grammar.Entry.obj (class_type : 'class_type Grammar.Entry.e))],
      Gramext.action
        (fun (cs : 'class_type) _ (loc : int * int) ->
           (Node ("CgInh", [cs]) : 'class_sig_item));
      [Gramext.Stoken ("", "declare");
       Gramext.srules
         [[Gramext.Slist0
             (Gramext.srules
                [[Gramext.Snterm
                    (Grammar.Entry.obj
                       (class_sig_item : 'class_sig_item Grammar.Entry.e));
                  Gramext.Stoken ("", ";")],
                 Gramext.action
                   (fun _ (s : 'class_sig_item) (loc : int * int) ->
                      (s : 'e__24))])],
          Gramext.action
            (fun (l : 'e__24 list) (loc : int * int) -> (list l : 'anti));
          [Gramext.Snterm
             (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
          Gramext.action
            (fun (a : 'anti_list) (loc : int * int) -> (a : 'anti))];
       Gramext.Stoken ("", "end")],
      Gramext.action
        (fun _ (st : ast) _ (loc : int * int) ->
           (Node ("CgDcl", [st]) : 'class_sig_item))]];
    Grammar.Entry.obj
      (class_description : 'class_description Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (virtual_flag : 'virtual_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_type_parameters : 'class_type_parameters Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm
         (Grammar.Entry.obj (class_type : 'class_type Grammar.Entry.e))],
      Gramext.action
        (fun (ct : 'class_type) _ (ctp : 'class_type_parameters) (n : 'lident)
           (vf : 'virtual_flag) (loc : int * int) ->
           (Record
              ["ciLoc", Loc; "ciVir", vf; "ciPrm", ctp; "ciNam", n;
               "ciExp", ct] :
            'class_description))]];
    Grammar.Entry.obj
      (class_type_declaration : 'class_type_declaration Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (virtual_flag : 'virtual_flag Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_type_parameters : 'class_type_parameters Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm
         (Grammar.Entry.obj (class_type : 'class_type Grammar.Entry.e))],
      Gramext.action
        (fun (cs : 'class_type) _ (ctp : 'class_type_parameters) (n : 'lident)
           (vf : 'virtual_flag) (loc : int * int) ->
           (Record
              ["ciLoc", Loc; "ciVir", vf; "ciPrm", ctp; "ciNam", n;
               "ciExp", cs] :
            'class_type_declaration))]];
    Grammar.Entry.obj (expr : 'expr Grammar.Entry.e),
    Some (Gramext.Level "apply"),
    [None, Some Gramext.LeftA,
     [[Gramext.Stoken ("", "new");
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_longident : 'class_longident Grammar.Entry.e))],
      Gramext.action
        (fun (i : 'class_longident) _ (loc : int * int) ->
           (Node ("ExNew", [i]) : 'expr))]];
    Grammar.Entry.obj (expr : 'expr Grammar.Entry.e),
    Some (Gramext.Level "."),
    [None, None,
     [[Gramext.Sself; Gramext.Stoken ("", "#");
       Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e))],
      Gramext.action
        (fun (lab : 'label) _ (e : 'expr) (loc : int * int) ->
           (Node ("ExSnd", [e; lab]) : 'expr))]];
    Grammar.Entry.obj (expr : 'expr Grammar.Entry.e),
    Some (Gramext.Level "simple"),
    [None, None,
     [[Gramext.Stoken ("", "{<");
       Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e));
       Gramext.Stoken ("", ">}")],
      Gramext.action
        (fun _ (fel : 'anti_list) _ (loc : int * int) ->
           (Node ("ExOvr", [fel]) : 'expr));
      [Gramext.Stoken ("", "{<");
       Gramext.Snterm
         (Grammar.Entry.obj
            (field_expr_list : 'field_expr_list Grammar.Entry.e));
       Gramext.Stoken ("", ">}")],
      Gramext.action
        (fun _ (fel : 'field_expr_list) _ (loc : int * int) ->
           (Node ("ExOvr", [List fel]) : 'expr));
      [Gramext.Stoken ("", "{<"); Gramext.Stoken ("", ">}")],
      Gramext.action
        (fun _ _ (loc : int * int) -> (Node ("ExOvr", [List []]) : 'expr));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ":>");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (t : 'ctyp) _ (e : 'expr) _ (loc : int * int) ->
           (Node ("ExCoe", [e; Option None; t]) : 'expr));
      [Gramext.Stoken ("", "("); Gramext.Sself; Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ":>");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
       Gramext.Stoken ("", ")")],
      Gramext.action
        (fun _ (t2 : 'ctyp) _ (t1 : 'ctyp) _ (e : 'expr) _
           (loc : int * int) ->
           (Node ("ExCoe", [e; Option (Some t1); t2]) : 'expr))]];
    Grammar.Entry.obj (field_expr_list : 'field_expr_list Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))],
      Gramext.action
        (fun (e : 'expr) _ (l : 'label) (loc : int * int) ->
           ([Tuple [l; e]] : 'field_expr_list));
      [Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
       Gramext.Stoken ("", ";")],
      Gramext.action
        (fun _ (e : 'expr) _ (l : 'label) (loc : int * int) ->
           ([Tuple [l; e]] : 'field_expr_list));
      [Gramext.Snterm (Grammar.Entry.obj (label : 'label Grammar.Entry.e));
       Gramext.Stoken ("", "=");
       Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
       Gramext.Stoken ("", ";"); Gramext.Sself],
      Gramext.action
        (fun (fel : 'field_expr_list) _ (e : 'expr) _ (l : 'label)
           (loc : int * int) ->
           (Tuple [l; e] :: fel : 'field_expr_list))]];
    Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e),
    Some (Gramext.Level "simple"),
    [None, None,
     [[Gramext.Stoken ("", "<"); Gramext.Stoken ("", ">")],
      Gramext.action
        (fun _ _ (loc : int * int) ->
           (Node ("TyObj", [List []; Bool false]) : 'ctyp));
      [Gramext.Stoken ("", "<");
       Gramext.Snterm
         (Grammar.Entry.obj (meth_list : 'meth_list Grammar.Entry.e));
       Gramext.Stoken ("", ">")],
      Gramext.action
        (fun _ (ml, v : 'meth_list) _ (loc : int * int) ->
           (Node ("TyObj", [ml; v]) : 'ctyp));
      [Gramext.Stoken ("", "#");
       Gramext.Snterm
         (Grammar.Entry.obj
            (class_longident : 'class_longident Grammar.Entry.e))],
      Gramext.action
        (fun (id : 'class_longident) _ (loc : int * int) ->
           (Node ("TyCls", [id]) : 'ctyp))]];
    Grammar.Entry.obj (meth_list : 'meth_list Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("", "..")],
      Gramext.action
        (fun _ (loc : int * int) -> (List [], Bool true : 'meth_list));
      [Gramext.Snterm (Grammar.Entry.obj (field : 'field Grammar.Entry.e))],
      Gramext.action
        (fun (f : 'field) (loc : int * int) ->
           (List [f], Bool false : 'meth_list));
      [Gramext.Snterm (Grammar.Entry.obj (field : 'field Grammar.Entry.e));
       Gramext.Stoken ("", ";")],
      Gramext.action
        (fun _ (f : 'field) (loc : int * int) ->
           (List [f], Bool false : 'meth_list));
      [Gramext.Snterm (Grammar.Entry.obj (field : 'field Grammar.Entry.e));
       Gramext.Stoken ("", ";"); Gramext.Sself],
      Gramext.action
        (fun (ml, v : 'meth_list) _ (f : 'field) (loc : int * int) ->
           (Cons (f, ml), v : 'meth_list));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e));
       Gramext.Snterm (Grammar.Entry.obj (anti_ : 'anti_ Grammar.Entry.e))],
      Gramext.action
        (fun (b : 'anti_) (a : 'anti_list) (loc : int * int) ->
           (a, b : 'meth_list));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_list) (loc : int * int) ->
           (a, Bool false : 'meth_list))]];
    Grammar.Entry.obj (field : 'field Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e));
       Gramext.Stoken ("", ":");
       Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e))],
      Gramext.action
        (fun (t : 'ctyp) _ (lab : 'lident) (loc : int * int) ->
           (Tuple [lab; t] : 'field))]];
    Grammar.Entry.obj (longid : 'longid Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e))],
      Gramext.action (fun (i : 'lident) (loc : int * int) -> ([i] : 'longid));
      [Gramext.Snterm (Grammar.Entry.obj (uident : 'uident Grammar.Entry.e));
       Gramext.Stoken ("", "."); Gramext.Sself],
      Gramext.action
        (fun (l : 'longid) _ (m : 'uident) (loc : int * int) ->
           (m :: l : 'longid))]];
    Grammar.Entry.obj (clty_longident : 'clty_longident Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_list) (loc : int * int) -> (a : 'clty_longident));
      [Gramext.Snterm (Grammar.Entry.obj (longid : 'longid Grammar.Entry.e))],
      Gramext.action
        (fun (l : 'longid) (loc : int * int) -> (List l : 'clty_longident))]];
    Grammar.Entry.obj (class_longident : 'class_longident Grammar.Entry.e),
    None,
    [None, None,
     [[Gramext.Snterm
         (Grammar.Entry.obj (anti_list : 'anti_list Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_list) (loc : int * int) -> (a : 'class_longident));
      [Gramext.Snterm (Grammar.Entry.obj (longid : 'longid Grammar.Entry.e))],
      Gramext.action
        (fun (l : 'longid) (loc : int * int) ->
           (List l : 'class_longident))]];
    Grammar.Entry.obj (virtual_flag : 'virtual_flag Grammar.Entry.e), None,
    [None, None,
     [[],
      Gramext.action (fun (loc : int * int) -> (Bool false : 'virtual_flag));
      [Gramext.Stoken ("", "virtual")],
      Gramext.action (fun _ (loc : int * int) -> (Bool true : 'virtual_flag));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_virt : 'anti_virt Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_virt) (loc : int * int) -> (a : 'virtual_flag))]];
    Grammar.Entry.obj (as_ident_opt : 'as_ident_opt Grammar.Entry.e), None,
    [None, None,
     [[],
      Gramext.action (fun (loc : int * int) -> (Option None : 'as_ident_opt));
      [Gramext.Snterm
         (Grammar.Entry.obj (anti_as : 'anti_as Grammar.Entry.e))],
      Gramext.action
        (fun (a : 'anti_as) (loc : int * int) -> (a : 'as_ident_opt));
      [Gramext.Stoken ("", "as");
       Gramext.Snterm (Grammar.Entry.obj (lident : 'lident Grammar.Entry.e))],
      Gramext.action
        (fun (p : 'lident) _ (loc : int * int) ->
           (Option (Some p) : 'as_ident_opt))]];
    Grammar.Entry.obj (anti_virt : 'anti_virt Grammar.Entry.e), None,
    [None, None,
     [[Gramext.Stoken ("ANTIQUOT", "virt")],
      Gramext.action
        (fun (a : string) (loc : int * int) ->
           (antiquot "virt" loc a : 'anti_virt))]]]);;

let loc = 0, 0;;

let rec expr_of_ast =
  function
    Node (n, al) ->
      List.fold_left (fun e a -> MLast.ExApp (loc, e, expr_of_ast a))
        (MLast.ExApp
           (loc,
            MLast.ExAcc
              (loc, MLast.ExUid (loc, "MLast"), MLast.ExUid (loc, n)),
            MLast.ExLid (loc, !(Stdpp.loc_name))))
        al
  | List al ->
      List.fold_right
        (fun a e ->
           MLast.ExApp
             (loc, MLast.ExApp (loc, MLast.ExUid (loc, "::"), expr_of_ast a),
              e))
        al (MLast.ExUid (loc, "[]"))
  | Tuple al -> MLast.ExTup (loc, List.map expr_of_ast al)
  | Option None -> MLast.ExUid (loc, "None")
  | Option (Some a) ->
      MLast.ExApp (loc, MLast.ExUid (loc, "Some"), expr_of_ast a)
  | Str s -> MLast.ExStr (loc, s)
  | Chr c -> MLast.ExChr (loc, c)
  | Bool true -> MLast.ExUid (loc, "True")
  | Bool false -> MLast.ExUid (loc, "False")
  | Cons (a1, a2) ->
      MLast.ExApp
        (loc, MLast.ExApp (loc, MLast.ExUid (loc, "::"), expr_of_ast a1),
         expr_of_ast a2)
  | Append (a1, a2) ->
      MLast.ExApp
        (loc, MLast.ExApp (loc, MLast.ExLid (loc, "@"), expr_of_ast a1),
         MLast.ExApp
           (loc, MLast.ExApp (loc, MLast.ExUid (loc, "::"), expr_of_ast a2),
            MLast.ExUid (loc, "[]")))
  | Record lal -> MLast.ExRec (loc, List.map label_expr_of_ast lal, None)
  | Loc -> MLast.ExLid (loc, !(Stdpp.loc_name))
  | Antiquot (loc, s) ->
      let e =
        try Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s) with
          Stdpp.Exc_located ((bp, ep), exc) ->
            raise (Stdpp.Exc_located ((fst loc + bp, fst loc + ep), exc))
      in
      MLast.ExAnt (loc, e)
and label_expr_of_ast (l, a) =
  MLast.PaAcc (loc, MLast.PaUid (loc, "MLast"), MLast.PaLid (loc, l)),
  expr_of_ast a
;;

let rec patt_of_ast =
  function
    Node (n, al) ->
      List.fold_left (fun e a -> MLast.PaApp (loc, e, patt_of_ast a))
        (MLast.PaApp
           (loc,
            MLast.PaAcc
              (loc, MLast.PaUid (loc, "MLast"), MLast.PaUid (loc, n)),
            MLast.PaAny loc))
        al
  | List al ->
      List.fold_right
        (fun a p ->
           MLast.PaApp
             (loc, MLast.PaApp (loc, MLast.PaUid (loc, "::"), patt_of_ast a),
              p))
        al (MLast.PaUid (loc, "[]"))
  | Tuple al -> MLast.PaTup (loc, List.map patt_of_ast al)
  | Option None -> MLast.PaUid (loc, "None")
  | Option (Some a) ->
      MLast.PaApp (loc, MLast.PaUid (loc, "Some"), patt_of_ast a)
  | Str s -> MLast.PaStr (loc, s)
  | Chr c -> MLast.PaChr (loc, c)
  | Bool true -> MLast.PaUid (loc, "True")
  | Bool false -> MLast.PaUid (loc, "False")
  | Cons (a1, a2) ->
      MLast.PaApp
        (loc, MLast.PaApp (loc, MLast.PaUid (loc, "::"), patt_of_ast a1),
         patt_of_ast a2)
  | Append (_, _) -> failwith "bad pattern"
  | Record lal -> MLast.PaRec (loc, List.map label_patt_of_ast lal)
  | Loc -> MLast.PaLid (loc, !(Stdpp.loc_name))
  | Antiquot (loc, s) ->
      let p =
        try Grammar.Entry.parse Pcaml.patt_eoi (Stream.of_string s) with
          Stdpp.Exc_located ((bp, ep), exc) ->
            raise (Stdpp.Exc_located ((fst loc + bp, fst loc + ep), exc))
      in
      MLast.PaAnt (loc, p)
and label_patt_of_ast (l, a) =
  MLast.PaAcc (loc, MLast.PaUid (loc, "MLast"), MLast.PaLid (loc, l)),
  patt_of_ast a
;;

let apply_entry e =
  let f s = Grammar.Entry.parse e (Stream.of_string s) in
  let expr s = expr_of_ast (f s) in
  let patt s = patt_of_ast (f s) in Quotation.ExAst (expr, patt)
;;

let sig_item_eoi = Grammar.Entry.create gram "signature item" in
Grammar.extend
  [Grammar.Entry.obj (sig_item_eoi : 'sig_item_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj (sig_item : 'sig_item Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'sig_item) (loc : int * int) -> (x : 'sig_item_eoi))]]];
Quotation.add "sig_item" (apply_entry sig_item_eoi);;

let str_item_eoi = Grammar.Entry.create gram "structure item" in
Grammar.extend
  [Grammar.Entry.obj (str_item_eoi : 'str_item_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj (str_item : 'str_item Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'str_item) (loc : int * int) -> (x : 'str_item_eoi))]]];
Quotation.add "str_item" (apply_entry str_item_eoi);;

let ctyp_eoi = Grammar.Entry.create gram "type" in
Grammar.extend
  [Grammar.Entry.obj (ctyp_eoi : 'ctyp_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm (Grammar.Entry.obj (ctyp : 'ctyp Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'ctyp) (loc : int * int) -> (x : 'ctyp_eoi))]]];
Quotation.add "ctyp" (apply_entry ctyp_eoi);;

let patt_eoi = Grammar.Entry.create gram "pattern" in
Grammar.extend
  [Grammar.Entry.obj (patt_eoi : 'patt_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm (Grammar.Entry.obj (patt : 'patt Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'patt) (loc : int * int) -> (x : 'patt_eoi))]]];
Quotation.add "patt" (apply_entry patt_eoi);;

let expr_eoi = Grammar.Entry.create gram "expression" in
Grammar.extend
  [Grammar.Entry.obj (expr_eoi : 'expr_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'expr) (loc : int * int) -> (x : 'expr_eoi))]]];
Quotation.add "expr" (apply_entry expr_eoi);;

let module_type_eoi = Grammar.Entry.create gram "module type" in
Grammar.extend
  [Grammar.Entry.obj (module_type_eoi : 'module_type_eoi Grammar.Entry.e),
   None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj (module_type : 'module_type Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'module_type) (loc : int * int) ->
          (x : 'module_type_eoi))]]];
Quotation.add "module_type" (apply_entry module_type_eoi);;

let module_expr_eoi = Grammar.Entry.create gram "module expression" in
Grammar.extend
  [Grammar.Entry.obj (module_expr_eoi : 'module_expr_eoi Grammar.Entry.e),
   None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj (module_expr : 'module_expr Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'module_expr) (loc : int * int) ->
          (x : 'module_expr_eoi))]]];
Quotation.add "module_expr" (apply_entry module_expr_eoi);;

let directive_eoi = Grammar.Entry.create gram "directive" in
Grammar.extend
  [Grammar.Entry.obj (directive_eoi : 'directive_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj (directive : 'directive Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'directive) (loc : int * int) -> (x : 'directive_eoi))]]];
Quotation.add "directive" (apply_entry directive_eoi);;

let class_type_eoi = Grammar.Entry.create gram "class_type" in
Grammar.extend
  [Grammar.Entry.obj (class_type_eoi : 'class_type_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj (class_type : 'class_type Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'class_type) (loc : int * int) ->
          (x : 'class_type_eoi))]]];
Quotation.add "class_type" (apply_entry class_type_eoi);;

let class_expr_eoi = Grammar.Entry.create gram "class_expr" in
Grammar.extend
  [Grammar.Entry.obj (class_expr_eoi : 'class_expr_eoi Grammar.Entry.e), None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj (class_expr : 'class_expr Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'class_expr) (loc : int * int) ->
          (x : 'class_expr_eoi))]]];
Quotation.add "class_expr" (apply_entry class_expr_eoi);;

let class_sig_item_eoi = Grammar.Entry.create gram "class_sig_item" in
Grammar.extend
  [Grammar.Entry.obj
     (class_sig_item_eoi : 'class_sig_item_eoi Grammar.Entry.e),
   None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj
           (class_sig_item : 'class_sig_item Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'class_sig_item) (loc : int * int) ->
          (x : 'class_sig_item_eoi))]]];
Quotation.add "class_sig_item" (apply_entry class_sig_item_eoi);;

let class_str_item_eoi = Grammar.Entry.create gram "class_str_item" in
Grammar.extend
  [Grammar.Entry.obj
     (class_str_item_eoi : 'class_str_item_eoi Grammar.Entry.e),
   None,
   [None, None,
    [[Gramext.Snterm
        (Grammar.Entry.obj
           (class_str_item : 'class_str_item Grammar.Entry.e));
      Gramext.Stoken ("EOI", "")],
     Gramext.action
       (fun _ (x : 'class_str_item) (loc : int * int) ->
          (x : 'class_str_item_eoi))]]];
Quotation.add "class_str_item" (apply_entry class_str_item_eoi);;
