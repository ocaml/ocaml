(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let input_file_as_string nom =
  let chanin = open_in_bin nom in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf

let string_of_longident li = String.concat "." (Longident.flatten li)

let string_of_type_expr t =
  let b = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer b in
  Printtyp.mark_loops t;
  Printtyp.type_scheme_max ~b_reset_names: false fmt t;
  Format.pp_print_flush fmt () ;
  let s =  Buffer.contents b in

  Buffer.reset b;
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '\n' -> Buffer.add_string b "\n  "
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

(** Return the given module type where methods and vals have been removed
   from the signatures. Used when we don't want to print a too long module type.*)
let simpl_module_type t =
  let rec iter t =
    match t with
      Types.Tmty_ident p -> t
    | Types.Tmty_signature _ -> Types.Tmty_signature []
    | Types.Tmty_functor (id, mt1, mt2) ->
        Types.Tmty_functor (id, iter mt1, iter mt2)
  in
  iter t

let string_of_module_type ?(complete=false) t =
  let t2 = if complete then t else simpl_module_type t in
  Printtyp.modtype Format.str_formatter t2;
  let s = Format.flush_str_formatter () in
  s


(** Return the given class type where methods and vals have been removed
   from the signatures. Used when we don't want to print a too long class type.*)
let simpl_class_type t =
  let rec iter t =
    match t with
      Types.Tcty_constr (p,texp_list,ct) -> t
    | Types.Tcty_signature cs ->
        (* on vire les vals et methods pour ne pas qu'elles soient imprimées
           quand on affichera le type *)
        let tnil = { Types.desc = Types.Tnil ; Types.level = 0; Types.id = 0 } in
        Types.Tcty_signature { Types.cty_self = { cs.Types.cty_self with 
                                                  Types.desc = Types.Tobject (tnil, ref None) };
                               Types.cty_vars = Types.Vars.empty ;
                               Types.cty_concr = Types.Concr.empty ;
                             }
    | Types.Tcty_fun (l, texp, ct) ->
        let new_ct = iter ct in
        Types.Tcty_fun (l, texp, new_ct)
  in
  iter t

let string_of_class_type ?(complete=false) t = 
  let t2 = if complete then t else simpl_class_type t in
  (* A VOIR : ma propre version de Printtyp.class_type pour ne pas faire reset_names *)
  Printtyp.class_type Format.str_formatter t2;
  let s = Format.flush_str_formatter () in
  s

let get_fields type_expr =
  let (fields, _) = Ctype.flatten_fields (Ctype.object_fields type_expr) in
  List.fold_left
    (fun acc -> fun (label, field_kind, typ) ->
      match field_kind with
        Types.Fabsent ->
          acc
      | _ ->
          if label = "*dummy method*" then
            acc
          else
            acc @ [label, typ]
    )
    []
    fields

let rec string_of_text t =
  let rec iter t_ele = 
    match t_ele with
      | Odoc_types.Raw s
      | Odoc_types.Code s
      | Odoc_types.CodePre s
      | Odoc_types.Verbatim s -> s
      | Odoc_types.Bold t 
      | Odoc_types.Italic t
      | Odoc_types.Center t
      | Odoc_types.Left t
      | Odoc_types.Right t
      | Odoc_types.Emphasize t -> string_of_text t
      | Odoc_types.List l ->
          (String.concat ""
             (List.map (fun t -> "\n- "^(string_of_text t)) l))^
          "\n"
      | Odoc_types.Enum l ->
          let rec f n = function
              [] -> "\n"
            | t :: q ->
                "\n"^(string_of_int n)^". "^(string_of_text t)^
                (f (n + 1) q)
          in
          f 1 l
      | Odoc_types.Newline -> "\n"
      | Odoc_types.Block t -> "\t"^(string_of_text t)^"\n"
      | Odoc_types.Title (_, _, t) -> "\n"^(string_of_text t)^"\n"
      | Odoc_types.Latex s -> "{% "^s^" %}"
      | Odoc_types.Link (s, t) ->
          "["^s^"]"^(string_of_text t)
      | Odoc_types.Ref (name, _) ->
          iter (Odoc_types.Code name)
      | Odoc_types.Superscript t ->
          "^{"^(string_of_text t)^"}"
      | Odoc_types.Subscript t ->
          "^{"^(string_of_text t)^"}"
  in
  String.concat "" (List.map iter t)

let string_of_author_list l =
  match l with
    [] ->
      ""
  | _ ->
      "* "^Odoc_messages.authors^":\n"^
      (String.concat ", " l)^
      "\n"

let string_of_version_opt v_opt =
  match v_opt with
    None -> ""
  | Some v -> Odoc_messages.version^": "^v^"\n"

let string_of_since_opt s_opt =
  match s_opt with
    None -> ""
  | Some s -> Odoc_messages.since^" "^s^"\n"

let string_of_raised_exceptions l =
  match l with
    [] -> ""
  | (s, t) :: [] -> Odoc_messages.raises^" "^s^" "^(string_of_text t)^"\n"
  | _ ->
      Odoc_messages.raises^"\n"^
      (String.concat ""
         (List.map
            (fun (ex, desc) -> "- "^ex^" "^(string_of_text desc)^"\n")
            l
         )
      )^"\n"

let string_of_see (see_ref, t) =
  let t_ref = 
    match see_ref with
      Odoc_types.See_url s -> [ Odoc_types.Link (s, t) ]
    | Odoc_types.See_file s -> (Odoc_types.Code s) :: (Odoc_types.Raw " ") :: t
    | Odoc_types.See_doc s -> (Odoc_types.Italic [Odoc_types.Raw s]) :: (Odoc_types.Raw " ") :: t
  in
  string_of_text t_ref

let string_of_sees l =
  match l with
    [] -> ""
  | see :: [] -> Odoc_messages.see_also^" "^(string_of_see see)^" \n"
  | _ ->
      Odoc_messages.see_also^"\n"^
      (String.concat ""
         (List.map
            (fun see -> "- "^(string_of_see see)^"\n")
            l
         )
      )^"\n"

let string_of_return_opt return_opt =
  match return_opt with
    None -> ""
  | Some s -> Odoc_messages.returns^" "^(string_of_text s)^"\n"

let string_of_info i =
  let module M = Odoc_types in
  (match i.M.i_deprecated with
    None -> ""
  | Some d -> Odoc_messages.deprecated^"! "^(string_of_text d)^"\n")^
  (match i.M.i_desc with
    None -> "" 
  | Some d when d = [Odoc_types.Raw ""] -> ""
  | Some d -> (string_of_text d)^"\n"
  )^
  (string_of_author_list i.M.i_authors)^
  (string_of_version_opt i.M.i_version)^
  (string_of_since_opt i.M.i_since)^
  (string_of_raised_exceptions i.M.i_raised_exceptions)^
  (string_of_return_opt i.M.i_return_value)

let apply_opt f v_opt =
  match v_opt with
    None -> None
  | Some v -> Some (f v)

let string_of_date ?(hour=true) d = 
  let add_0 s = if String.length s < 2 then "0"^s else s in
  let t = Unix.localtime d in
  (string_of_int (t.Unix.tm_year + 1900))^"-"^
  (add_0 (string_of_int (t.Unix.tm_mon + 1)))^"-"^
  (add_0 (string_of_int t.Unix.tm_mday))^
  (
   if hour then 
     " "^
     (add_0 (string_of_int t.Unix.tm_hour))^":"^
     (add_0 (string_of_int t.Unix.tm_min))
   else
     ""
  )


let rec text_list_concat sep l =
  match l with
    [] -> []
  | [t] -> t
  | t :: q ->
      t @ (sep :: (text_list_concat sep q))

let rec text_no_title_no_list t =
  let rec iter t_ele = 
    match t_ele with
    | Odoc_types.Title (_,_,t) -> text_no_title_no_list t
    | Odoc_types.List l
    | Odoc_types.Enum l -> 
        (Odoc_types.Raw " ") ::
        (text_list_concat
           (Odoc_types.Raw ", ") 
           (List.map text_no_title_no_list l))
    | Odoc_types.Raw _
    | Odoc_types.Code _
    | Odoc_types.CodePre _
    | Odoc_types.Verbatim _
    | Odoc_types.Ref _ -> [t_ele]
    | Odoc_types.Newline ->  [Odoc_types.Newline]
    | Odoc_types.Block t -> [Odoc_types.Block (text_no_title_no_list t)]
    | Odoc_types.Bold t -> [Odoc_types.Bold (text_no_title_no_list t)]
    | Odoc_types.Italic t -> [Odoc_types.Italic (text_no_title_no_list t)]
    | Odoc_types.Center t -> [Odoc_types.Center (text_no_title_no_list t)]
    | Odoc_types.Left t -> [Odoc_types.Left (text_no_title_no_list t)]
    | Odoc_types.Right t -> [Odoc_types.Right (text_no_title_no_list t)]
    | Odoc_types.Emphasize t -> [Odoc_types.Emphasize (text_no_title_no_list t)]
    | Odoc_types.Latex s -> [Odoc_types.Latex s]
    | Odoc_types.Link (s, t) -> [Odoc_types.Link (s, (text_no_title_no_list t))]
    | Odoc_types.Superscript t -> [Odoc_types.Superscript (text_no_title_no_list t)]
    | Odoc_types.Subscript t -> [Odoc_types.Subscript (text_no_title_no_list t)]
  in
  List.flatten (List.map iter t)

let get_titles_in_text t =
  let l = ref [] in
  let rec iter_ele ele =
    match ele with
    | Odoc_types.Title (n,lopt,t) -> l := (n,lopt,t) :: !l
    | Odoc_types.List l
    | Odoc_types.Enum l -> List.iter iter_text l        
    | Odoc_types.Raw _
    | Odoc_types.Code _
    | Odoc_types.CodePre _
    | Odoc_types.Verbatim _
    | Odoc_types.Ref _ -> ()
    | Odoc_types.Newline ->  ()
    | Odoc_types.Block t
    | Odoc_types.Bold t
    | Odoc_types.Italic t
    | Odoc_types.Center t
    | Odoc_types.Left t
    | Odoc_types.Right t
    | Odoc_types.Emphasize t -> iter_text t
    | Odoc_types.Latex s -> ()
    | Odoc_types.Link (_, t)
    | Odoc_types.Superscript t 
    | Odoc_types.Subscript t  -> iter_text t
  and iter_text te = 
    List.iter iter_ele te
  in
  iter_text t;
  List.rev !l


(*********************************************************)
let rec get_before_dot s =
  try
    let len = String.length s in
    let n = String.index s '.' in
    if n + 1 >= len then
      (* le point est le dernier caractère *)
      (true, s, "")
    else
      match s.[n+1] with
        ' ' | '\n' | '\r' | '\t' ->
          (true, String.sub s 0 (n+1),
           String.sub s (n+1) (len - n - 1))
      | _ ->
          let b, s2, s_after = get_before_dot (String.sub s (n + 1) (len - n - 1)) in
          (b, (String.sub s 0 (n+1))^s2, s_after)
  with
    Not_found -> (false, s, "")

let rec first_sentence_text t =
  match t with
    [] -> (false, [], [])
  | ele :: q ->
      let (stop, ele2, ele3_opt) = first_sentence_text_ele ele in
      if stop then 
        (stop, [ele2], 
         match ele3_opt with None -> q | Some e -> e :: q)
      else
        let (stop2, q2, rest) = first_sentence_text q in
        (stop2, ele2 :: q2, rest)


and first_sentence_text_ele text_ele =
  match text_ele with
  | Odoc_types.Raw s -> 
      let b, s2, s_after = get_before_dot s in
      (b, Odoc_types.Raw s2, Some (Odoc_types.Raw s_after))
  | Odoc_types.Code _ 
  | Odoc_types.CodePre _ 
  | Odoc_types.Verbatim _ -> (false, text_ele, None)
  | Odoc_types.Bold t ->
      let (b, t2, t3) = first_sentence_text t in
      (b, Odoc_types.Bold t2, Some (Odoc_types.Bold t3))
  | Odoc_types.Italic t ->
      let (b, t2, t3) = first_sentence_text t in
      (b, Odoc_types.Italic t2, Some (Odoc_types.Italic t3))
  | Odoc_types.Center t ->
      let (b, t2, t3) = first_sentence_text t in
      (b, Odoc_types.Center t2, Some (Odoc_types.Center t3))
  | Odoc_types.Left t ->
      let (b, t2, t3) = first_sentence_text t in
      (b, Odoc_types.Left t2, Some (Odoc_types.Left t3))
  | Odoc_types.Right t ->
      let (b, t2, t3) = first_sentence_text t in
      (b, Odoc_types.Right t2, Some (Odoc_types.Right t3))
  | Odoc_types.Emphasize t ->
      let (b, t2, t3) = first_sentence_text t in
      (b, Odoc_types.Emphasize t2, Some (Odoc_types.Emphasize t3))
  | Odoc_types.Block t -> 
      let (b, t2, t3) = first_sentence_text t in
      (b, Odoc_types.Block t2, Some (Odoc_types.Block t3))
  | Odoc_types.Title (n, l_opt, t) ->
      let (b, t2, t3) = first_sentence_text t in
      (b, 
       Odoc_types.Title (n, l_opt, t2), 
       Some (Odoc_types.Title (n, l_opt, t3)))
  | Odoc_types.Newline ->
      (true, Odoc_types.Raw "", Some Odoc_types.Newline)
  | Odoc_types.List _
  | Odoc_types.Enum _
  | Odoc_types.Latex _
  | Odoc_types.Link _ 
  | Odoc_types.Ref _ 
  | Odoc_types.Superscript _ 
  | Odoc_types.Subscript _ -> (false, text_ele, None)


let first_sentence_of_text t = 
  let (_,t2,_) = first_sentence_text t in 
  t2

let first_sentence_and_rest_of_text t =
  let (_,t1, t2) = first_sentence_text t in
  (t1, t2)

(*********************************************************)

let create_index_lists elements string_of_ele =
  let rec f current acc0 acc1 acc2 = function
      [] -> (acc0 :: acc1) @ [acc2]
    | ele :: q ->
        let s = string_of_ele ele in
        match s with
          "" -> f current acc0 acc1 (acc2 @ [ele]) q
        | _ ->
            let first = Char.uppercase s.[0] in
            match first with
              'A' .. 'Z' ->
                if current = first then
                  f current acc0 acc1 (acc2 @ [ele]) q
                else
                  f first acc0 (acc1 @ [acc2]) [ele] q
            | _ ->
                f current (acc0 @ [ele]) acc1 acc2 q
  in
  f '_' [] [] [] elements


(*** for labels *)

let is_optional = Btype.is_optional
let label_name = Btype.label_name

let remove_option typ =
  let rec iter t =
    match t with
    | Types.Tconstr (p,tlist,_) ->
        (
         match p with
           Path.Pident id when Ident.name id = "option" ->
             (
              match tlist with
                [t2] -> t2.Types.desc
              | _ -> t
             )
         | _ -> t
        )
    | Types.Tvar
    | Types.Tunivar
    | Types.Tpoly _
    | Types.Tarrow _ 
    | Types.Ttuple _ 
    | Types.Tobject _
    | Types.Tfield _
    | Types.Tnil 
    | Types.Tvariant _ -> t
    | Types.Tlink t2
    | Types.Tsubst t2 -> iter t2.Types.desc
  in
  { typ with Types.desc = iter typ.Types.desc }

(* eof $Id$ *)
