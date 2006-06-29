(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
type spec_list = list (string * Arg.spec * string);
open Format;

value rec action_arg s sl =
  fun
  [ Arg.Unit f -> if s = "" then do { f (); Some sl } else None
  | Arg.Bool f ->
      if s = "" then
        match sl with
        [ [s :: sl] ->
            try do { f (bool_of_string s); Some sl } with
            [ Invalid_argument "bool_of_string" -> None ]
        | [] -> None ]
      else
        try do { f (bool_of_string s); Some sl } with
        [ Invalid_argument "bool_of_string" -> None ]
  | Arg.Set r -> if s = "" then do { r.val := True; Some sl } else None
  | Arg.Clear r -> if s = "" then do { r.val := False; Some sl } else None
  | Arg.Rest f -> do { List.iter f [s :: sl]; Some [] }
  | Arg.String f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { f s; Some sl }
        | [] -> None ]
      else do { f s; Some sl }
  | Arg.Set_string r ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { r.val := s; Some sl }
        | [] -> None ]
      else do { r.val := s; Some sl }
  | Arg.Int f ->
      if s = "" then
        match sl with
        [ [s :: sl] ->
            try do { f (int_of_string s); Some sl } with
            [ Failure "int_of_string" -> None ]
        | [] -> None ]
      else
        try do { f (int_of_string s); Some sl } with
        [ Failure "int_of_string" -> None ]
  | Arg.Set_int r ->
      if s = "" then
        match sl with
        [ [s :: sl] ->
            try do { r.val := (int_of_string s); Some sl } with
            [ Failure "int_of_string" -> None ]
        | [] -> None ]
      else
        try do { r.val := (int_of_string s); Some sl } with
        [ Failure "int_of_string" -> None ]
  | Arg.Float f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { f (float_of_string s); Some sl }
        | [] -> None ]
      else do { f (float_of_string s); Some sl }
  | Arg.Set_float r ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { r.val := (float_of_string s); Some sl }
        | [] -> None ]
      else do { r.val := (float_of_string s); Some sl }
  | Arg.Tuple specs ->
      let rec action_args s sl =
        fun
        [ [] -> Some sl
        | [spec :: spec_list] ->
             match action_arg s sl spec with
             [ None -> action_args "" [] spec_list
             | Some [s :: sl] -> action_args s sl spec_list
             | Some sl -> action_args "" sl spec_list
             ]
        ] in
      action_args s sl specs
  | Arg.Symbol syms f ->
      match (if s = "" then sl else [s :: sl]) with
      [ [s :: sl] when List.mem s syms -> do { f s; Some sl }
      | _ -> None ]
  ];

value common_start s1 s2 =
  loop 0 where rec loop i =
    if i == String.length s1 || i == String.length s2 then i
    else if s1.[i] == s2.[i] then loop (i + 1)
    else i;

value parse_arg fold s sl =
  fold
    (fun (name, action, _) acu ->
      let i = common_start s name in
      if i == String.length name then
        try action_arg (String.sub s i (String.length s - i)) sl action with
        [ Arg.Bad _ -> acu ]
      else acu) None;

value rec parse_aux fold anon_fun =
  fun
  [ [] -> []
  | [s :: sl] ->
      if String.length s > 1 && s.[0] = '-' then
        match parse_arg fold s sl with
        [ Some sl -> parse_aux fold anon_fun sl
        | None -> [s :: parse_aux fold anon_fun sl] ]
      else do { (anon_fun s : unit); parse_aux fold anon_fun sl } ];

value align_doc key s =
  let s =
    loop 0 where rec loop i =
      if i = String.length s then ""
      else if s.[i] = ' ' then loop (i + 1)
     else String.sub s i (String.length s - i)
  in
  let (p, s) =
    if String.length s > 0 then
      if s.[0] = '<' then
        loop 0 where rec loop i =
          if i = String.length s then ("", s)
          else if s.[i] <> '>' then loop (i + 1)
          else
            let p = String.sub s 0 (i + 1) in
            loop (i + 1) where rec loop i =
              if i >= String.length s then (p, "")
              else if s.[i] = ' ' then loop (i + 1)
              else (p, String.sub s i (String.length s - i))
      else ("", s)
    else ("", "")
  in
  let tab =
    String.make (max 1 (16 - String.length key - String.length p)) ' '
  in
  p ^ tab  ^ s;

value make_symlist l =
  match l with
  [ [] -> "<none>"
  | [h::t] -> (List.fold_left (fun x y -> x ^ "|" ^ y) ("{" ^ h) t) ^ "}" ];

value print_usage_list l =
  List.iter
    (fun (key, spec, doc) ->
      match spec with
      [ Arg.Symbol symbs _ ->
          let s = make_symlist symbs in
          let synt = key ^ " " ^ s in
          eprintf "  %s %s\n" synt (align_doc synt doc)
      | _ -> eprintf "  %s %s\n" key (align_doc key doc) ] )
    l;

value remaining_args argv =
  let rec loop l i =
    if i == Array.length argv then l else loop [argv.(i) :: l] (i + 1)
  in
  List.rev (loop [] (Arg.current.val + 1));

value init_spec_list = ref [];
value ext_spec_list = ref [];

value init spec_list = init_spec_list.val := spec_list;

value add name spec descr =
  ext_spec_list.val := [(name, spec, descr) :: ext_spec_list.val];

value fold f init =
  let spec_list = init_spec_list.val @ ext_spec_list.val in
  let specs = Sort.list (fun (k1, _, _) (k2, _, _) -> k1 >= k2) spec_list in
  List.fold_right f specs init;

value parse anon_fun argv =
  let remaining_args = remaining_args argv in
  parse_aux fold anon_fun remaining_args;

value ext_spec_list () = ext_spec_list.val;
