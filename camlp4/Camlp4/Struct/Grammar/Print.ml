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

module Make (Structure : Structure.S) = struct
  open Structure;
  open Format;
  open Sig.Grammar;

  value rec flatten_tree =
    fun
    [ DeadEnd -> []
    | LocAct _ _ -> [[]]
    | Node {node = n; brother = b; son = s} ->
        [ [n :: l] | l <- flatten_tree s ] @ flatten_tree b ];

  value rec print_symbol ppf =
    fun
    [ Smeta n sl _ -> print_meta ppf n sl
    | Slist0 s -> fprintf ppf "LIST0 %a" print_symbol1 s
    | Slist0sep s t ->
        fprintf ppf "LIST0 %a SEP %a" print_symbol1 s print_symbol1 t
    | Slist1 s -> fprintf ppf "LIST1 %a" print_symbol1 s
    | Slist1sep s t ->
        fprintf ppf "LIST1 %a SEP %a" print_symbol1 s print_symbol1 t
    | Sopt s -> fprintf ppf "OPT %a" print_symbol1 s
    | Snterml e l -> fprintf ppf "%s@ LEVEL@ %S" e.ename l
    | Snterm _ | Snext | Sself | Stree _ | Stoken _ | Skeyword _ as s ->
        print_symbol1 ppf s ]
  and print_meta ppf n sl =
    loop 0 sl where rec loop i =
      fun
      [ [] -> ()
      | [s :: sl] ->
          let j =
            try String.index_from n i ' ' with [ Not_found -> String.length n ]
          in
          do {
            fprintf ppf "%s %a" (String.sub n i (j - i)) print_symbol1 s;
            if sl = [] then ()
            else do { fprintf ppf " "; loop (min (j + 1) (String.length n)) sl }
          } ]
  and print_symbol1 ppf =
    fun
    [ Snterm e -> pp_print_string ppf e.ename
    | Sself -> pp_print_string ppf "SELF"
    | Snext -> pp_print_string ppf "NEXT"
    | Stoken (_, descr) -> pp_print_string ppf descr
    | Skeyword s -> fprintf ppf "%S" s
    | Stree t -> print_level ppf pp_print_space (flatten_tree t)
    | Smeta _ _ _ | Snterml _ _ | Slist0 _ | Slist0sep _ _ | Slist1 _ |
      Slist1sep _ _ | Sopt _ as s ->
        fprintf ppf "(%a)" print_symbol s ]
  and print_rule ppf symbols =
    do {
      fprintf ppf "@[<hov 0>";
      let _ =
        List.fold_left
          (fun sep symbol ->
            do {
              fprintf ppf "%t%a" sep print_symbol symbol;
              fun ppf -> fprintf ppf ";@ "
            })
          (fun _ -> ()) symbols
      in
      fprintf ppf "@]"
    }
  and print_level ppf pp_print_space rules =
    do {
      fprintf ppf "@[<hov 0>[ ";
      let _ =
        List.fold_left
          (fun sep rule ->
            do {
              fprintf ppf "%t%a" sep print_rule rule;
              fun ppf -> fprintf ppf "%a| " pp_print_space ()
            })
          (fun _ -> ()) rules
      in
      fprintf ppf " ]@]"
    }
  ;

  value levels ppf elev =
    let _ =
      List.fold_left
        (fun sep lev ->
          let rules =
            [ [Sself :: t] | t <- flatten_tree lev.lsuffix ] @
              flatten_tree lev.lprefix
          in
          do {
            fprintf ppf "%t@[<hov 2>" sep;
            match lev.lname with
            [ Some n -> fprintf ppf "%S@;<1 2>" n
            | None -> () ];
            match lev.assoc with
            [ LeftA -> fprintf ppf "LEFTA"
            | RightA -> fprintf ppf "RIGHTA"
            | NonA -> fprintf ppf "NONA" ];
            fprintf ppf "@]@;<1 2>";
            print_level ppf pp_force_newline rules;
            fun ppf -> fprintf ppf "@,| "
          })
        (fun _ -> ()) elev
    in
    ();

  value entry ppf e =
    do {
      fprintf ppf "@[<v 0>%s: [ " e.ename;
      match e.edesc with
      [ Dlevels elev -> levels ppf elev
      | Dparser _ -> fprintf ppf "<parser>" ];
      fprintf ppf " ]@]"
    };

end;

module MakeDump (Structure : Structure.S) = struct
  open Structure;
  open Format;
  open Sig.Grammar;

  type brothers = [ Bro of symbol and list brothers ];

  value rec print_tree ppf tree =
    let rec get_brothers acc =
      fun
      [ DeadEnd -> List.rev acc
      | LocAct _ _ -> List.rev acc
      | Node {node = n; brother = b; son = s} -> get_brothers [Bro n (get_brothers [] s) :: acc] b ]
    and print_brothers ppf brothers =
      if brothers = [] then fprintf ppf "@ []"
      else
        List.iter (fun [ Bro n xs -> do {
          fprintf ppf "@ @[<hv2>- %a" print_symbol n;
          match xs with
          [ [] -> ()
          | [_] -> try print_children ppf (get_children [] xs)
                   with [ Exit -> fprintf ppf ":%a" print_brothers xs ]
          | _ -> fprintf ppf ":%a" print_brothers xs ];
          fprintf ppf "@]";
        }]) brothers
    and print_children ppf = List.iter (fprintf ppf ";@ %a" print_symbol)
    and get_children acc =
      fun
      [ [] -> List.rev acc
      | [Bro n x] -> get_children [n::acc] x
      | _ -> raise Exit ]
    in print_brothers ppf (get_brothers [] tree)
  and print_symbol ppf =
    fun
    [ Smeta n sl _ -> print_meta ppf n sl
    | Slist0 s -> fprintf ppf "LIST0 %a" print_symbol1 s
    | Slist0sep s t ->
        fprintf ppf "LIST0 %a SEP %a" print_symbol1 s print_symbol1 t
    | Slist1 s -> fprintf ppf "LIST1 %a" print_symbol1 s
    | Slist1sep s t ->
        fprintf ppf "LIST1 %a SEP %a" print_symbol1 s print_symbol1 t
    | Sopt s -> fprintf ppf "OPT %a" print_symbol1 s
    | Snterml e l -> fprintf ppf "%s@ LEVEL@ %S" e.ename l
    | Snterm _ | Snext | Sself | Stree _ | Stoken _ | Skeyword _ as s ->
        print_symbol1 ppf s ]
  and print_meta ppf n sl =
    loop 0 sl where rec loop i =
      fun
      [ [] -> ()
      | [s :: sl] ->
          let j =
            try String.index_from n i ' ' with [ Not_found -> String.length n ]
          in
          do {
            fprintf ppf "%s %a" (String.sub n i (j - i)) print_symbol1 s;
            if sl = [] then ()
            else do { fprintf ppf " "; loop (min (j + 1) (String.length n)) sl }
          } ]
  and print_symbol1 ppf =
    fun
    [ Snterm e -> pp_print_string ppf e.ename
    | Sself -> pp_print_string ppf "SELF"
    | Snext -> pp_print_string ppf "NEXT"
    | Stoken (_, descr) -> pp_print_string ppf descr
    | Skeyword s -> fprintf ppf "%S" s
    | Stree t -> print_tree ppf t
    | Smeta _ _ _ | Snterml _ _ | Slist0 _ | Slist0sep _ _ | Slist1 _ |
      Slist1sep _ _ | Sopt _ as s ->
        fprintf ppf "(%a)" print_symbol s ]
  and print_rule ppf symbols =
    do {
      fprintf ppf "@[<hov 0>";
      let _ =
        List.fold_left
          (fun sep symbol ->
            do {
              fprintf ppf "%t%a" sep print_symbol symbol;
              fun ppf -> fprintf ppf ";@ "
            })
          (fun _ -> ()) symbols
      in
      fprintf ppf "@]"
    }
  and print_level ppf pp_print_space rules =
    do {
      fprintf ppf "@[<hov 0>[ ";
      let _ =
        List.fold_left
          (fun sep rule ->
            do {
              fprintf ppf "%t%a" sep print_rule rule;
              fun ppf -> fprintf ppf "%a| " pp_print_space ()
            })
          (fun _ -> ()) rules
      in
      fprintf ppf " ]@]"
    }
  ;

  value levels ppf elev =
    let _ =
      List.fold_left
        (fun sep lev ->
          do {
            fprintf ppf "%t@[<v2>" sep;
            match lev.lname with
            [ Some n -> fprintf ppf "%S@;<1 2>" n
            | None -> () ];
            match lev.assoc with
            [ LeftA -> fprintf ppf "LEFTA"
            | RightA -> fprintf ppf "RIGHTA"
            | NonA -> fprintf ppf "NONA" ];
            fprintf ppf "@]@;<1 2>";
            fprintf ppf "@[<v2>suffix:@ ";
            print_tree ppf lev.lsuffix;
            fprintf ppf "@]@ @[<v2>prefix:@ ";
            print_tree ppf lev.lprefix;
            fprintf ppf "@]";
            fun ppf -> fprintf ppf "@,| "
          })
        (fun _ -> ()) elev
    in
    ();

  value entry ppf e =
    do {
      fprintf ppf "@[<v 0>%s: [ " e.ename;
      match e.edesc with
      [ Dlevels elev -> levels ppf elev
      | Dparser _ -> fprintf ppf "<parser>" ];
      fprintf ppf " ]@]"
    };

end;
