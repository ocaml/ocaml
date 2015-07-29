(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type 'name t =
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float_array of float list
  | String of string
  | Immstring of string
  (* CR mshinwell: think about whether this should really be here. *)
  | Block of Tag.t * 'name list

let compare (x : _ t) (y : _ t) ~compare_name_lists =
  let compare_floats x1 x2 =
    Int64.compare (Int64.bits_of_float x1) (Int64.bits_of_float x2)
  in
   let rec compare_float_lists l1 l2 =
     match l1, l2 with
     | [], [] -> 0
     | [], _::_ -> -1
     | _::_, [] -> 1
     | h1::t1, h2::t2 ->
       let c = compare_floats h1 h2 in
       if c <> 0 then c else compare_float_lists t1 t2
  in
  match x, y with
  | Float x, Float y -> compare_floats x y
  | Int32 x, Int32 y -> compare x y
  | Int64 x, Int64 y -> compare x y
  | Nativeint x, Nativeint y -> compare x y
  | Float_array x, Float_array y -> compare_float_lists x y
  | String x, String y -> compare x y
  | Immstring x, Immstring y -> compare x y
  | Block (tag1, fields1), Block (tag2, fields2) ->
    let c = Tag.compare tag1 tag2 in
    if c <> 0 then c
    else compare_name_lists fields1 fields2
  | Float _, _ -> -1
  | _, Float _ -> 1
  | Int32 _, _ -> -1
  | _, Int32 _ -> 1
  | Int64 _, _ -> -1
  | _, Int64 _ -> 1
  | Nativeint _, _ -> -1
  | _, Nativeint _ -> 1
  | Float_array _, _ -> -1
  | _, Float_array _ -> 1
  | String _, _ -> -1
  | _, String _ -> 1
  | Immstring _, _ -> -1
  | _, Immstring _ -> 1

let map (t : _ t) ~f =
  match t with
  | Float v -> Float v
  | Int32 v -> Int32 v
  | Int64 v -> Int64 v
  | Nativeint v -> Nativeint v
  | Float_array v -> Float_array v
  | String v -> String v
  | Immstring v -> Immstring v
  | Block (tag, fields) ->
    Block (tag, List.map f fields)

let print print_name ppf (t : _ t) =
  let fprintf = Format.fprintf in
  match t with
  | String s -> fprintf ppf "%S" s
  | Immstring s -> fprintf ppf "#%S" s
  | Int32 n -> fprintf ppf "%lil" n
  | Int64 n -> fprintf ppf "%LiL" n
  | Nativeint n -> fprintf ppf "%nin" n
  | Float f -> fprintf ppf "%f" f
  | Float_array [] -> fprintf ppf "[| |]"
  | Float_array (f1 :: fl) ->
    let floats ppf fl =
      List.iter (fun f -> fprintf ppf "@ %f" f) fl
    in
    fprintf ppf "@[<1>[|@[%f%a@]|]@]" f1 floats fl
  | Block (tag, []) -> fprintf ppf "[| Atom: tag=%a |]" Tag.print tag
  | Block (tag, f1 :: fl) ->
    let fields ppf fl =
      List.iter (fun f -> fprintf ppf "@ %a" print_name f) fl
    in
    fprintf ppf "@[<1>[|tag=%a@ @[%a%a@]|]@]" Tag.print tag
      print_name f1 fields fl
