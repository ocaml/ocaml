module Ident = struct
  type t = string
end

module Path = struct
  type t =
      Pident of Ident.t
    | Pdot of t * string * int
    | Papply of t * t
end

type mutable_flag = Immutable | Mutable

type label = string

type type_expr =
  { mutable desc: type_desc; 
    (* mutable level: int; *)
    (* mutable id: int *) }

and type_desc =
    Tvar
  | Tarrow of label * type_expr * type_expr (* * commutable *)
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list (* * abbrev_memo ref *)
(*
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar
  | Tpoly of type_expr * type_expr list
*)

let mk_type desc = { desc= desc }

(* Type definitions *)

type record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_manifest: type_expr option;
    type_variance: (bool * bool * bool) list }
            (* covariant, contravariant, weakly contravariant *)

and type_kind =
    Type_abstract
  | Type_variant of (string * type_expr list) list (* * private_flag *)
  | Type_record of (string * mutable_flag * type_expr) list
                 * record_representation (* * private_flag *)

open Format

(* Print a type expression *)

(* From: Printyp *)
let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0

let reset_names () = names := []; name_counter := 0

let new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter)) 
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26) in
  incr name_counter;
  name

let name_of_type t =
  try List.assq t !names with Not_found ->
    let name = new_name () in
    names := (t, name) :: !names;
    name

let rec print_path ppf = function
  | Path.Pident id -> fprintf ppf "%s" id
  | Path.Pdot (p, name, n) -> fprintf ppf "%a.%s_%d" print_path p name n
  | Path.Papply (p1, p2) -> fprintf ppf "%a(%a)" print_path p1 print_path p2

(* From: Oprint.print_out_type *)
let rec print ppf ty = print1 ppf ty 

and print1 ppf ty = 
  match ty.desc with
  | Tarrow (lab, ty1, ty2) ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print2 ty1 print1 ty2
  | _ -> print2 ppf ty

and print2 ppf ty =
  match ty.desc with
  | Ttuple tyl -> 
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple " *") tyl
  | _ -> print_simple ppf ty

and print_simple ppf ty =
  match ty.desc with
  | Tconstr (p, tyl) ->
      fprintf ppf "@[%a%a@]" print_typargs tyl print_path p
  | Tvar -> 
      fprintf ppf "'%s" (name_of_type ty)
  | Tarrow (_,_,_) | Ttuple _ ->
      fprintf ppf "@[<1>(%a)@]" print ty

and print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a" print_elem ty sep (print_typlist print_elem sep)
        tyl
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] -> fprintf ppf "%a@ " print_simple ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " (print_typlist print ",") tyl
