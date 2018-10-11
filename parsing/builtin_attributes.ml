(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

let string_of_cst = function
  | Pconst_string(s, _) -> Some s
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      string_of_cst c
  | _ -> None

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

let error_of_extension ext =
  let submessage_from main_loc main_txt = function
    | {pstr_desc=Pstr_extension
           (({txt = ("ocaml.error"|"error"); loc}, p), _)} ->
        begin match p with
        | PStr([{pstr_desc=Pstr_eval
                     ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}]) ->
            { Location.loc; txt = fun ppf -> Format.pp_print_text ppf msg }
        | _ ->
            { Location.loc; txt = fun ppf ->
                Format.fprintf ppf
                  "Invalid syntax for sub-message of extension '%s'." main_txt }
        end
    | {pstr_desc=Pstr_extension (({txt; loc}, _), _)} ->
        { Location.loc; txt = fun ppf ->
            Format.fprintf ppf "Uninterpreted extension '%s'." txt }
    | _ ->
        { Location.loc = main_loc; txt = fun ppf ->
            Format.fprintf ppf
              "Invalid syntax for sub-message of extension '%s'." main_txt }
  in
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
      begin match p with
      | PStr [] -> raise Location.Already_displayed_error
      | PStr({pstr_desc=Pstr_eval
                  ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::
             inner) ->
          let sub = List.map (submessage_from loc txt) inner in
          Location.error_of_printer ~loc ~sub Format.pp_print_text msg
      | _ ->
          Location.errorf ~loc "Invalid syntax for extension '%s'." txt
      end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let cat s1 s2 =
  if s2 = "" then s1 else s1 ^ "\n" ^ s2

let deprecated_attr x =
  match x with
  | {attr_name = {txt = "ocaml.deprecated"|"deprecated"; _};_} -> Some x
  | _ -> None

let rec deprecated_attrs = function
  | [] -> None
  | hd :: tl ->
    match deprecated_attr hd with
    | Some x -> Some x
    | None -> deprecated_attrs tl

let deprecated_of_attrs l =
  match deprecated_attrs l with
  | None -> None
  | Some a -> Some (string_of_opt_payload a.attr_payload)

let check_deprecated loc attrs s =
  match deprecated_of_attrs attrs with
  | None -> ()
  | Some txt -> Location.deprecated loc (cat s txt)

let check_deprecated_inclusion ~def ~use loc attrs1 attrs2 s =
  match deprecated_of_attrs attrs1, deprecated_of_attrs attrs2 with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None -> Location.deprecated ~def ~use loc (cat s txt)

let rec deprecated_mutable_of_attrs = function
  | [] -> None
  | {attr_name =  {txt = "ocaml.deprecated_mutable"|"deprecated_mutable"; _};
     attr_payload = p} :: _ ->
     Some (string_of_opt_payload p)
  | _ :: tl -> deprecated_mutable_of_attrs tl

let check_deprecated_mutable loc attrs s =
  match deprecated_mutable_of_attrs attrs with
  | None -> ()
  | Some txt ->
      Location.deprecated loc (Printf.sprintf "mutating field %s" (cat s txt))

let check_deprecated_mutable_inclusion ~def ~use loc attrs1 attrs2 s =
  match deprecated_mutable_of_attrs attrs1,
        deprecated_mutable_of_attrs attrs2
  with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None ->
      Location.deprecated ~def ~use loc
        (Printf.sprintf "mutating field %s" (cat s txt))

let rec deprecated_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_sig tl
      | Some _ as r -> r
      end
  | _ -> None


let rec deprecated_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_str tl
      | Some _ as r -> r
      end
  | _ -> None


let check_no_deprecated attrs =
  match deprecated_attrs attrs with
  | None -> ()
  | Some {attr_name = {txt;_};attr_loc} ->
    Location.prerr_warning attr_loc (Warnings.Misplaced_attribute txt)

let warning_attribute ?(ppwarning = true) =
  let process loc txt errflag payload =
    match string_of_payload payload with
    | Some s ->
        begin try Warnings.parse_options errflag s
        with Arg.Bad _ ->
          Location.prerr_warning loc
            (Warnings.Attribute_payload
               (txt, "Ill-formed list of warnings"))
        end
    | None ->
        Location.prerr_warning loc
          (Warnings.Attribute_payload
             (txt, "A single string literal is expected"))
  in
  function
  | {attr_name = {txt = ("ocaml.warning"|"warning") as txt; _};
     attr_loc;
     attr_payload;
     } ->
      process attr_loc txt false attr_payload
  | {attr_name = {txt = ("ocaml.warnerror"|"warnerror") as txt; _};
     attr_loc;
     attr_payload
    } ->
      process attr_loc txt true attr_payload
  | {attr_name = {txt="ocaml.ppwarning"|"ppwarning"; _};
     attr_loc = _;
     attr_payload =
       PStr [
         { pstr_desc=
             Pstr_eval({pexp_desc=Pexp_constant (Pconst_string (s, _))},_);
           pstr_loc }
       ];
    } when ppwarning ->
     Location.prerr_warning pstr_loc (Warnings.Preprocessor s)
  | _ ->
     ()

let warning_scope ?ppwarning attrs f =
  let prev = Warnings.backup () in
  try
    List.iter (warning_attribute ?ppwarning) (List.rev attrs);
    let ret = f () in
    Warnings.restore prev;
    ret
  with exn ->
    Warnings.restore prev;
    raise exn


let warn_on_literal_pattern =
  List.exists
    (fun a -> match a.attr_name.txt with
       | "ocaml.warn_on_literal_pattern"|"warn_on_literal_pattern" -> true
       | _ -> false
    )

let explicit_arity =
  List.exists
    (fun a -> match a.attr_name.txt with
       | "ocaml.explicit_arity"|"explicit_arity" -> true
       | _ -> false
    )

let immediate =
  List.exists
    (fun a -> match a.attr_name.txt with
       | "ocaml.immediate"|"immediate" -> true
       | _ -> false
    )

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let check l a = List.mem a.attr_name.txt l

let has_unboxed attr =
  List.exists (check ["ocaml.unboxed"; "unboxed"])
    attr

let has_boxed attr =
  List.exists (check ["ocaml.boxed"; "boxed"]) attr


module Redirect = struct
  type expr =
    | Ident of string
    | Literal of string
    | Leq of expr * expr
    | And of expr * expr

  exception Error of Location.t * string

  open Longident

  let parse loc = function
    | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_apply(condition, [Nolabel,{pexp_desc=Pexp_ident lid}])},_)}] ->
        let rec expr = function
          | {pexp_desc=Pexp_ident {txt=Lident s}} -> Ident s
          | {pexp_desc=Pexp_constant (Pconst_string (s, _))} -> Literal s
          | {pexp_desc=Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident "<="}}, [Nolabel,e1; Nolabel,e2])} ->
              Leq (expr e1, expr e2)
          | {pexp_desc=Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident "&&"}}, [Nolabel,e1; Nolabel,e2])} ->
              And (expr e1, expr e2)
          | e -> raise (Error (e.pexp_loc, "invalid expression"))
        in
        expr condition, lid
    | _ -> raise (Error (loc, "invalid payload"))

  let version s =
    try
      s
      |> String.trim
      |> String.split_on_char '.'
      |> List.map int_of_string
    with Failure _ ->
      failwith (Printf.sprintf "Invalid version string %s" s)

  let rec eval = function
    | And (e1, e2) -> eval e1 && eval e2
    | Leq (Ident id, Literal s) ->
        begin match Sys.getenv_opt (String.uppercase_ascii id) with
        | None -> false
        | Some v -> version v <= version s
        end
    | _ ->
        failwith "Invalid condition"
end


let rec redirect loc = function
  | [] -> None
  | {
      attr_name={txt=("ocaml.redirect"|"redirect") as txt};
      attr_payload;
      attr_loc;
    } :: tl ->
      begin match Redirect.parse attr_loc attr_payload with
      | exception (Redirect.Error (loc, str)) ->
          Location.prerr_warning loc (Warnings.Attribute_payload (txt, str));
          redirect loc tl
      | (condition, target) ->
          if Redirect.eval condition then begin
            Location.deprecated loc "this identifier has been redirected";
            Some target
          end else redirect loc tl
      end
  | _ :: tl -> redirect loc tl
