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
(* camlp4r *)

open Format;

module ObjTools = struct

  value desc obj =
    if Obj.is_block obj then
      "tag = " ^ string_of_int (Obj.tag obj)
    else "int_val = " ^ string_of_int (Obj.obj obj);

  (*Imported from the extlib*)
  value rec to_string r =
    if Obj.is_int r then
      let i = (Obj.magic r : int)
      in string_of_int i ^ " | CstTag" ^ string_of_int (i + 1)
    else (* Block. *)
      let rec get_fields acc =
        fun
        [ 0 -> acc
        | n -> let n = n-1 in get_fields [Obj.field r n :: acc] n ]
      in
      let rec is_list r =
        if Obj.is_int r then
          r = Obj.repr 0 (* [] *)
        else
          let s = Obj.size r and t = Obj.tag r in
          t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
      in
      let rec get_list r =
        if Obj.is_int r then []
        else let h = Obj.field r 0 and t = get_list (Obj.field r 1) in [h :: t]
      in
      let opaque name =
        (* XXX In future, print the address of value 'r'.  Not possible in
        * pure OCaml at the moment.
        *)
        "<" ^ name ^ ">"
      in
      let s = Obj.size r and t = Obj.tag r in
      (* From the tag, determine the type of block. *)
      match t with
      [ _ when is_list r ->
              let fields = get_list r in
              "[" ^ String.concat "; " (List.map to_string fields) ^ "]"
      | 0 ->
              let fields = get_fields [] s in
              "(" ^ String.concat ", " (List.map to_string fields) ^ ")"
      | x when x = Obj.lazy_tag ->
              (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
              * clear if very large constructed values could have the same
              * tag. XXX *)
              opaque "lazy"
      | x when x = Obj.closure_tag ->
              opaque "closure"
      | x when x = Obj.object_tag ->
              let fields = get_fields [] s in
              let (_class, id, slots) =
                      match fields with
                      [ [h; h'::t] -> (h, h', t)
                      | _ -> assert False ]
              in
              (* No information on decoding the class (first field).  So just print
              * out the ID and the slots. *)
              "Object #" ^ to_string id ^ " (" ^ String.concat ", " (List.map to_string slots) ^ ")"
      | x when x = Obj.infix_tag ->
              opaque "infix"
      | x when x = Obj.forward_tag ->
              opaque "forward"
      | x when x < Obj.no_scan_tag ->
              let fields = get_fields [] s in
              "Tag" ^ string_of_int t ^
              " (" ^ String.concat ", " (List.map to_string fields) ^ ")"
      | x when x = Obj.string_tag ->
              "\"" ^ String.escaped (Obj.magic r : string) ^ "\""
      | x when x = Obj.double_tag ->
              string_of_float (Obj.magic r : float)
      | x when x = Obj.abstract_tag ->
              opaque "abstract"
      | x when x = Obj.custom_tag ->
              opaque "custom"
      | x when x = Obj.final_tag ->
              opaque "final"
      | _ ->
              failwith ("ObjTools.to_string: unknown tag (" ^ string_of_int t ^ ")") ];

  value print ppf x = fprintf ppf "%s" (to_string x);
  value print_desc ppf x = fprintf ppf "%s" (desc x);

end;

value default_handler ppf x = do {
  let x = Obj.repr x;
  fprintf ppf "Camlp4: Uncaught exception: %s"
    (Obj.obj (Obj.field (Obj.field x 0) 0) : string);
  if Obj.size x > 1 then do {
    pp_print_string ppf " (";
    for i = 1 to Obj.size x - 1 do
      if i > 1 then pp_print_string ppf ", " else ();
      ObjTools.print ppf (Obj.field x i);
    done;
    pp_print_char ppf ')'
  }
  else ();
  fprintf ppf "@."
};

value handler = ref (fun ppf default_handler exn -> default_handler ppf exn);

value register f =
  let current_handler = handler.val in
  handler.val :=
    fun ppf default_handler exn ->
      try f ppf exn with exn -> current_handler ppf default_handler exn;

module Register (Error : Sig.Error) = struct
  let current_handler = handler.val in
  handler.val :=
    fun ppf default_handler ->
      fun [ Error.E x -> Error.print ppf x
          | x -> current_handler ppf default_handler x ];
end;


value gen_print ppf default_handler =
  fun
  [ Out_of_memory -> fprintf ppf "Out of memory"
  | Assert_failure (file, line, char) ->
      fprintf ppf "Assertion failed, file %S, line %d, char %d"
                  file line char
  | Match_failure (file, line, char) ->
      fprintf ppf "Pattern matching failed, file %S, line %d, char %d"
                  file line char
  | Failure str -> fprintf ppf "Failure: %S" str
  | Invalid_argument str -> fprintf ppf "Invalid argument: %S" str
  | Sys_error str -> fprintf ppf "I/O error: %S" str
  | Stream.Failure -> fprintf ppf "Parse failure"
  | Stream.Error str -> fprintf ppf "Parse error: %s" str
  | x -> handler.val ppf default_handler x ];

value print ppf = gen_print ppf default_handler;

value try_print ppf = gen_print ppf (fun _ -> raise);

value to_string exn =
  let buf = Buffer.create 128 in
  let () = bprintf buf "%a" print exn in
  Buffer.contents buf;

value try_to_string exn =
  let buf = Buffer.create 128 in
  let () = bprintf buf "%a" try_print exn in
  Buffer.contents buf;
