(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

open Tables;;

open Format;;

let escape_string s =
  let more = ref 0 in
  for i = 0 to String.length s - 1 do
   match s.[i] with
   | '\\' | '"' -> incr more
   |  _ -> ()
  done;
  if !more = 0 then s else
  let res = String.create (String.length s + !more) in
  let j = ref 0 in
  for i = 0 to String.length s - 1 do
   let c = s.[i] in
   match c with
   | '\\' | '"' -> res.[!j] <- '\\'; incr j; res.[!j] <- c; incr j
   | _ -> res.[!j] <- c; incr j
  done;
  res;;

let escape_char c = if c = '\'' then "\\'" else String.make 1 c;;

let print_quoted_string s = printf "\"%s\"" (escape_string s);;
let print_quoted_char c = printf "'%s'" (escape_char c);;
let print_quoted_int i =
 if i < 0 then printf "(%d)" i else printf "%d" i;;
let print_quoted_float f =
 if f <= 0.0 then printf "(%f)" f else printf "%f" f;;

(* Iterators *)
let print_list f l =
 printf "@[<1>[";
 let rec pl = function
 | [] -> printf "@;<0 -1>]@]"
 | [x] -> f x; pl []
 | x :: xs -> f x; printf ";@ "; pl xs in
 pl l;;

let print_array f v =
 printf "@[<2>[|";
 let l = Array.length v in
 if l >= 1 then f v.(0);
 if l >= 2 then
  for i = 1 to l - 1 do
   printf ";@ "; f v.(i)
  done;
 printf "@;<0 -1>|]@]";;

let print_option f = function
  | None -> print_string "None"
  | Some x -> printf "@[<1>Some@ "; f x; printf "@]";;

let print_bool = function
  | true -> print_string "true" | _ -> print_string "false";;

let print_poly x = print_string "<poly>";;

(* Types of the description language *)
let rec print_mltype = function
  | Unit -> printf "Unit" | Int -> printf "Int" | Float -> printf "Float"
  | Bool -> printf "Bool" | Char -> printf "Char" | String -> printf "String"
  | List m -> printf "@[<1>(%s@ " "List"; print_mltype m; printf ")@]"
  | Product l_m ->
     printf "@[<1>(%s@ " "Product"; print_list print_mltype l_m; printf ")@]"
  | Record l_t_s_m ->
     printf "@[<1>(%s@ " "Record";
     print_list
      (function (s, m) ->
        printf "@[<1>("; print_quoted_string s; printf ",@ "; print_mltype m;
        printf ")@]")
      l_t_s_m;
     printf ")@]"
  | UserDefined s ->
     printf "@[<1>(%s@ " "UserDefined"; print_quoted_string s; printf ")@]"
  | Subtype (s, s0) ->
     printf "@[<1>(%s@ " "Subtype"; printf "@[<1>("; print_quoted_string s;
     printf ",@ "; print_quoted_string s0; printf ")@]"; printf ")@]"
  | Function m ->
     printf "@[<1>(%s@ " "Function"; print_mltype m; printf ")@]"
  | As (m, s) ->
     printf "@[<1>(%s@ " "As"; printf "@[<1>("; print_mltype m; printf ",@ ";
     print_quoted_string s; printf ")@]"; printf ")@]";;

let rec print_template = function
  | StringArg s ->
     printf "@[<1>(%s@ " "StringArg"; print_quoted_string s; printf ")@]"
  | TypeArg (s, m) ->
     printf "@[<1>(%s@ " "TypeArg"; printf "@[<1>("; print_quoted_string s;
     printf ",@ "; print_mltype m; printf ")@]"; printf ")@]"
  | ListArg l_t ->
     printf "@[<1>(%s@ " "ListArg"; print_list print_template l_t;
     printf ")@]"
  | OptionalArgs (s, l_t, l_t0) ->
     printf "@[<1>(%s@ " "OptionalArgs"; printf "@[<1>(";
     print_quoted_string s; printf ",@ "; print_list print_template l_t;
     printf ",@ "; print_list print_template l_t0; printf ")@]"; printf ")@]";;

(* Sorts of components *)
let rec print_component_type = function
  | Constructor -> printf "Constructor" | Command -> printf "Command"
  | External -> printf "External";;

(* Full definition of a component *)
let rec print_fullcomponent = function
  {component = c; ml_name = s; var_name = s0; template = t; result = m;
   safe = b; 
  } ->
    printf "@[<1>{"; printf "@[<1>component =@ "; print_component_type c;
    printf ";@]@ "; printf "@[<1>ml_name =@ "; print_quoted_string s;
    printf ";@]@ "; printf "@[<1>var_name =@ "; print_quoted_string s0;
    printf ";@]@ "; printf "@[<1>template =@ "; print_template t;
    printf ";@]@ "; printf "@[<1>result =@ "; print_mltype m; printf ";@]@ ";
    printf "@[<1>safe =@ "; print_bool b; printf ";@]@ "; printf "@,}@]";;

(* components are given either in full or abbreviated *)
let rec print_component = function
  | Full f -> printf "@[<1>(%s@ " "Full"; print_fullcomponent f; printf ")@]"
  | Abbrev s ->
     printf "@[<1>(%s@ " "Abbrev"; print_quoted_string s; printf ")@]";;

(* A type definition *)
(* 
 requires_widget_context: the converter of the type MUST be passed
   an additional argument of type Widget.
*)
let rec print_parser_arity = function
  | OneToken -> printf "OneToken" | MultipleToken -> printf "MultipleToken";;

let rec print_type_def = function
  {parser_arity = p; constructors = l_f; subtypes = l_t_s_l_f;
   requires_widget_context = b; variant = b0; 
  } ->
    printf "@[<1>{"; printf "@[<1>parser_arity =@ "; print_parser_arity p;
    printf ";@]@ "; printf "@[<1>constructors =@ ";
    print_list print_fullcomponent l_f; printf ";@]@ ";
    printf "@[<1>subtypes =@ ";
    print_list
     (function (s, l_f0) ->
       printf "@[<1>("; print_quoted_string s; printf ",@ ";
       print_list print_fullcomponent l_f0; printf ")@]")
     l_t_s_l_f;
    printf ";@]@ "; printf "@[<1>requires_widget_context =@ "; print_bool b;
    printf ";@]@ "; printf "@[<1>variant =@ "; print_bool b0; printf ";@]@ ";
    printf "@,}@]";;

let rec print_module_type = function
  | Widget -> printf "Widget" | Family -> printf "Family";;

let rec print_module_def = function
  {module_type = m; commands = l_f; externals = l_f0; } ->
    printf "@[<1>{"; printf "@[<1>module_type =@ "; print_module_type m;
    printf ";@]@ "; printf "@[<1>commands =@ ";
    print_list print_fullcomponent l_f; printf ";@]@ ";
    printf "@[<1>externals =@ "; print_list print_fullcomponent l_f0;
    printf ";@]@ "; printf "@,}@]";;
