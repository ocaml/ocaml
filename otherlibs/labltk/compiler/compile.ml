(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Francois Rouaix, Francois Pessaux and Jun Furuse              *)
(*               projet Cristal, INRIA Rocquencourt                      *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tables

(* CONFIGURE *)
(* if you set it true, ImagePhoto and ImageBitmap will annoy you... *)
let safetype = true

let labeloff ~at l = match l with
  "", t -> t
| l, t -> raise (Failure ("labeloff: " ^ l ^ " at " ^ at))

let labelstring l =
  if l = "" then l else
  if l.[0] = '?' then l ^ ":" else
  "~" ^ l ^ ":"

let typelabel l =
  if l = "" then l else l ^ ":"

let forbidden = [ "class"; "type"; "in"; "from"; "to" ]
let nicknames =
  [ "class", "clas";
    "type", "typ" ]

let small = String.lowercase

let gettklabel fc = 
  match fc.template with
    ListArg( StringArg s :: _ ) ->
      let s = small s in
      if s = "" then s else
      let s =
        if s.[0] = '-'
        then String.sub s ~pos:1 ~len:(String.length s - 1)
        else s
      in begin
        if List.mem s forbidden then
          try List.assoc s nicknames
          with Not_found -> small fc.var_name
        else s
      end
  | _ -> raise (Failure "gettklabel")

let count ~item:x l =
  let count = ref 0 in
  List.iter ~f:(fun y -> if x = y then incr count) l;
  !count

(* Extract all types from a template *)
let rec types_of_template = function
    StringArg _ -> []
  | TypeArg (l, t) -> [l, t]
  | ListArg l -> List.flatten (List.map ~f:types_of_template l)
  | OptionalArgs (l, tl, _) -> 
      begin
      match List.flatten (List.map ~f:types_of_template tl) with
          ["", t] -> ["?" ^ l, t]
        | [_, _] -> raise (Failure "0 label required")
        | _ -> raise (Failure "0 or more than 1 args in for optionals")
      end

(* 
 * Pretty print a type
 *  used to write ML type definitions
 *)
let ppMLtype ?(any=false) ?(return=false) ?(def=false) ?(counter=ref 0) =
  let rec ppMLtype =
  function
    Unit -> "unit"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
(* new *)
  | List (Subtype (sup, sub)) ->
    if return then
      sub ^ "_" ^ sup ^ " list"
    else 
     begin
       try 
        let typdef = Hashtbl.find types_table sup in
        let fcl = List.assoc sub typdef.subtypes in
        let tklabels = List.map ~f:gettklabel fcl in
        let l = List.map fcl ~f:
          begin fun fc ->
            "?" ^ begin let p = gettklabel fc in
                  if count ~item:p tklabels > 1 then small fc.var_name else p
                  end
            ^ ":" ^ 
            let l = types_of_template fc.template in
            match l with
              [] -> "unit"
            | [lt] -> ppMLtype (labeloff lt ~at:"ppMLtype")
            | l ->
                "(" ^ String.concat ~sep:"*" 
                  (List.map l
                     ~f:(fun lt -> ppMLtype (labeloff lt ~at:"ppMLtype")))
                ^ ")"
          end in
        String.concat ~sep:"   ->\n" l
      with
        Not_found -> Printf.eprintf "ppMLtype %s/%s\n" sup sub; exit (-1)
     end
  | List ty -> (ppMLtype ty) ^ " list"
  | Product tyl ->
      "(" ^ String.concat ~sep:" * " (List.map ~f:ppMLtype tyl) ^ ")"
  | Record tyl -> 
      String.concat ~sep:" * "
        (List.map tyl ~f:(fun (l, t) -> typelabel l ^ ppMLtype t))
  | Subtype ("widget", sub) -> sub ^ " widget"
  | UserDefined "widget" -> 
      if any then "any widget" else 
        let c = String.make 1 (Char.chr(Char.code 'a' + !counter)) 
        in 
          incr counter;
          "'" ^ c ^ " widget"
  | UserDefined s -> 
      (* a bit dirty hack for ImageBitmap and ImagePhoto *)
     begin
      try
        let typdef = Hashtbl.find types_table s in
        if typdef.variant then
          if return then try
            "[>" ^
            String.concat ~sep:"|" 
              (List.map typdef.constructors ~f:
                begin
                  fun c ->
                    "`" ^ c.var_name ^ 
                    (match types_of_template c.template with
                       [] -> ""
                     | l ->  " of " ^ ppMLtype (Product (List.map l
                              ~f:(labeloff ~at:"ppMLtype UserDefined"))))
                end) ^ "]"
          with
            Not_found -> prerr_endline ("ppMLtype " ^ s ^ " ?"); s
          else if not def && List.length typdef.constructors > 1 then
            "#" ^ s
          else s
        else s
      with Not_found -> s
     end
  | Subtype (s, s') -> s' ^ "_" ^ s
  | Function (Product tyl) -> 
        raise (Failure "Function (Product tyl) ? ppMLtype")
  | Function (Record tyl) -> 
        "(" ^ String.concat ~sep:" -> " 
          (List.map tyl ~f:(fun (l, t) -> typelabel l ^ ppMLtype t))
        ^ " -> unit)"
  | Function ty ->
        "(" ^ (ppMLtype ty) ^ " -> unit)"
  | As (_, s) -> s
  in
    ppMLtype

(* Produce a documentation version of a template *)
let rec ppTemplate = function
    StringArg s -> s
  | TypeArg (l, t) -> "<" ^ ppMLtype t ^ ">"
  | ListArg l -> "{" ^ String.concat ~sep:" " (List.map ~f:ppTemplate l) ^ "}"
  | OptionalArgs (l, tl, d) ->
      "?" ^ l ^ "{" ^ String.concat ~sep:" " (List.map ~f:ppTemplate tl)
      ^ "}[<" ^ String.concat ~sep:" " (List.map ~f:ppTemplate d) ^ ">]"

let doc_of_template = function
    ListArg l -> String.concat ~sep:" " (List.map ~f:ppTemplate l)
  | t -> ppTemplate t

(*
 * Type definitions
 *)

(* Write an ML constructor *)
let write_constructor ~w {ml_name = mlconstr; template = t} =
   w mlconstr;
   begin match types_of_template t with
       [] -> ()
     | l -> w " of ";
         w (ppMLtype ~any:true (Product (List.map l
                ~f:(labeloff ~at:"write_constructor"))))
   end;
   w "        (* tk option: "; w (doc_of_template t); w " *)"

(* Write a rhs type decl *)
let write_constructors ~w = function
    [] -> fatal_error "empty type"
  | x :: l ->
      write_constructor ~w x;
      List.iter l ~f:
        begin fun x ->
          w "\n    | ";
          write_constructor ~w x
        end

(* Write an ML variant *)
let write_variant ~w {var_name = varname; template = t} =
  w "`";
  w varname;
  begin match types_of_template t with
    [] -> ()
  | l ->    
      w " of ";
      w (ppMLtype ~any:true ~def:true
           (Product (List.map l ~f:(labeloff ~at:"write_variant"))))
   end;
   w "        (* tk option: "; w (doc_of_template t); w " *)"

let write_variants ~w = function
    [] -> fatal_error "empty variants"
  | l ->
      List.iter l ~f:
        begin fun x ->
          w "\n   | ";
          write_variant ~w x
        end

(* Definition of a type *)          
let write_type ~intf:w ~impl:w' name ~def:typdef =
  (* Only needed if no subtypes, otherwise use optionals *)
  if typdef.subtypes = [] then begin
    w "(* Variant type *)\n";
    w ("type " ^ name ^ " = [");
    write_variants ~w (sort_components typdef.constructors);
    w "\n]\n\n"
  end

(************************************************************)
(* Converters                                               *)
(************************************************************)

let rec converterTKtoCAML ~arg = function
  | Int -> "int_of_string " ^ arg
  | Float -> "float_of_string " ^ arg
  | Bool -> "(match " ^ arg ^ " with
            | \"1\" -> true
            | \"0\" -> false
            | s -> Pervasives.raise (Invalid_argument (\"cTKtoCAMLbool\" ^ s)))"
  | Char -> "String.get " ^ arg ^ " 0"
  | String -> arg
  | UserDefined s -> "cTKtoCAML" ^ s ^ " " ^ arg
  | Subtype ("widget", s') ->
      String.concat ~sep:" "
        ["(Obj.magic (cTKtoCAMLwidget "; arg; ") :"; s'; "widget)"]
  | Subtype (s, s') -> "cTKtoCAML" ^ s' ^ "_" ^ s ^ " " ^ arg
  | List ty ->
     begin match type_parser_arity ty with
       OneToken ->
         String.concat ~sep:" "
           ["(List.map (function x ->";
            converterTKtoCAML ~arg:"x" ty; ")"; arg; ")"]
     | MultipleToken ->
         String.concat ~sep:" "
           ["iterate_converter (function x ->";
            converterTKtoCAML ~arg:"x" ty; ")"; arg; ")"]
     end
  | As (ty, _) -> converterTKtoCAML ~arg ty
  | t ->
     prerr_endline ("ERROR with " ^ arg ^ " " ^ ppMLtype t);
     fatal_error "converterTKtoCAML"


(*******************************)
(* Wrappers                    *)
(*******************************)
let varnames ~prefix n =
  let rec var i = 
    if i > n then []
    else (prefix ^ string_of_int i) :: var (succ i)
  in var 1

(* 
 * generate wrapper source for callbacks
 *  transform a function ... -> unit in a function : unit -> unit
 *  using primitives arg_ ... from the protocol
 *  Warning: sequentiality is important in generated code
 *  TODO: remove arg_ stuff and process lists directly ?
 *)

let rec wrapper_code ~name ty =
  match ty with
    Unit -> "(fun _ -> " ^ name ^ " ())"
  | As (ty, _) -> wrapper_code ~name ty
  | ty ->
      "(fun args ->\n        " ^
      begin match ty with
          Product tyl -> raise (Failure "Product -> record was done. ???")
        | Record tyl ->
          (* variables for each component of the product *)
          let vnames = varnames ~prefix:"a" (List.length tyl) in
          (* getting the arguments *)
          let readarg = 
            List.map2 vnames tyl ~f:
            begin fun v (l, ty) ->
              match type_parser_arity ty with
                OneToken ->
                  "let (" ^ v ^ ", args) = " ^
                  converterTKtoCAML ~arg:"(List.hd args)" ty ^
                  ", List.tl args in\n        "
              | MultipleToken ->
                  "let (" ^ v ^ ", args) = " ^
                  converterTKtoCAML ~arg:"args" ty ^
                  " in\n        "
            end in
          String.concat ~sep:"" readarg ^ name ^ " " ^
          String.concat ~sep:" " 
            (List.map2 ~f:(fun v (l, _) -> labelstring l ^ v) vnames tyl)

        (* all other types are read in one operation *)
        | List ty ->
            name ^ "(" ^ converterTKtoCAML ~arg:"args" ty ^ ")"
        | String ->
            name ^ "(" ^ converterTKtoCAML ~arg:"(List.hd args)" ty ^ ")"
        | ty ->
          begin match type_parser_arity ty with
            OneToken -> 
              name ^ "(" ^ converterTKtoCAML ~arg:"(List.hd args)" ty ^ ")"
          | MultipleToken ->
              "let (v, _) = " ^ converterTKtoCAML ~arg:"args" ty ^
              " in\n        " ^ name ^ " v"
          end
      end ^ ")"

(*************************************************************)
(* Parsers                                                   *)
(*  are required only for values returned by commands and    *)
(*  functions (table is computed by the parser)              *)

(* Tuples/Lists are Ok if they don't contain strings         *)
(* they will be returned as list of strings                  *)

(* Can we generate a "parser" ?
   -> all constructors are unit and at most one int and one string, with null constr
*)
type parser_pieces =
    { mutable zeroary : (string * string) list ; (* kw string, ml name *)
      mutable intpar : string list; (* one at most, mlname *)
      mutable stringpar : string list (* idem *)
    }

type mini_parser = 
   NoParser 
 | ParserPieces of parser_pieces

let can_generate_parser constructors =
  let pp = {zeroary = []; intpar = []; stringpar = []} in
  if List.for_all constructors ~f:
    begin fun c ->
      match c.template with
        ListArg [StringArg s] ->
          pp.zeroary <- (s, "`" ^ c.var_name) :: 
            pp.zeroary; true
      | ListArg [TypeArg(_, Int)] | ListArg[TypeArg(_, Float)] -> 
          if pp.intpar <> [] then false
          else (pp.intpar <- ["`" ^ c.var_name]; true)
      | ListArg [TypeArg(_, String)] ->
          if pp.stringpar <> [] then false
          else (pp.stringpar <- ["`" ^ c.var_name]; true)
      | _ -> false
    end
  then ParserPieces pp
  else NoParser


(* We can generate parsers only for simple types *)
(* we should avoid multiple walks *)
let write_TKtoCAML ~w name ~def:typdef =
  if typdef.parser_arity = MultipleToken then
    prerr_string ("You must write cTKtoCAML" ^ name ^
                            " : string list ->" ^ name ^ " * string list\n")
  else 
  let write ~consts ~name = 
    match can_generate_parser consts with
      NoParser ->
        prerr_string
          ("You must write cTKtoCAML" ^ name ^ " : string ->" ^ name ^ "\n")
    | ParserPieces pp ->
        w ("let cTKtoCAML" ^ name ^ " n =\n");
        (* First check integer *)
        if pp.intpar <> [] then
        begin
          w ("   try " ^ List.hd pp.intpar ^ " (int_of_string n)\n");
          w ("   with _ ->\n")
        end;
        w ("    match n with\n");
        List.iter pp.zeroary ~f:
          begin fun (tk, ml) -> 
            w "    | \""; w tk; w "\" -> "; w ml; w "\n"
          end;
        let final = if pp.stringpar <> [] then
              "n -> " ^ List.hd pp.stringpar ^ " n"
           else "s -> Pervasives.raise (Invalid_argument (\"cTKtoCAML"
                ^ name ^ ": \" ^ s))"
        in
        w "    | ";
        w final;
        w "\n\n"
  in
    begin
      write ~name ~consts:typdef.constructors;
      List.iter typdef.subtypes ~f: begin
        fun (subname, consts) -> write ~name:(subname ^ "_" ^ name) ~consts
      end
    end

(******************************)
(* Converters                 *)
(******************************)

(* Produce an in-lined converter Caml -> Tk for simple types *)
(* the converter is a function of type:  <type> -> string  *)
let rec converterCAMLtoTK ~context_widget argname ty =
 match ty with
    Int -> "TkToken (string_of_int " ^ argname ^ ")"
 |  Float -> "TkToken (string_of_float " ^ argname ^ ")"
 |  Bool -> "if " ^ argname ^ " then TkToken \"1\" else TkToken \"0\""
 |  Char -> "TkToken (Char.escaped " ^ argname ^ ")"
 |  String -> "TkToken " ^ argname
 |  As (ty, _) -> converterCAMLtoTK ~context_widget argname ty
 |  UserDefined s -> 
       let name = "cCAMLtoTK" ^ s ^ " " in
       let args = argname in
       let args = 
           if requires_widget_context s then
             context_widget ^ " " ^ args
           else args in
       name ^ args
 |  Subtype ("widget", s') ->
       let name = "cCAMLtoTKwidget" in
       let args = "(" ^ argname ^ " : " ^ s' ^ " widget)" in
       name ^ args
 |  Subtype (s, s') ->
       let name = "cCAMLtoTK" ^ s' ^ "_" ^ s ^ " " in
       let args = if safetype then "(" ^ argname ^ " : #" ^ s' ^ "_" ^ s ^ ")"
                  else argname
       in
       let args = 
           if requires_widget_context s then context_widget ^ " " ^ args
           else args in
       name ^ args
 | Product tyl ->
     let vars = varnames ~prefix:"z" (List.length tyl) in
     String.concat ~sep:" "
       ("let" :: String.concat ~sep:"," vars :: "=" :: argname ::
        "in TkTokenList [" ::
        String.concat ~sep:"; "
          (List.map2 vars tyl ~f:(converterCAMLtoTK ~context_widget)) ::
        ["]"])
 | Function _ -> fatal_error "unexpected function type in converterCAMLtoTK"
 | Unit -> fatal_error "unexpected unit type in converterCAMLtoTK"
 | Record _ -> fatal_error "unexpected product type in converterCAMLtoTK"
 | List ty -> fatal_error "unexpected list type in converterCAMLtoTK"

(* 
 * Produce a list of arguments from a template
 *  The idea here is to avoid allocation as much as possible
 *
 *)

let code_of_template ~context_widget ?func:(funtemplate=false) template =
  let catch_opts = ref ("", "") in (* class name and first option *)
  let variables = ref [] in
  let variables2 = ref [] in
  let varcnter = ref 0 in
  let optionvar = ref None in
  let newvar1 l = 
      match !optionvar with
        Some v -> optionvar := None; v
      | None ->
          incr varcnter;
          let v = "v" ^ (string_of_int !varcnter) in
          variables := (l, v) :: !variables; v in
  let newvar2 l =
      match !optionvar with
        Some v -> optionvar := None; v
      | None ->
          incr varcnter;
          let v = "v" ^ (string_of_int !varcnter) in
          variables2 := (l, v) :: !variables2; v in
  let newvar = ref newvar1 in     
  let rec coderec = function
    StringArg s -> "TkToken \"" ^ s ^ "\""
  | TypeArg (_, List (Subtype (sup, sub) as ty)) ->
      let typdef = Hashtbl.find types_table sup in
      let classdef = List.assoc sub typdef.subtypes in
      let lbl = gettklabel (List.hd classdef) in
      catch_opts := (sub ^ "_" ^ sup, lbl);
      newvar := newvar2;
      "TkTokenList opts"
  | TypeArg (l, List ty) ->
      "TkTokenList (List.map ~f:(function x -> "
      ^ converterCAMLtoTK ~context_widget "x" ty
      ^ ") " ^ !newvar l ^ ")"
  | TypeArg (l, Function tyarg) ->
     "let id = register_callback " ^ context_widget
     ^ " ~callback: " ^ wrapper_code ~name:(!newvar l) tyarg
     ^ " in TkToken (\"camlcb \" ^ id)"
  | TypeArg (l, ty) -> converterCAMLtoTK ~context_widget (!newvar l) ty
  | ListArg l ->
      "TkQuote (TkTokenList ["
      ^ String.concat ~sep:";\n    " (List.map ~f:coderec l) ^ "])" 
  | OptionalArgs (l, tl, d) -> 
      let nv = !newvar ("?" ^ l) in
      optionvar := Some nv; (* Store *)
      let argstr = String.concat ~sep:"; " (List.map ~f:coderec tl) in 
      let defstr = String.concat ~sep:"; " (List.map ~f:coderec d) in
      "TkTokenList (match " ^ nv ^ " with\n"
      ^ " | Some " ^ nv ^ " -> [" ^ argstr ^ "]\n"
      ^ " | None -> [" ^ defstr ^ "])"
  in
  let code = 
    if funtemplate then 
    match template with
      ListArg l ->
        "[|" ^ String.concat ~sep:";\n    " (List.map ~f:coderec l) ^ "|]"
    | _ -> "[|" ^ coderec template ^ "|]"
    else
    match template with
      ListArg [x] -> coderec x
    | ListArg l ->
        "TkTokenList [" ^
        String.concat ~sep:";\n    " (List.map ~f:coderec l) ^
        "]"
    | _ -> coderec template
    in
    code, List.rev !variables, List.rev !variables2, !catch_opts

(*
 * Converters for user defined types
 *)

(* For each case of a concrete type *)
let write_clause ~w ~context_widget comp =
  let warrow () = w " -> " in
  w "`";
  w comp.var_name;

  let code, variables, variables2, (co, _) =
    code_of_template ~context_widget comp.template in

  (* no subtype I think ... *)
  if co <> "" then raise (Failure "write_clause subtype ?"); 
  begin match variables with
  | [] -> warrow()
  | [x] -> w " "; w (labeloff x ~at:"write_clause"); warrow()
  | l ->
      w " ( ";
      w (String.concat ~sep:", " (List.map ~f:(labeloff ~at:"write_clause") l));
      w ")";
      warrow()
  end;
  w code

(* The full converter *)
let write_CAMLtoTK ~w ~def:typdef ?safetype:(st = true) name =
  let write_one name constrs =
    w ("let cCAMLtoTK" ^ name);
    let context_widget = 
      if typdef.requires_widget_context then begin
        w " w"; "w"
        end
      else
        "dummy" in
    if st then begin
      w " : ";
      if typdef.variant then w "#";
      w name; w " -> tkArgs "
    end;
    w (" = function");
    List.iter constrs
      ~f:(fun c -> w "\n  | "; write_clause ~w ~context_widget c);
    w "\n\n\n"
  in
  
  (* Only needed if no subtypes, otherwise use optionals *)
  let constrs = typdef.constructors in
  if typdef.subtypes == [] then
    write_one name constrs
  else
    List.iter constrs ~f:
      begin fun fc ->
        let code, vars, _, (co, _) =
          code_of_template ~context_widget:"dummy" fc.template in
        if co <> "" then fatal_error "optionals in optionals";
        let vars = List.map ~f:snd vars in
        w "let ccCAMLtoTK"; w name; w "_"; w (small fc.ml_name);
        w " ("; w (String.concat ~sep:", " vars); w ") =\n    ";
        w code; w "\n\n"
      end

(* Tcl does not really return "lists". It returns sp separated tokens *)
let rec write_result_parsing ~w = function
    List String ->
      w "(splitlist res)"
  | List ty ->
      w ("    List.map ~f: " ^ converterTKtoCAML ~arg:"(splitlist res)" ty)
  | Product tyl -> raise (Failure "Product -> record was done. ???") 
  | Record tyl -> (* of course all the labels are "" *)
      let rnames = varnames ~prefix:"r" (List.length tyl) in
      w "    let l = splitlist res in";
      w ("\n      if List.length l <> " ^ string_of_int (List.length tyl));
      w ("\n      then Pervasives.raise (TkError (\"unexpected result: \" ^ res))");
      w ("\n      else ");
      List.iter2 rnames tyl ~f:
        begin fun r (l, ty) ->
          if l <> "" then raise (Failure "lables in return type!!!"); 
          w ("    let " ^ r ^ ", l = ");
          begin match type_parser_arity ty with
            OneToken ->
              w (converterTKtoCAML ~arg:"(List.hd l)" ty); w (", List.tl l")
          | MultipleToken ->
              w (converterTKtoCAML ~arg:"l" ty)
          end;
          w (" in\n")
        end;
      w (String.concat ~sep:", " rnames)
  | String ->
      w (converterTKtoCAML ~arg:"res" String)
  | As (ty, _) -> write_result_parsing ~w ty
  | ty ->
      match type_parser_arity ty with
        OneToken -> w (converterTKtoCAML ~arg:"res" ty)
      | MultipleToken -> w (converterTKtoCAML ~arg:"(splitlist res)" ty)

let write_function ~w def =
  w ("let " ^ def.ml_name);
  (* a bit approximative *)
  let context_widget = match def.template with
    ListArg (TypeArg(_, UserDefined("widget")) :: _) -> "v1"
  | ListArg (TypeArg(_, Subtype("widget", _)) :: _) -> "v1"
  | _ -> "dummy" in

  let code, variables, variables2, (co, lbl) =
    code_of_template ~func:true ~context_widget def.template in
  (* Arguments *)
  let uv, lv, ov = 
    let rec replace_args ~u ~l ~o = function
        [] -> u, l, o
      | ("", x) :: ls ->
          replace_args ~u:(x :: u) ~l ~o  ls
      | (p, _ as x) :: ls when p.[0] = '?' ->
          replace_args ~u ~l ~o:(x :: o) ls
      | x :: ls ->
          replace_args ~u ~l:(x :: l) ~o ls
    in
      replace_args ~u:[] ~l:[] ~o:[] (List.rev (variables @ variables2))
  in
  let has_opts = (ov <> [] || co <> "") in
  if not has_opts then List.iter uv ~f:(fun x -> w " "; w x);
  List.iter (lv@ov) ~f:(fun (l, v) -> w " "; w (labelstring l); w v);
  if co <> "" then begin
    if lv = [] && ov = [] then w (" ?" ^ lbl ^ ":eta");
    w " =\n";
    w (co ^ "_optionals");
    if lv = [] && ov = [] then w (" ?" ^ lbl ^ ":eta");
    w " (fun opts";
    if uv = [] then w " ()" else
    if has_opts then List.iter uv ~f:(fun x -> w " "; w x);
    w " ->\n"
  end else begin
    if (ov <> [] || lv = []) && uv = [] then w " ()" else
    if has_opts then List.iter uv ~f:(fun x -> w " "; w x);
    w " =\n"
  end;
  begin match def.result with
  | Unit | As (Unit, _) -> w "tkCommand "; w code
  | ty ->
      w "let res = tkEval "; w code ; w " in \n";
      write_result_parsing ~w ty
  end;
  if co <> "" then w ")";
  w "\n\n"

let write_create ~w clas =
  (w  "let create ?name =\n" : unit);
  w ("  " ^ clas ^ "_options_optionals (fun opts parent ->\n");
  w ("     let w = new_atom \"" ^ clas ^ "\" ~parent ?name in\n");
  w  "     tkCommand [|";
  w ("TkToken \"" ^ clas ^ "\";\n");
  w ("              TkToken (Widget.name w);\n");
  w ("              TkTokenList opts |];\n");
  w ("      w)\n\n\n")

(* Search Path. *)
let search_path = ref ["."]

(* taken from utils/misc.ml *)
let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir :: rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

(* builtin-code: the file (without suffix) is in .template... *)
(* not efficient, but hell *)
let write_external ~w def =
  match def.template with
  | StringArg fname ->
      begin try
        let realname = find_in_path !search_path (fname ^ ".ml") in
        let ic = open_in_bin realname in
        begin try
         while true do
           w (input_line ic);
           w "\n"
         done
        with
        | End_of_file -> close_in ic
        end
      with
      | Not_found ->
          raise (Compiler_Error ("can't find external file: " ^ fname))
      end
| _ -> raise (Compiler_Error "invalid external definition")

let write_catch_optionals ~w clas ~def:typdef =
  if typdef.subtypes = [] then () else
  List.iter typdef.subtypes ~f:
  begin fun (subclass, classdefs) ->
    w  ("let " ^ subclass ^ "_" ^ clas ^ "_optionals f = fun\n");
    let tklabels = List.map ~f:gettklabel classdefs in
    let l = 
      List.map classdefs ~f:
      begin fun fc ->
        (*
        let code, vars, _, (co, _) =
          code_of_template ~context_widget:"dummy" fc.template in
        if co <> "" then fatal_error "optionals in optionals";
        *)
        let p = gettklabel fc in
        (if count ~item:p tklabels > 1 then small fc.var_name else p),
        small fc.ml_name
      end in
    let p =  List.map l ~f:(fun (si, _) -> "  ?" ^ si) in
    let v =
      List.map l ~f:
        begin fun (si, s) ->
          "(maycons ccCAMLtoTK" ^ clas ^ "_" ^ s ^ " " ^ si
        end in
    w (String.concat ~sep:"\n" p);
    w " ->\n";
    w "    f ";
    w (String.concat ~sep:"\n      " v);
    w "\n       []";
    w (String.make (List.length v) ')');
    w "\n\n"
  end
