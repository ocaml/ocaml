(* $Id$ *)

open Tables

(* CONFIGURE *)
(* if you set it true, ImagePhoto and ImageBitmap will annoy you... *)
let safetype = false 

let lowercase s =
  let r = String.create len:(String.length s) in
  String.blit s pos:0 to:r to_pos:0 len:(String.length s);
  for i = 0 to String.length s - 1 
  do
    let c = s.[i] in
    if c >= 'A' & c <= 'Z' then r.[i] <- Char.chr(Char.code c + 32)
  done;
  r

let labeloff :at l = match l with
  "",t -> t
| l ,t -> raise (Failure ("labeloff : " ^ l ^ " at " ^ at))

let labelstring l = match l with
  "" -> ""
| _ -> l ^ ":" 

let labelprint :w l = w (labelstring l)

let small s =
  let sout = ref "" in
  for i=0 to String.length s - 1 do
    let c =
      if s.[i] >= 'A' && s.[i] <= 'Z' then 
      	Char.chr(Char.code(s.[i]) - (Char.code 'A' - Char.code 'a'))
      else s.[i]
    in
      sout := !sout ^ (String.make len:1 c)
  done;
  !sout

let small_ident s =
  let idents = ["to"; "raise"; "in"; "class"; "new"]
  in
  let s = small s in
  if List.mem elt:s idents then (String.make len:1 s.[0])^s
  else s

let gettklabel fc = 
  match fc.template with
    ListArg( StringArg s :: _ ) ->
      if (try s.[0] = '-' with _ -> false) then
  	String.sub s pos:1 len:(String.length s - 1)
      else
  	if s = "" then small fc.ml_name else small s
  | _ -> raise (Failure "gettklabel")

let count elt:x l =
  let count = ref 0 in
  List.iter fun:(fun y -> if x = y then incr count) l;
  !count

let catenate_sep :sep =
  function 
    [] -> ""
  | x::l -> List.fold_left fun:(fun :acc s' -> acc ^ sep ^ s') acc:x l

(* Extract all types from a template *)
let rec types_of_template = function
    StringArg _ -> []
  | TypeArg (l,t) -> [l,t]
  | ListArg l -> List.flatten (List.map fun:types_of_template l)
  | OptionalArgs (l,tl,_) -> 
      begin
      match List.flatten (List.map fun:types_of_template tl) with
      	  ["",t] -> ["?"^l,t]
        | [_,_] -> raise (Failure "0 label required")
	| _ -> raise (Failure "0 or more than 1 args in for optionals")
      end

(* 
 * Pretty print a type
 *  used to write ML type definitions
 *)
let ppMLtype ?:any{=false} ?:return{=false} ?:def{=false} ?:counter{=ref 0} =
  let rec ppMLtype =
  function
    Unit -> "unit"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
(* new *)
  | List (Subtype (sup,sub)) ->
    if return then
      sub^"_"^sup^" list"
    else 
     begin
       try 
      	let typdef = Hashtbl.find types_table key:sup in
        let fcl = List.assoc key:sub typdef.subtypes in
        let tklabels = List.map fun:gettklabel fcl in
	let l = List.map fcl fun:
          begin fun fc ->
	    "?" ^ begin let p = gettklabel fc in
      	       	  if count elt:p tklabels > 1 then small fc.ml_name else p
      	          end
            ^ ":" ^ 
      	    let l = types_of_template fc.template in
      	    match l with
	      [] -> "unit"
            | [lt] -> ppMLtype (labeloff lt at:"ppMLtype")
      	    | l ->
                "(" ^ catenate_sep sep:"*" 
                  (List.map l
                     fun:(fun lt -> ppMLtype (labeloff lt at:"ppMLtype")))
                ^ ")"
          end in
	catenate_sep sep:"   ->\n" l
      with
        Not_found -> Printf.eprintf "ppMLtype %s/%s\n" sup sub; exit (-1)
     end
  | List ty -> (ppMLtype ty) ^ " list"
  | Product tyl -> catenate_sep sep:" * " (List.map fun:ppMLtype tyl)
  | Record tyl -> 
      catenate_sep sep:" * "
	(List.map tyl fun:(fun (l,t) -> labelstring l ^ ppMLtype t))
  | Subtype ("widget", sub) -> sub ^ " widget"
  | UserDefined "widget" -> 
      if any then "any widget" else 
      	let c = String.make len:1 (Char.chr(Char.code 'a' + !counter)) 
      	in 
      	  incr counter;
      	  "'" ^ c ^ " widget"
  | UserDefined s -> 
      (* a bit dirty hack for ImageBitmap and ImagePhoto *)
     begin
      try
	let typdef = Hashtbl.find types_table key:s in
	if typdef.variant then
	  if return then try
	    "[>" ^
  	    catenate_sep sep:"|" 
  	      (List.map typdef.constructors fun:
  		begin
  		  fun c ->
  		    "`" ^ c.var_name ^ 
  		    (match types_of_template c.template with
  		       [] -> ""
  		     | l ->  " " ^ ppMLtype (Product (List.map l
			      fun:(labeloff at:"ppMLtype UserDefined"))))
  		end) ^ "]"
  	  with
  	    Not_found ->
	    (prerr_endline ("ppMLtype "^s^ " ?"); s)
	  else if not def & List.length typdef.constructors > 1 then
	    "#" ^ s
	  else s
	else s
      with Not_found -> s
     end
  | Subtype (s,s') -> s'^"_"^s
  | Function (Product tyl) -> 
      	raise (Failure "Function (Product tyl) ? ppMLtype")
  | Function (Record tyl) -> 
      	"(" ^ catenate_sep sep:" -> " 
      	  (List.map tyl fun:(fun (l,t) -> labelstring l ^ ppMLtype t))
      	^ " -> unit)"
  | Function ty ->
      	"(" ^ (ppMLtype ty) ^ " -> unit)"
  | As (_, s) -> s
  in
    ppMLtype

(* Produce a documentation version of a template *)
let rec ppTemplate = function
    StringArg s -> s
  | TypeArg (l,t) -> "<" ^ ppMLtype t ^ ">"
  | ListArg l -> "{" ^ catenate_sep sep:" " (List.map fun:ppTemplate l) ^ "}"
  | OptionalArgs (l,tl,d) ->
      "?" ^ l ^ "{" ^ catenate_sep sep:" " (List.map fun:ppTemplate tl)
      ^ "}[<" ^ catenate_sep sep:" " (List.map fun:ppTemplate d) ^ ">]"

let doc_of_template = function
    ListArg l -> catenate_sep sep:" " (List.map fun:ppTemplate l)
  | t -> ppTemplate t

(*
 * Type definitions
 *)

(* Write an ML constructor *)
let write_constructor :w {ml_name = mlconstr; template = t} =
   w mlconstr;
   begin match types_of_template t with
       [] -> ()
     | l -> w " of ";
         w (ppMLtype any:true (Product (List.map l
                fun:(labeloff at:"write_constructor"))))
   end;
   w "\t\t(* tk option: "; w (doc_of_template t); w " *)"

(* Write a rhs type decl *)
let write_constructors :w = function
    [] -> fatal_error "empty type"
  | x::l ->
      write_constructor :w x;
      List.iter l fun:
	begin fun x ->
	  w "\n\t| ";
	  write_constructor :w x
        end

(* Write an ML variant *)
let write_variant :w {ml_name = mlconstr; var_name = varname; template = t} =
  w "`";
  w varname;
  begin match types_of_template t with
    [] -> ()
  | l ->    
      w " ";
      w (ppMLtype any:true def:true
	   (Product (List.map l fun:(labeloff at:"write_variant"))))
   end;
   w "\t\t(* tk option: "; w (doc_of_template t); w " *)"

let write_variants :w = function
    [] -> fatal_error "empty variants"
  | x::l ->
      write_variant :w x;
      List.iter l fun:
      	begin fun x ->
      	  w "\n  | ";
      	  write_variant :w x
        end

(* Definition of a type *)	    
let write_type intf:w impl:w' name def:typdef =
(*  if typdef.subtypes = [] then (* If there is no subtypes *) 
    begin
    (* The type itself *)
    (* Put markers for extraction *)
    w "(* type *)\n";
    w ("type "^name^" =\n\t");
    write_constructors :w (sort_components typdef.constructors);
    w "\n(* /type *)\n\n"
    end
  else
*)
    begin
      if typdef.subtypes = [] then
        begin
          w "(* Variant type *)\n";
          w ("type "^name^" = [\n    ");
          write_variants :w (sort_components typdef.constructors);
          w "\n]\n\n"
        end
      else
        begin
          (* Dynamic Subtyping *)
          (* All the subtypes *)
          List.iter typdef.subtypes fun:
  	    begin fun (s,l) ->
  	      w ("type "^s^"_"^name^" = [\n\t");
  	      write_variants w:w (sort_components l);
  	      w ("]\n\n")
            end
  	end
    end

(************************************************************)
(* Converters                                               *)
(************************************************************)

let rec converterTKtoCAML argname as:ty =
  match ty with
   Int -> "int_of_string " ^ argname
 | Float -> "float_of_string " ^ argname
 | Bool -> "(match " ^ argname ^" with
       	     \"1\" -> true
           | \"0\" -> false
           | s -> Pervasives.raise (Invalid_argument (\"cTKtoCAMLbool\" ^ s)))"
 | Char -> "String.get "^argname ^" 0"
 | String -> argname
 | UserDefined s -> "cTKtoCAML"^s^" "^argname
 | Subtype ("widget",s') ->
     "(Obj.magic (cTKtoCAMLwidget "^argname^") : "^s'^" widget)"
 | Subtype (s,s') -> "cTKtoCAML"^s'^"_"^s^" "^argname
 | List ty ->
    begin match type_parser_arity ty with
      OneToken -> 
      	 "(List.map (function x -> " ^ (converterTKtoCAML "x) " as:ty)
         ^ argname ^ ")"
    | MultipleToken ->
      	 "iterate_converter (function x -> " ^
              (converterTKtoCAML "x) " as:ty) ^ argname ^ ")"
    end
 | As (ty, _) -> converterTKtoCAML argname as:ty
 | t -> (prerr_endline ("ERROR with "^argname^" "^ppMLtype t);fatal_error "converterTKtoCAML")


(*******************************)
(* Wrappers                    *)
(*******************************)
let varnames :prefix n =
  let rec var i = 
    if i > n then []
    else (prefix^(string_of_int i)) :: (var (succ i))
  in var 1

(* 
 * generate wrapper source for callbacks
 *  transform a function ... -> unit in a function : unit -> unit
 *  using primitives arg_ ... from the protocol
 *  Warning: sequentiality is important in generated code
 *  TODO: remove arg_ stuff and process lists directly ?
 *)

let rec wrapper_code fname of:ty =
  match ty with
    Unit -> "(function _ -> "^fname^" ())"
  | As (ty, _) -> wrapper_code fname of:ty
  | ty ->
      "(function args ->\n\t\t" ^ 
      begin match ty with
          Product tyl -> raise (Failure "Product -> record was done. ???")
        | Record tyl ->
	  (* variables for each component of the product *)
	  let vnames = varnames prefix:"a" (List.length tyl) in
	  (* getting the arguments *)
	  let readarg = 
	    List.map2 vnames tyl fun:
	    begin fun v (l,ty) ->
	      match type_parser_arity ty with
		OneToken ->
		  "let ("^v^",args) = " ^
                  converterTKtoCAML "(List.hd args)"  as:ty  ^
                  ", List.tl args in\n\t\t"
	      | MultipleToken ->
		  "let ("^v^",args) = " ^
		  converterTKtoCAML "args"  as:ty ^
                  " in\n\t\t"
	    end in
          catenate_sep sep:"" readarg ^ fname ^ " " ^
          catenate_sep sep:" " 
      	    (List.map2 fun:(fun v (l,_) -> labelstring l^v) vnames tyl)

	(* all other types are read in one operation *)
	| List ty ->
	    fname ^ "(" ^ converterTKtoCAML "args" as:ty ^ ")"
	| String ->
	    fname ^ "(" ^ converterTKtoCAML "(List.hd args)" as:ty ^ ")"
	| ty ->
	  begin match type_parser_arity ty with
	    OneToken -> 
      	      fname ^ "(" ^ converterTKtoCAML "(List.hd args)" as:ty ^ ")"
	  | MultipleToken ->
	      "let (v,_) = " ^ converterTKtoCAML "args" as:ty ^
	      " in\n\t\t" ^ fname ^ " v"
          end
      end ^ ")"

(*************************************************************)
(* Parsers 						     *)
(*  are required only for values returned by commands and    *)
(*  functions (table is computed by the parser)		     *)

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
  if List.for_all constructors pred:
    begin fun c ->
      match c.template with
	ListArg [StringArg s] ->
	  pp.zeroary <- (s,"`" ^ c.var_name):: 
            pp.zeroary; true
      | ListArg [TypeArg(_,Int)] | ListArg[TypeArg(_,Float)] -> 
      	  if pp.intpar <> [] then false
	  else (pp.intpar <- ["`" ^ c.var_name]; true)
      | ListArg [TypeArg(_,String)] ->
	  if pp.stringpar <> [] then false
	  else (pp.stringpar <- ["`" ^ c.var_name]; true)
      | _ -> false
    end
  then ParserPieces pp
  else NoParser


(* We can generate parsers only for simple types *)
(* we should avoid multiple walks *)
let write_TKtoCAML :w name def:typdef =
  if typdef.parser_arity = MultipleToken then
    prerr_string ("You must write cTKtoCAML" ^ name ^
                            " : string list ->" ^ name ^ " * string list\n")
  else 
  let write :consts :name = 
    match can_generate_parser consts with
      NoParser ->
  	prerr_string
  	  ("You must write cTKtoCAML" ^ name ^" : string ->"^name^"\n")
    | ParserPieces pp ->
  	w ("let cTKtoCAML"^name^" n =\n");
  	(* First check integer *)
  	if pp.intpar <> [] then
  	begin
  	  w ("   try " ^ List.hd pp.intpar ^ " (int_of_string n)\n");
  	  w ("   with _ ->\n")
  	end;
  	w ("\tmatch n with\n");
  	let first = ref true in
  	List.iter pp.zeroary fun:
  	  begin fun (tk,ml) -> 
  	    if not !first then w "\t| " else w "\t";
  	    first := false;
  	    w "\""; w tk; w "\" -> "; w ml; w "\n"
  	  end;
  	let final = if pp.stringpar <> [] then
  	      "n -> " ^ List.hd pp.stringpar ^ " n"
  	   else " s -> Pervasives.raise (Invalid_argument (\"cTKtoCAML"
  		^ name ^ ": \" ^s))"
  	in
  	if not !first then w "\t| " else w "\t";
  	w final;
  	w "\n\n"
  in
    begin
      write :name consts:typdef.constructors;
      List.iter typdef.subtypes fun: begin
      	fun (subname,consts) -> write name:(subname^"_"^name) :consts
      end
    end

(******************************)
(* Converters                 *)
(******************************)

(* Produce an in-lined converter Caml -> Tk for simple types *)
(* the converter is a function of type:  <type> -> string  *)
let rec converterCAMLtoTK :context_widget argname as:ty =
 match ty with
    Int -> "TkToken (string_of_int " ^ argname ^ ")"
 |  Float -> "TkToken (string_of_float " ^ argname ^ ")"
 |  Bool -> "if "^argname^" then TkToken \"1\" else TkToken \"0\""
 |  Char -> "TkToken (Char.escaped " ^ argname ^ ")"
 |  String -> "TkToken " ^ argname
 |  As (ty, _) -> converterCAMLtoTK :context_widget argname as:ty
 |  UserDefined s -> 
       let name = "cCAMLtoTK"^s^" " in
       let args = argname in
(*
       let args = 
       	   if is_subtyped s then  (* unconstraint subtype *)
	     s^"_any_table "^args
	   else args in
*)
       let args = 
       	   if requires_widget_context s then
 	     context_widget^" "^args
           else args in
       name^args
 |  Subtype ("widget",s') ->
       let name = "cCAMLtoTKwidget" in
       let args = "("^argname^" : "^s'^" widget)" in
(*
       let args = 
       	   if requires_widget_context s then
	     context_widget^" "^args
           else args in
*)
       name^args
 |  Subtype (s,s') ->
       let name = "cCAMLtoTK"^s'^"_"^s^" " in
       let args = if safetype then "("^argname^" : "^s'^"_"^s^")" else argname
       in
(*
       let args = s^"_"^s'^"_table "^argname in
*)
       let args = 
       	   if requires_widget_context s then
	     context_widget^" "^args
           else args in
       name^args
 | Function _ -> fatal_error "unexpected function type in converterCAMLtoTK"
 | Unit       -> fatal_error "unexpected unit type in converterCAMLtoTK"
 | Product _  -> fatal_error "unexpected product type in converterCAMLtoTK"
 | Record _  -> fatal_error "unexpected product type in converterCAMLtoTK"
 | List ty -> fatal_error "unexpected list type in converterCAMLtoTK"

(* 
 * Produce a list of arguments from a template
 *  The idea here is to avoid allocation as much as possible
 *
 *)
 
let code_of_template :context_widget ?func:funtemplate{=false} template =
  let catch_opts = ref ("","") in (* class name and first option *)
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
          variables := (l,v) :: !variables; v in
  let newvar2 l =
      match !optionvar with
        Some v -> optionvar := None; v
      | None ->
          incr varcnter;
	  let v = "v" ^ (string_of_int !varcnter) in
          variables2 := (l,v) :: !variables2; v in
  let newvar = ref newvar1 in     
  let rec coderec = function
    StringArg s -> "TkToken\"" ^ s ^ "\""
  | TypeArg (_,List (Subtype (sup,sub) as ty)) ->
      let typdef = Hashtbl.find key:sup types_table in
      let classdef = List.assoc key:sub typdef.subtypes in
      let lbl = gettklabel (List.hd classdef) in
      catch_opts := (sub^"_"^sup, lbl);
      newvar := newvar2;
      "TkTokenList (List.map fun:(function x -> "
      ^ converterCAMLtoTK :context_widget "x" as:ty ^ ") opts)" 
  | TypeArg (l,List ty) ->
      "TkTokenList (List.map fun:(function x -> "
      ^ converterCAMLtoTK :context_widget "x" as:ty
      ^ ") " ^ !newvar l ^ ")"
  | TypeArg (l,Function tyarg) ->
     "let id = register_callback " ^context_widget
     ^ " callback: "^ wrapper_code (!newvar l) of:tyarg
     ^ " in TkToken (\"camlcb \"^id)"
  | TypeArg (l,ty) -> converterCAMLtoTK :context_widget (!newvar l) as:ty
  | ListArg l ->
      "TkQuote (TkTokenList ["
      ^ catenate_sep sep:";\n\t" (List.map fun:coderec l) ^ "])" 
  | OptionalArgs (l,tl,d) -> 
      let nv = !newvar ("?"^l) in
      optionvar := Some nv; (* Store *)
      let argstr = catenate_sep sep:"; " (List.map fun:coderec tl) in 
      let defstr = catenate_sep sep:"; " (List.map fun:coderec d) in
      "TkTokenList (match "^ nv ^" with\n"
      ^ "   Some " ^ nv ^ " -> [" ^ argstr ^ "]\n"
      ^ " | None -> [" ^ defstr ^ "])"
  in
  let code = 
    if funtemplate then 
    match template with
      ListArg l ->
	"[|" ^ catenate_sep sep:";\n\t" (List.map fun:coderec l) ^ "|]"
    | _ -> "[|" ^ coderec template ^ "|]"
    else
    match template with
      ListArg [x] -> coderec x
    | ListArg l ->
	"TkTokenList ["
	^ catenate_sep sep:";\n\t" (List.map fun:coderec l) ^ "]"
    | _ -> coderec template
    in
    code , List.rev !variables, List.rev !variables2, !catch_opts

(*
 * Converters for user defined types
 *)

(* For each case of a concrete type *)
let write_clause :w :context_widget comp =
  let warrow () = 
    w " -> "
  in

  w "`";
  w comp.var_name;
  
  let code, variables, variables2, (co, _) =
    code_of_template :context_widget comp.template in

  (* no subtype I think ... *)
  if co <> "" then raise (Failure "write_clause subtype ?"); 
  begin match variables with
    [] -> warrow()
  | [x] -> w " "; w (labeloff x at:"write_clause"); warrow()
  | l ->
      w " ( ";
      w (catenate_sep sep:", " (List.map fun:(labeloff at:"write_clause") l));
      w ")";
      warrow()
  end;
  w code


(* The full converter *)	 
let write_CAMLtoTK :w def:typdef ?safetype:st{=true} name =
  let write_one name constrs =
    w ("let cCAMLtoTK"^name);
    let context_widget = 
      if typdef.requires_widget_context then begin
	w " w"; "w"
        end
      else
	"dummy" in
    if safetype && st then
      w (" : " ^ name ^ " -> tkArgs ");
    w(" = function\n\t");
    write_clause :w :context_widget (List.hd constrs);
    List.iter (List.tl constrs)
      fun:(fun c -> w "\n\t| "; write_clause :w :context_widget c);
    w "\n\n\n"
  in
    if typdef.subtypes == [] then
      write_one name typdef.constructors
    else
      List.iter typdef.subtypes fun:begin
	fun (subname,constrs) -> 
	  write_one (subname^"_"^name) constrs
      end

(* Tcl does not really return "lists". It returns sp separated tokens *)
let rec write_result_parsing :w = function
    List String ->
      w "(splitlist res)"
  | List ty ->
      w ("\tList.map fun: "^ converterTKtoCAML "(splitlist res)" as:ty)
  | Product tyl -> raise (Failure "Product -> record was done. ???") 
  | Record tyl -> (* of course all the labels are "" *)
      let rnames = varnames prefix:"r" (List.length tyl) in
      w "\tlet l = splitlist res in\n";
      w ("\t  if List.length l <> " ^ string_of_int (List.length tyl) ^ "\n");
      w ("\t  then Pervasives.raise (TkError (\"unexpected result: \" ^ res))");
      w ("\t  else ");
      List.iter2 rnames tyl fun:
	begin fun r (l,ty) ->
	  if l <> "" then raise (Failure "lables in return type!!!"); 
	  w ("\tlet " ^ r ^ ", l = ");
	  begin match type_parser_arity ty with
	    OneToken ->
	      w (converterTKtoCAML "(List.hd l)" as:ty); w (", List.tl l")
	  | MultipleToken ->
	      w (converterTKtoCAML "l" as:ty)
	  end;
	  w (" in\n")
	end;
      w (catenate_sep sep:"," rnames)
  | String ->
      w (converterTKtoCAML "res" as:String)
  | As (ty, _) -> write_result_parsing :w ty
  | ty ->
      match type_parser_arity ty with
	OneToken -> w (converterTKtoCAML "res" as:ty)
      | MultipleToken -> w (converterTKtoCAML "(splitlist res)" as:ty)

let write_function :w def =
  w ("let "^def.ml_name);
  (* a bit approximative *)
  let context_widget = match def.template with
    ListArg (TypeArg(_,UserDefined("widget"))::_) -> "v1"
  | ListArg (TypeArg(_,Subtype("widget",_))::_) -> "v1"
  | _ -> "dummy" in

  let code, variables, variables2, (co, lbl) =
    code_of_template func:true :context_widget def.template in
  (* Arguments *)
  let uv, lv, ov = 
    let rec replace_args :u :l :o = function
    	[] -> u, l, o
      |	("",x)::ls ->
	  replace_args u:(x::u) :l :o  ls
      | (p,_ as x)::ls when p.[0] = '?' ->
      	  replace_args :u :l o:(x::o) ls
      | x::ls ->
	  replace_args :u l:(x::l) :o ls
    in
      replace_args u:[] l:[] o:[] (List.rev (variables @ variables2))
  in
  List.iter (lv@ov) fun:(fun (l,v) -> w " "; w (labelstring l); w v);
  if co <> "" then begin
    if lv = [] && ov = [] then w (" ?" ^ lbl ^ ":eta");
    w " =\n";
    w (co ^ "_optionals");
    if lv = [] && ov = [] then w (" ?" ^ lbl ^ ":eta");
    w " (fun opts";
    if uv = [] then w " ()"
    else List.iter uv fun:(fun x -> w " "; w x);
    w " ->\n"
  end else begin
    List.iter uv fun:(fun x -> w " "; w x);
    if (ov <> [] || lv = []) && uv = [] then w " ()";
    w " =\n"
  end;
  begin match def.result with
    Unit | As (Unit, _) ->  
      w "tkEval ";  w code; w ";()";
  | ty -> 
      w "let res = tkEval "; w code ; w " in \n";
      write_result_parsing :w ty;
  end;
  if co <> "" then w ")";
  w "\n\n"

let write_create :w clas =
  (w  "let create :parent ?:name =\n" : unit);
  w ("  "^ clas ^ "_options_optionals (fun options () ->\n");
  w ("     let w = new_atom \"" ^ clas ^ "\" :parent ?:name in\n");
  w  "     tkEval [|";
  w ("TkToken \"" ^ clas ^ "\";\n");
  w ("              TkToken (Widget.name w);\n");
  w ("              TkTokenList (List.map fun:(cCAMLtoTK" ^ clas ^ "_options dummy) options) |];\n");
  w ("      w)\n\n\n")

(* builtin-code: the file (without suffix) is in .template... *)
(* not efficient, but hell *)
let write_external :w def =
  match def.template with
    StringArg fname ->
      let ic = open_in_bin (fname ^ ".ml") in
      	begin try
	 while true do
	   w (input_line ic);
	   w "\n"
	 done
        with
	 End_of_file -> close_in ic
        end
  | _ -> raise (Compiler_Error "invalid external definition")

let write_catch_optionals :w clas def:typdef =
  if typdef.subtypes = [] then 
    (* begin Printf.eprintf "No subtypes\n";() end *) ()
  else 
    (* Printf.eprintf "Type constructors of %s\n" clas; *)
  List.iter typdef.subtypes fun:
  begin fun (subclass, classdefs) ->
(*
      Printf.eprintf "Subclass %s" subclass;
      List.iter (fun fc -> 
  	Printf.eprintf "  %s\n" fc.ml_name) classdefs;
*)
    w  ("let " ^ subclass ^"_"^ clas ^ "_optionals f = fun\n");
    let tklabels = List.map fun:gettklabel classdefs in
    let l = 
      List.map classdefs fun:
      begin fun fc ->
	List.length (types_of_template fc.template),
	types_of_template fc.template,
	(* used as names of variants *)
	fc.var_name,
	begin let p = gettklabel fc in
	  if count elt:p tklabels > 1 then small fc.ml_name else p
	end,
	small_ident fc.ml_name (* used as labels *)
      end in
    let p = 
      List.map l fun:
      begin fun (_,_,_,s,si) ->
      	if s = si then "  ?:" ^ s 
      	else "  ?" ^ s ^ ":" ^ si
      end in
    let v =
      List.map l fun:
      begin fun (i,t,c,s,si) -> 
      	let vars =
	  if i = 0 then "()" else
      	  if i = 1 then "x" 
	  else 
      	  let s = ref [] in
      	  for i=1 to i do
      	    s := !s @ ["x" ^ string_of_int i]
	  done;
	  "(" ^ catenate_sep sep:"," !s ^ ")"
	in
	let apvars = 
	  if i = 0 then "" 
	  (* VERY VERY QUICK HACK FOR 'a widget -> any widget *)
	  else if i = 1 && vars = "x" && t = ["",UserDefined "widget"] then
	  "(forget_type x)"
	  else vars
	in  
	"(maycons (fun " ^ vars ^ " -> " ^ "`" ^ c ^ " " ^ apvars ^ ") " ^ si
      end in
    w (catenate_sep sep:"\n" p);
    w " ->\n";
    w "    f ";
    w (catenate_sep sep:"\n      " v);
    w "\n       []";
    w (String.make len:(List.length v) ')');
    w "\n\n"
  end
