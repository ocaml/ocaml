(* $Id$ *)

(* Internal compiler errors *)

exception Compiler_Error of string 
let fatal_error s = raise (Compiler_Error s)


(* Types of the description language *)
type mltype =
   Unit
 | Int
 | Float
 | Bool
 | Char
 | String
 | List of mltype
 | Product of mltype list
 | Record of (string * mltype) list
 | UserDefined of string
 | Subtype of string * string
 | Function of mltype			(* arg type only *)
 | As of mltype * string

type template =
   StringArg of string
 | TypeArg of string * mltype
 | ListArg of template list
 | OptionalArgs of string * template list * template list

(* Sorts of components *)
type component_type =
   Constructor
 | Command
 | External

(* Full definition of a component *)
type fullcomponent = {
  component : component_type;
  ml_name : string; (* may be no longer useful *)
  var_name : string;
  template : template;
  result   : mltype;
  safe : bool
  }

let sort_components =
  Sort.list order:(fun c1 c2 ->  c1.ml_name < c2.ml_name)


(* components are given either in full or abbreviated *)
type component = 
   Full of fullcomponent
 | Abbrev of string

(* A type definition *)
(* 
 requires_widget_context: the converter of the type MUST be passed
   an additional argument of type Widget.
*)

type parser_arity =
  OneToken
| MultipleToken

type type_def = {
  parser_arity : parser_arity;
  mutable constructors : fullcomponent list;
  mutable subtypes : (string * fullcomponent list) list;
  mutable requires_widget_context : bool;
  mutable variant : bool
}

type module_type =
    Widget
  | Family

type module_def = {
  module_type : module_type;
  commands : fullcomponent list;
  externals : fullcomponent list
}

(******************** The tables ********************)

(* the table of all explicitly defined types *)
let types_table = (Hashtbl.create 37 : (string, type_def) Hashtbl.t)
(* "builtin" types *)
let types_external = ref ([] : (string * parser_arity) list)
(* dependancy order *)
let types_order = (Tsort.create () : string Tsort.porder)
(* Types of atomic values returned by Tk functions *)
let types_returned = ref  ([] : string list)
(* Function table *)
let function_table = ref ([] : fullcomponent list)
(* Widget/Module table *)
let module_table = (Hashtbl.create 37 : (string, module_def) Hashtbl.t)


(* variant name *)
let rec getvarname ml_name temp = 
  let offhypben s =
    let s = String.copy s in
    if (try String.sub s pos:0 len:1 with _ -> "") = "-" then
      String.sub s pos:1 len:(String.length s - 1)
    else s
  and makecapital s =
    begin
      try 
        let cd = s.[0] in
	  if cd >= 'a' && cd <= 'z' then
	    s.[0] <- Char.chr (Char.code cd + (Char.code 'A' - Char.code 'a'))
      with
      	_ -> ()
    end;
    s
  in
    let head =  makecapital (offhypben begin
                  match temp with
                    StringArg s -> s
                  | TypeArg (s,t) -> s  
                  | ListArg (h::_) -> getvarname ml_name h
                  | OptionalArgs (s,_,_) -> s
                  | ListArg [] -> ""
                end)
    in
    let varname = if head = "" then ml_name 
                  else if head.[0] >= 'A' && head.[0] <= 'Z' then head 
                       else ml_name
    in varname

(***** Some utilities on the various tables *****)
(* Enter a new empty type *)
let new_type typname arity = 
  Tsort.add_element types_order typname;
  let typdef = {parser_arity = arity;
                constructors = []; 
      	        subtypes = []; 
      	       	requires_widget_context = false;
	        variant = false} in
    Hashtbl.add types_table key:typname data:typdef;
    typdef


(* Assume that types not yet defined are not subtyped *)
(* Widget is builtin and implicitly subtyped *)
let is_subtyped s =
  s = "widget" or
  try  
    let typdef = Hashtbl.find types_table key:s in
      typdef.subtypes <> []
  with
    Not_found -> false

let requires_widget_context s = 
  try  
    (Hashtbl.find types_table key:s).requires_widget_context
  with
    Not_found -> false

let declared_type_parser_arity s = 
  try  
    (Hashtbl.find types_table key:s).parser_arity
  with
    Not_found -> 
      try List.assoc key:s !types_external
      with
      	Not_found ->
	   prerr_string "Type "; prerr_string s;
	   prerr_string " is undeclared external or undefined\n";
	   prerr_string ("Assuming cTKtoCAML"^s^" : string -> "^s^"\n");
	   OneToken

let rec type_parser_arity = function
   Unit -> OneToken
 | Int -> OneToken
 | Float -> OneToken
 | Bool -> OneToken
 | Char -> OneToken
 | String -> OneToken
 | List _ -> MultipleToken
 | Product _ -> MultipleToken
 | Record _ -> MultipleToken
 | UserDefined s -> declared_type_parser_arity s
 | Subtype (s,_) -> declared_type_parser_arity s
 | Function _ -> OneToken
 | As (ty, _) -> type_parser_arity ty

let enter_external_type s v =
  types_external := (s,v)::!types_external

(*** Stuff for topological Sort.list of types ***)
(* Make sure all types used in commands and functions are in *)
(* the table *)
let rec enter_argtype = function
    Unit | Int | Float | Bool | Char | String -> ()
  | List ty -> enter_argtype ty
  | Product tyl -> List.iter fun:enter_argtype tyl
  | Record tyl -> List.iter tyl fun:(fun (l,t) -> enter_argtype t)
  | UserDefined s -> Tsort.add_element types_order s
  | Subtype (s,_) -> Tsort.add_element types_order s
  | Function ty -> enter_argtype ty
  | As (ty, _) -> enter_argtype ty

let rec enter_template_types = function
     StringArg _ -> ()
   | TypeArg (l,t) -> enter_argtype t
   | ListArg l -> List.iter fun:enter_template_types l
   | OptionalArgs (_,tl,_) -> List.iter fun:enter_template_types tl 
 
(* Find type dependancies on s *)
let rec add_dependancies s =
  function
    List ty -> add_dependancies s ty
  | Product tyl -> List.iter fun:(add_dependancies s) tyl
  | Subtype(s',_) -> if s <> s' then Tsort.add_relation types_order (s', s)
  | UserDefined s' -> if s <> s' then Tsort.add_relation types_order (s', s)
  | Function ty -> add_dependancies s ty
  | As (ty, _) -> add_dependancies s ty
  | _ -> ()

let rec add_template_dependancies s = function
     StringArg _ -> ()
   | TypeArg (l,t) -> add_dependancies s t
   | ListArg l -> List.iter fun:(add_template_dependancies s) l
   | OptionalArgs (_,tl,_) -> List.iter fun:(add_template_dependancies s) tl

(* Assumes functions are not nested in products, which is reasonable due to syntax*)
let rec has_callback = function
     StringArg _ -> false
   | TypeArg (l,Function _ ) -> true
   | TypeArg _ -> false
   | ListArg l -> List.exists pred:has_callback l
   | OptionalArgs (_,tl,_) -> List.exists pred:has_callback tl

(*** Returned types ***)
let really_add ty = 
  if List.mem elt:ty !types_returned then ()
  else types_returned := ty :: !types_returned

let rec add_return_type = function
    Unit -> ()
  | Int -> ()
  | Float -> ()
  | Bool -> ()
  | Char -> ()
  | String -> ()
  | List ty -> add_return_type ty
  | Product tyl -> List.iter fun:add_return_type tyl
  | Record tyl -> List.iter tyl fun:(fun (l,t) -> add_return_type t) 
  | UserDefined s -> really_add s
  | Subtype (s,_) -> really_add s
  | Function _ -> fatal_error "unexpected return type (function)" (* whoah *)
  | As (ty, _) -> add_return_type ty

(*** Update tables for a component ***)
let enter_component_types {template = t; result = r} =
  add_return_type r;
  enter_argtype r;
  enter_template_types t


(******************** Types and subtypes ********************)
exception Duplicate_Definition of string * string
exception Invalid_implicit_constructor of string

(* Checking duplicate definition of constructor in subtypes *)
let rec check_duplicate_constr allowed c =
  function
    [] -> false		(* not defined *)
  | c'::rest -> 
    if c.ml_name = c'.ml_name then  (* defined *)
      if allowed then 
      	if c.template = c'.template then true (* same arg *)
	else raise (Duplicate_Definition ("constructor",c.ml_name))
      else raise (Duplicate_Definition ("constructor", c.ml_name))
    else check_duplicate_constr allowed c rest

(* Retrieve constructor *)
let rec find_constructor cname = function
   [] -> raise (Invalid_implicit_constructor cname)
 | c::l -> if c.ml_name = cname then c
       	   else find_constructor cname l

(* Enter a type, must not be previously defined *)
let enter_type typname ?:variant{=false} arity constructors =
  try
      Hashtbl.find types_table key:typname;
      raise (Duplicate_Definition ("type", typname))
  with Not_found ->
    let typdef = new_type typname arity in
    if variant then typdef.variant <- true;  
    List.iter constructors fun:
      begin fun c ->
	if not (check_duplicate_constr false c typdef.constructors)
	then begin 
	   typdef.constructors <- c :: typdef.constructors;
	   add_template_dependancies typname c.template
	end;
	(* Callbacks require widget context *)
	typdef.requires_widget_context <- 
       	  typdef.requires_widget_context or
                  has_callback c.template
      end

(* Enter a subtype *)
let enter_subtype typ arity subtyp constructors =
  (* Retrieve the type if already defined, else add a new one *)
  let typdef = 
    try Hashtbl.find types_table key:typ
    with Not_found -> new_type typ arity
  in
    if List.mem_assoc key:subtyp typdef.subtypes
    then raise (Duplicate_Definition ("subtype", typ ^" "^subtyp))
    else begin
      let real_constructors = 
      	List.map constructors fun:
	  begin function
	    Full c -> 
	      if not (check_duplicate_constr true c typdef.constructors)
	      then begin
		add_template_dependancies typ c.template;
		typdef.constructors <- c :: typdef.constructors
	      end;
	      typdef.requires_widget_context <-
      	       	typdef.requires_widget_context or
      	       	has_callback c.template;
              c
	  | Abbrev name -> find_constructor name typdef.constructors
	  end
      in
       (* TODO: duplicate def in subtype are not checked *)
       typdef.subtypes <-
	  (subtyp , Sort.list real_constructors
	              order:(fun c1 c2 -> c1.var_name <= c2.var_name)) ::
	  typdef.subtypes
    end

(******************** Widgets ********************)
(* used by the parser; when enter_widget is called,
   all components are assumed to be in Full form *)
let retrieve_option optname =
  let optiontyp =
    try Hashtbl.find types_table key:"options"
    with 
      Not_found -> raise (Invalid_implicit_constructor optname)
  in find_constructor optname optiontyp.constructors
  
(* Sort components by type *)
let rec add_sort acc:l obj =
  match l with
    []  -> [obj.component ,[obj]]
  | (s',l)::rest ->
     if obj.component = s' then
       (s',obj::l)::rest
     else 
       (s',l)::(add_sort acc:rest obj)

let separate_components =  List.fold_left fun:add_sort acc:[]

let enter_widget name components =
  try 
    Hashtbl.find module_table key:name;
    raise (Duplicate_Definition ("widget/module", name))
  with Not_found ->
  let sorted_components = separate_components components in
  List.iter sorted_components fun:
    begin function 
      Constructor, l ->
	enter_subtype "options" MultipleToken 
      	  name (List.map fun:(fun c -> Full c) l)
    | Command, l -> 
	List.iter fun:enter_component_types l
    | External, _ -> ()
    end;
  let commands = 
      try List.assoc key:Command sorted_components
      with Not_found -> [] 
  and externals = 
      try List.assoc key:External sorted_components
      with Not_found -> []
  in
  Hashtbl.add module_table key:name 
    data:{module_type = Widget; commands = commands; externals = externals}
  
(******************** Functions ********************)
let enter_function comp =
  enter_component_types comp;
  function_table := comp :: !function_table


(******************** Modules ********************)
let enter_module name components = 
  try 
    Hashtbl.find module_table key:name;
    raise (Duplicate_Definition ("widget/module", name))
  with Not_found ->
  let sorted_components = separate_components components in
  List.iter sorted_components fun:
    begin function 
      Constructor, l -> fatal_error "unexpected Constructor"
    | Command, l -> List.iter fun:enter_component_types l
    | External, _ -> ()
    end;
  let commands = 
      try List.assoc key:Command sorted_components
      with Not_found -> [] 
  and externals = 
      try List.assoc key:External sorted_components
      with Not_found -> []
  in
    Hashtbl.add module_table key:name 
      data:{module_type = Family; commands = commands; externals = externals}

