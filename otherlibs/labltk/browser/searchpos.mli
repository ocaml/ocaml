(* $Id$ *)

open Widget

val top_widgets : any widget list ref

type module_widgets =
    { mw_frame: frame widget;
      mw_detach: button widget;
      mw_edit: button widget;
      mw_intf: button widget }

val add_shown_module : Path.t -> widgets:module_widgets -> unit
val find_shown_module : Path.t -> module_widgets

val view_defined_ref : (Longident.t -> env:Env.t -> unit) ref
val editor_ref :
    (?file:string -> ?pos:int -> ?opendialog:bool -> unit -> unit) ref

val view_signature :
  ?title:string -> ?path:Path.t -> ?env:Env.t -> Types.signature -> unit
val view_signature_item :
  Types.signature -> path:Path.t -> env:Env.t -> unit
val view_module_id : Longident.t -> env:Env.t -> unit
val view_type_id : Longident.t -> env:Env.t -> unit
val view_class_id : Longident.t -> env:Env.t -> unit
val view_cltype_id : Longident.t -> env:Env.t -> unit
val view_modtype_id : Longident.t -> env:Env.t -> unit
val view_type_decl : Path.t -> env:Env.t -> unit

type skind = [`Type|`Class|`Module|`Modtype]
exception Found_sig of skind * Longident.t * Env.t
val search_pos_signature :
  Parsetree.signature -> pos:int -> env:Env.t -> Env.t
      (* raises Found_sig to return its result, or Not_found *)
val view_decl : Longident.t -> kind:skind -> env:Env.t -> unit
val view_decl_menu :
    Longident.t ->
    kind:skind -> env:Env.t -> parent:text widget -> menu widget

type fkind =
    [ `Exp [`Expr|`Pat|`Const|`Val Path.t|`Var Path.t|`New Path.t]
	* Types.type_expr
    | `Class Path.t * Types.class_type
    | `Module Path.t * Types.module_type ]
exception Found_str of fkind * Env.t
val search_pos_structure :
  pos:int -> Typedtree.structure_item list -> unit
      (* raises Found_str to return its result *)
val view_type : fkind -> env:Env.t -> unit
val view_type_menu : fkind -> env:Env.t -> parent:'a widget -> menu widget

val parent_path : Path.t -> Path.t option
val string_of_path : Path.t -> string
val string_of_longident : Longident.t -> string
val lines_to_chars : int -> in:string -> int

