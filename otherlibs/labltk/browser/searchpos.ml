(* $Id$ *)

open Tk
open Jg_tk
open Parsetree
open Types
open Typedtree
open Location
open Longident
open Path
open Env
open Searchid

(* auxiliary functions *)

let lines_to_chars n in:s =
  let l = String.length s in
  let rec ltc n :pos =
    if n = 1 or pos >= l then pos else
    if s.[pos] = '\n' then ltc (n-1) pos:(pos+1) else ltc n pos:(pos+1)
  in ltc n pos:0

let in_loc loc :pos =
  pos >= loc.loc_start & pos < loc.loc_end

let rec string_of_longident = function
    Lident s -> s
  | Ldot (id,s) -> string_of_longident id ^ "." ^ s
  | Lapply (id1, id2) ->
      string_of_longident id1 ^ "(" ^ string_of_longident id2 ^ ")"

let string_of_path p = string_of_longident (Searchid.longident_of_path p)

let parent_path = function
    Pdot (path, _, _) -> Some path
  | Pident _ | Papply _ -> None

let ident_of_path :default = function
    Pident i -> i
  | Pdot (_, s, _) -> Ident.create s
  | Papply _ -> Ident.create default

let rec head_id = function
    Pident id -> id
  | Pdot (path,_,_) -> head_id path
  | Papply (path,_) -> head_id path (* wrong, but ... *)

let rec list_of_path = function
    Pident id -> [Ident.name id]
  | Pdot (path, s, _) -> list_of_path path @ [s]
  | Papply (path, _) -> list_of_path path (* wrong, but ... *)

(* a standard (diposable) buffer class *)

class buffer :len = object
  val mutable buffer = String.create :len
  val mutable length = len
  val mutable current = 0
  method out buffer:b :pos :len =
    while len + current > length do
      let newbuf = String.create len:(length * 2) in
      String.blit buffer pos:0 len:current to:newbuf to_pos:0;
      buffer <- newbuf;
      length <- 2 * length
    done;
    String.blit b :pos to:buffer to_pos:current :len;
    current <- current + len
  method get = String.sub buffer pos:0 len:current
end

(* Search in a signature *)

type skind = [`Type|`Class|`Module|`Modtype]

exception Found_sig of skind * Longident.t * Env.t

let rec search_pos_type t :pos :env =
  if in_loc :pos t.ptyp_loc then
  begin (match t.ptyp_desc with
    Ptyp_any
  | Ptyp_var _ -> ()
  | Ptyp_variant(tl, _, _) ->
      List.iter tl
	fun:(fun (_,_,tl) -> List.iter tl fun:(search_pos_type :pos :env))
  | Ptyp_arrow (_, t1, t2) ->
      search_pos_type t1 :pos :env;
      search_pos_type t2 :pos :env
  | Ptyp_tuple tl ->
      List.iter tl fun:(search_pos_type :pos :env)
  | Ptyp_constr (lid, tl) ->
      List.iter tl fun:(search_pos_type :pos :env);
      raise (Found_sig (`Type, lid, env))
  | Ptyp_object fl ->
      List.iter fl fun:
      	begin function
	| {pfield_desc = Pfield (_, ty)} -> search_pos_type ty :pos :env
	| _ -> ()
	end
  | Ptyp_class (lid, tl, _) ->
      List.iter tl fun:(search_pos_type :pos :env);
      raise (Found_sig (`Type, lid, env))
  | Ptyp_alias (t, _) -> search_pos_type :pos :env t);
  raise Not_found
  end

let rec search_pos_class_type cl :pos :env =
  if in_loc cl.pcty_loc :pos then begin
    begin match cl.pcty_desc with
      Pcty_constr (lid, _) ->
	raise (Found_sig (`Class, lid, env))
    | Pcty_signature (_, cfl) ->
	List.iter cfl fun:
	  begin function
	      Pctf_inher cty -> search_pos_class_type cty :pos :env
	    | Pctf_val (_, _, Some ty, loc) ->
		if in_loc loc :pos then search_pos_type ty :pos :env
	    | Pctf_val _ -> ()
	    | Pctf_virt (_, _, ty, loc) ->
		if in_loc loc :pos then search_pos_type ty :pos :env
	    | Pctf_meth (_, _, ty, loc) ->
		if in_loc loc :pos then search_pos_type ty :pos :env
	    | Pctf_cstr (ty1, ty2, loc) ->
		if in_loc loc :pos then begin
		  search_pos_type ty1 :pos :env;
		  search_pos_type ty2 :pos :env
		end
	  end
    | Pcty_fun (_, ty, cty) ->
	search_pos_type ty :pos :env;
	search_pos_class_type cty :pos :env
    end;
    raise Not_found
  end

let search_pos_type_decl td :pos :env =
  if in_loc :pos td.ptype_loc then begin
    begin match td.ptype_manifest with
      Some t -> search_pos_type t :pos :env
    | None -> ()
    end;
    begin match td.ptype_kind with
      Ptype_abstract -> ()
    | Ptype_variant dl ->
      	List.iter dl
	  fun:(fun (_, tl) -> List.iter tl fun:(search_pos_type :pos :env))
    | Ptype_record dl ->
      	List.iter dl fun:(fun (_, _, t) -> search_pos_type t :pos :env)
    end;
    raise Not_found
  end
  
let rec search_pos_signature l :pos :env =
  List.fold_left l acc:env fun:
  begin fun acc:env pt ->
    let env = match pt.psig_desc with
      Psig_open id ->
        let path, mt = lookup_module id env in
      	begin match mt with
	  Tmty_signature sign -> open_signature path sign env
	| _ -> env
	end
    | sign_item ->
	try add_signature (Typemod.transl_signature env [pt]) env
	with Typemod.Error _ | Typeclass.Error _
	| Typetexp.Error _  | Typedecl.Error _ -> env
    in
    if in_loc :pos pt.psig_loc then begin
      begin match pt.psig_desc with
      	Psig_value (_, desc) -> search_pos_type desc.pval_type :pos :env
      | Psig_type l ->
    	  List.iter l fun:(fun (_,desc) -> search_pos_type_decl :pos desc :env)
      | Psig_exception (_, l) ->
      	  List.iter l fun:(search_pos_type :pos :env);
       	  raise (Found_sig (`Type, Lident "exn", env))
      | Psig_module (_, t) -> 
          search_pos_module t :pos :env
      | Psig_modtype (_, Pmodtype_manifest t) ->
          search_pos_module t :pos :env
      | Psig_modtype _ -> ()
      | Psig_class l ->
      	  List.iter l
	    fun:(fun ci -> search_pos_class_type ci.pci_expr :pos :env)
      | Psig_class_type l ->
      	  List.iter l
	    fun:(fun ci -> search_pos_class_type ci.pci_expr :pos :env)
      (* The last cases should not happen in generated interfaces *) 
      | Psig_open lid -> raise (Found_sig (`Module, lid, env))
      | Psig_include t -> search_pos_module t :pos :env
      end;
      raise Not_found
    end;
    env
  end

and search_pos_module m :pos :env =
  if in_loc m.pmty_loc :pos then begin
    begin match m.pmty_desc with
      Pmty_ident lid -> raise (Found_sig (`Modtype, lid, env))
    | Pmty_signature sg -> let _ = search_pos_signature sg :pos :env in ()
    | Pmty_functor (_ , m1, m2) ->
      	search_pos_module m1 :pos :env;
      	search_pos_module m2 :pos :env
    | Pmty_with (m, l) ->
      	search_pos_module m :pos :env;
      	List.iter l fun:
	  begin function
              _, Pwith_type t -> search_pos_type_decl t :pos :env 
	    | _ -> ()
	  end
    end;
    raise Not_found
  end

(* the module display machinery *)

type module_widgets =
    { mw_frame: Widget.frame Widget.widget;
      mw_detach: Widget.button Widget.widget;
      mw_edit: Widget.button Widget.widget;
      mw_intf: Widget.button Widget.widget }

let shown_modules = Hashtbl.create 17
let filter_modules () =
  Hashtbl.iter shown_modules fun:
    begin fun :key :data ->
      if not (Winfo.exists data.mw_frame) then
	Hashtbl.remove :key shown_modules
    end
let add_shown_module path :widgets =
  Hashtbl.add shown_modules key:path data:widgets
and find_shown_module path =
  filter_modules ();
  Hashtbl.find shown_modules key:path

(* Viewing a signature *)

(* Forward definitions of Viewer.view_defined and Editor.editor *)
let view_defined_ref = ref (fun lid :env -> ())
let editor_ref = ref (fun ?:file ?:pos ?:opendialog () -> ())

let edit_source :file :path :sign =
  match sign with
    [item] ->
      let id, kind =
	match item with
	  Tsig_value (id, _) -> id, Pvalue
	| Tsig_type (id, _) -> id, Ptype
	| Tsig_exception (id, _) -> id, Pconstructor
	| Tsig_module (id, _) -> id, Pmodule
	| Tsig_modtype (id, _) -> id, Pmodtype
	| Tsig_class (id, _) -> id, Pclass
	| Tsig_cltype (id, _) -> id, Pcltype
      in
      let prefix = List.tl (list_of_path path) and name = Ident.name id in
      let pos =
	try
	  let chan = open_in file in
	  if Filename.check_suffix file suff:".ml" then
	    let parsed = Parse.implementation (Lexing.from_channel chan) in
	    close_in chan;
	    Searchid.search_structure parsed :name :kind :prefix
	  else
	    let parsed = Parse.interface (Lexing.from_channel chan) in
	    close_in chan;
	    Searchid.search_signature parsed :name :kind :prefix
	with _ -> 0
      in !editor_ref :file :pos ()
  | _ -> !editor_ref :file ()

(* List of windows to destroy by Close All *)
let top_widgets = ref []

let rec view_signature ?:title ?:path ?:env{= !start_env} sign =
  let env =
    match path with None -> env
    | Some path -> Env.open_signature path sign env in
  let title =
    match title, path with Some title, _ -> title
    | None, Some path -> string_of_path path
    | None, None -> "Signature"
  in
  let tl, tw, finish =
    try match path with
      None -> raise Not_found
    | Some path ->
	let widgets =
	  try find_shown_module path
	  with Not_found ->
	    view_module path :env;
	    find_shown_module path
	in
	Button.configure widgets.mw_detach
	  command:(fun () -> view_signature sign :title :env);
	pack [widgets.mw_detach] side:`Left;
	Pack.forget [widgets.mw_edit; widgets.mw_intf];
	List.iter2 [widgets.mw_edit; widgets.mw_intf] [".ml"; ".mli"] fun:
	  begin fun button ext ->
	    try
	      let id = head_id path in
	      let file =
		Misc.find_in_path !Config.load_path
		  (String.uncapitalize (Ident.name id) ^ ext) in
	      Button.configure button
		command:(fun () -> edit_source :file :path :sign);
	      pack [button] side:`Left
	    with Not_found -> ()
	  end;
	let top = Winfo.toplevel widgets.mw_frame in
  	if not (Winfo.ismapped top) then Wm.deiconify top;
	Focus.set top;
	List.iter fun:destroy (Winfo.children widgets.mw_frame);
	Jg_message.formatted :title on:widgets.mw_frame maxheight:15 ()
    with Not_found ->
      let tl, tw, finish = Jg_message.formatted :title maxheight:15 () in
      top_widgets := tl :: !top_widgets;
      tl, tw, finish
  in
  Format.set_max_boxes 100;
  Printtyp.signature sign;
  finish ();
  Lexical.init_tags tw;
  Lexical.tag tw;
  Text.configure tw state:`Disabled;
  let text = Jg_text.get_all tw in
  let pt =
      try Parse.interface (Lexing.from_string text)
      with Syntaxerr.Error e ->
	let l =
	  match e with
	    Syntaxerr.Unclosed(l,_,_,_) -> l
	  | Syntaxerr.Other l -> l
	in
      	Jg_text.tag_and_see  tw start:(tpos l.loc_start)
	  end:(tpos l.loc_end) tag:"error"; []
      | Lexer.Error (_, s, e) ->
      	 Jg_text.tag_and_see tw start:(tpos s) end:(tpos e) tag:"error"; []
  in
  Jg_bind.enter_focus tw;
  bind tw events:[[`Control], `KeyPressDetail"s"]
    action:(`Set ([], fun _ -> Jg_text.search_string tw));
  bind tw events:[[`Double], `ButtonPressDetail 1]
    action:(`Setbreakable ([`MouseX;`MouseY], fun ev ->
      let `Linechar (l, c) =
 	Text.index tw index:(`Atxy(ev.ev_MouseX,ev.ev_MouseY), []) in
      try try
	search_pos_signature pt pos:(lines_to_chars l in:text + c) :env;
      	break ()
      with Found_sig (kind, lid, env) -> view_decl lid :kind :env
      with Not_found | Env.Error _ -> ()));
  bind tw events:[[], `ButtonPressDetail 3]
    action:(`Setbreakable ([`MouseX;`MouseY], fun ev ->
      let x = ev.ev_MouseX and y = ev.ev_MouseY in
      let `Linechar (l, c) =
 	Text.index tw index:(`Atxy(x,y), []) in
      try try
	search_pos_signature pt pos:(lines_to_chars l in:text + c) :env;
      	break ()
      with Found_sig (kind, lid, env) ->
	let menu = view_decl_menu lid :kind :env parent:tw in
	let x = x + Winfo.rootx tw and y = y + Winfo.rooty tw - 10 in
	Menu.popup menu :x :y
      with Not_found -> ()))

and view_signature_item sign :path :env =
  view_signature sign title:(string_of_path path) ?path:(parent_path path) :env

and view_module path :env =
  match find_module path env with
    Tmty_signature sign -> 
      !view_defined_ref (Searchid.longident_of_path path) :env
  | modtype ->
      let id = ident_of_path path default:"M" in
      view_signature_item [Tsig_module (id, modtype)] :path :env

and view_module_id id :env =
  let path, _ = lookup_module id env in
  view_module path :env

and view_type_decl path :env =
  let td = find_type path env in
  try match td.type_manifest with None -> raise Not_found
    | Some ty -> match Ctype.repr ty with
	{desc = Tobject _} ->
	  let clt = find_cltype path env in
	  view_signature_item :path :env
	    [Tsig_cltype(ident_of_path path default:"ct", clt)]
      |	_ -> raise Not_found
  with Not_found ->
    view_signature_item :path :env
      [Tsig_type(ident_of_path path default:"t", td)]

and view_type_id li :env =
  let path, decl = lookup_type li env in
  view_type_decl path :env

and view_class_id li :env =
  let path, cl = lookup_class li env in
  view_signature_item :path :env
     [Tsig_class(ident_of_path path default:"c", cl)]

and view_cltype_id li :env =
  let path, clt = lookup_cltype li env in
  view_signature_item :path :env
     [Tsig_cltype(ident_of_path path default:"ct", clt)]

and view_modtype_id li :env =
  let path, td = lookup_modtype li env in
  view_signature_item :path :env
    [Tsig_modtype(ident_of_path path default:"S", td)]

and view_expr_type ?:title ?:path ?:env ?:name{="noname"} t =
  let title =
    match title, path with Some title, _ -> title
    | None, Some path -> string_of_path path
    | None, None -> "Expression type"
  and path, id =
    match path with None -> None, Ident.create name
    | Some path -> parent_path path, ident_of_path path default:name
  in
  view_signature :title ?:path ?:env
    [Tsig_value (id, {val_type = t; val_kind = Val_reg})]

and view_decl lid :kind :env =
  match kind with
    `Type -> view_type_id lid :env
  | `Class -> view_class_id lid :env
  | `Module -> view_module_id lid :env
  | `Modtype -> view_modtype_id lid :env

and view_decl_menu lid :kind :env :parent =
  let path, kname =
    try match kind with
      `Type -> fst (lookup_type lid env), "Type"
    | `Class -> fst (lookup_class lid env), "Class"
    | `Module -> fst (lookup_module lid env), "Module"
    | `Modtype -> fst (lookup_modtype lid env), "Module type"
    with Env.Error _ -> raise Not_found
  in
  let menu = Menu.create :parent tearoff:false () in
  let label = kname ^ " " ^ string_of_path path in
  begin match path with
    Pident _ ->
      Menu.add_command menu :label state:`Disabled
  | _ ->
      Menu.add_command menu :label
    	command:(fun () -> view_decl lid :kind :env);
  end;
  if kind = `Type or kind = `Modtype then begin
    let buf = new buffer len:60 in
    let (fo,ff) = Format.get_formatter_output_functions ()
    and margin = Format.get_margin () in
    Format.set_formatter_output_functions out:buf#out flush:(fun () -> ());
    Format.set_margin 60;
    Format.open_hbox ();
    if kind = `Type then
      Printtyp.type_declaration
	(ident_of_path path default:"t")
	(find_type path env)
    else
      Printtyp.modtype_declaration
	(ident_of_path path default:"S")
	(find_modtype path env);
    Format.close_box (); Format.print_flush ();
    Format.set_formatter_output_functions out:fo flush:ff;
    Format.set_margin margin;
    let l = Str.split sep:(Str.regexp "\n") buf#get in
    let font =
      let font =
	Option.get Widget.default_toplevel name:"font" class:"Font" in
      if font = "" then "7x14" else font
    in
    (* Menu.add_separator menu; *)
    List.iter l
      fun:(fun label -> Menu.add_command menu :label :font state:`Disabled)
  end;
  menu

(* search and view in a structure *)

type fkind =
    [ `Exp [`Expr|`Pat|`Const|`Val Path.t|`Var Path.t|`New Path.t]
	* Types.type_expr
    | `Class Path.t * Types.class_type
    | `Module Path.t * Types.module_type ]
exception Found_str of fkind * Env.t

let view_type kind :env =
  match kind with
    `Exp (k, ty) ->
      begin match k with
	`Expr -> view_expr_type ty title:"Expression type" :env
      | `Pat -> view_expr_type ty title:"Pattern type" :env
      | `Const -> view_expr_type ty title:"Constant type" :env
      | `Val path ->
	  begin try
	    let vd = find_value path env in
	    view_signature_item :path :env
	      [Tsig_value(ident_of_path path default:"v", vd)]
	  with Not_found ->
	    view_expr_type ty :path :env
	  end
      | `Var path ->
	  let vd = find_value path env in
	  view_expr_type vd.val_type :env :path title:"Variable type"
      | `New path ->
	  let cl = find_class path env in
	  view_signature_item :path :env
     	    [Tsig_class(ident_of_path path default:"c", cl)]
      end
  | `Class (path, cty) ->
      let cld = { cty_params = []; cty_type = cty;
		  cty_path = path; cty_new = None } in
      view_signature_item :path :env
     	[Tsig_class(ident_of_path path default:"c", cld)]
  | `Module (path, mty) ->
      match mty with
	Tmty_signature sign -> view_signature sign :path :env
      |	modtype ->
	  view_signature_item :path :env
	    [Tsig_module(ident_of_path path default:"M", mty)]

let view_type_menu kind :env :parent =
  let title =
    match kind with
      `Exp (`Expr,_) -> "Expression :"
    | `Exp (`Pat, _) -> "Pattern :"
    | `Exp (`Const, _) -> "Constant :"
    | `Exp (`Val path, _) -> "Value " ^ string_of_path path ^ " :"
    | `Exp (`Var path, _) ->
	"Variable " ^ Ident.name (ident_of_path path default:"noname") ^ " :"
    | `Exp (`New path, _) -> "Class " ^ string_of_path path ^ " :"
    | `Class (path, _) -> "Class " ^ string_of_path path ^ " :"
    | `Module (path,_) -> "Module " ^ string_of_path path in
  let menu = Menu.create :parent tearoff:false () in
  begin match kind with
    `Exp((`Expr | `Pat | `Const | `Val (Pident _)),_)  ->
      Menu.add_command menu label:title state:`Disabled
  | `Exp _ | `Class _ | `Module _ ->
      Menu.add_command menu label:title
	command:(fun () -> view_type kind :env)
  end;
  begin match kind with `Module _ | `Class _ -> ()
  | `Exp(_, ty) ->
      let buf = new buffer len:60 in
      let (fo,ff) = Format.get_formatter_output_functions ()
      and margin = Format.get_margin () in
      Format.set_formatter_output_functions out:buf#out	flush:(fun () -> ());
      Format.set_margin 60;
      Format.open_hbox ();
      Printtyp.reset ();
      Printtyp.mark_loops ty;
      Printtyp.type_expr ty;
      Format.close_box (); Format.print_flush ();
      Format.set_formatter_output_functions out:fo flush:ff;
      Format.set_margin margin;
      let l = Str.split sep:(Str.regexp "\n") buf#get in
      let font =
	let font =
	  Option.get Widget.default_toplevel name:"font" class:"Font" in
	if font = "" then "7x14" else font
      in
      (* Menu.add_separator menu; *)
      List.iter l fun:
	begin fun label -> match (Ctype.repr ty).desc with
	  Tconstr (path,_,_) ->
	    Menu.add_command menu :label :font
	      command:(fun () -> view_type_decl path :env)
	| Tvariant {row_name = Some (path, _)} ->
	    Menu.add_command menu :label :font
	      command:(fun () -> view_type_decl path :env)
	| _ ->
	    Menu.add_command menu :label :font state:`Disabled
	end
  end;
  menu

let rec search_pos_structure :pos str =
  List.iter str fun:
  begin function
    Tstr_eval exp -> search_pos_expr exp :pos
  | Tstr_value (rec_flag, l) ->
      List.iter l fun:
      begin fun (pat, exp) ->
	let env =
	  if rec_flag = Asttypes.Recursive then exp.exp_env else Env.empty in
      	search_pos_pat pat :pos :env;
      	search_pos_expr exp :pos
      end
  | Tstr_primitive (_, vd) ->()
  | Tstr_type _ -> ()
  | Tstr_exception _ -> ()
  | Tstr_module (_, m) -> search_pos_module_expr m :pos
  | Tstr_modtype _ -> ()
  | Tstr_open _ -> ()
  | Tstr_class l ->
      List.iter l fun:(fun (id, _, _, cl) -> search_pos_class_expr cl :pos)
  | Tstr_cltype _ -> ()
  end

and search_pos_class_expr :pos cl =
  if in_loc cl.cl_loc :pos then begin
    begin match cl.cl_desc with
      Tclass_ident path ->
	raise (Found_str (`Class (path, cl.cl_type), !start_env))
    | Tclass_structure cls ->
	List.iter cls.cl_field fun:
	  begin function
	      Cf_inher (cl, _, _) ->
		search_pos_class_expr cl :pos
	    | Cf_val (_, _, exp) -> search_pos_expr exp :pos
	    | Cf_meth (_, exp) -> search_pos_expr exp :pos
	    | Cf_let (_, pel, iel) ->
		List.iter pel fun:
		  begin fun (pat, exp) ->
		    search_pos_pat pat :pos env:exp.exp_env;
		    search_pos_expr exp :pos
		  end;
		List.iter iel fun:(fun (_,exp) -> search_pos_expr exp :pos)
	    | Cf_init exp -> search_pos_expr exp :pos
	  end
    | Tclass_fun (pat, iel, cl, _) ->
	search_pos_pat pat :pos env:pat.pat_env;
	List.iter iel fun:(fun (_,exp) -> search_pos_expr exp :pos);
	search_pos_class_expr cl :pos
    | Tclass_apply (cl, el) ->
	search_pos_class_expr cl :pos;
	List.iter el fun:(Misc.may (search_pos_expr :pos))
    | Tclass_let (_, pel, iel, cl) ->
	List.iter pel fun:
	  begin fun (pat, exp) ->
	    search_pos_pat pat :pos env:exp.exp_env;
	    search_pos_expr exp :pos
	  end;
	List.iter iel fun:(fun (_,exp) -> search_pos_expr exp :pos);
	search_pos_class_expr cl :pos
    | Tclass_constraint (cl, _, _, _) ->
	search_pos_class_expr cl :pos
    end;
    raise (Found_str
	     (`Class (Pident (Ident.create "c"), cl.cl_type), !start_env))
  end

and search_pos_expr :pos exp =
  if in_loc exp.exp_loc :pos then begin
  begin match exp.exp_desc with
    Texp_ident (path, _) ->
      raise (Found_str (`Exp(`Val path, exp.exp_type), exp.exp_env))
  | Texp_constant v ->
      raise (Found_str (`Exp(`Const, exp.exp_type), exp.exp_env))
  | Texp_let (_, expl, exp) ->
      List.iter expl fun:
      begin fun (pat, exp') ->
      	search_pos_pat pat :pos env:exp.exp_env;
      	search_pos_expr exp' :pos
      end;
      search_pos_expr exp :pos
  | Texp_function (l, _) ->
      List.iter l fun:
      begin fun (pat, exp) ->
      	search_pos_pat pat :pos env:exp.exp_env;
	search_pos_expr exp :pos
      end
  | Texp_apply (exp, l) ->
      List.iter l fun:(Misc.may (search_pos_expr :pos));
      search_pos_expr exp :pos
  | Texp_match (exp, l, _) ->
      search_pos_expr exp :pos;
      List.iter l fun:
      begin fun (pat, exp) ->
      	search_pos_pat pat :pos env:exp.exp_env;
	search_pos_expr exp :pos
      end
  | Texp_try (exp, l) ->
      search_pos_expr exp :pos;
      List.iter l fun:
      begin fun (pat, exp) ->
      	search_pos_pat pat :pos env:exp.exp_env;
	search_pos_expr exp :pos
      end
  | Texp_tuple l -> List.iter l fun:(search_pos_expr :pos)
  | Texp_construct (_, l) -> List.iter l fun:(search_pos_expr :pos)
  | Texp_variant (_, None) -> ()
  | Texp_variant (_, Some exp) -> search_pos_expr exp :pos
  | Texp_record (l, opt) ->
      List.iter l fun:(fun (_, exp) -> search_pos_expr exp :pos);
      (match opt with None -> () | Some exp -> search_pos_expr exp :pos)
  | Texp_field (exp, _) -> search_pos_expr exp :pos
  | Texp_setfield (a, _, b) ->
      search_pos_expr a :pos; search_pos_expr b :pos
  | Texp_array l -> List.iter l fun:(search_pos_expr :pos)
  | Texp_ifthenelse (a, b, c) ->
      search_pos_expr a :pos; search_pos_expr b :pos;
      begin match c with None -> ()
      | Some exp -> search_pos_expr exp :pos
      end
  | Texp_sequence (a,b) ->
      search_pos_expr a :pos; search_pos_expr b :pos
  | Texp_while (a,b) ->
      search_pos_expr a :pos; search_pos_expr b :pos
  | Texp_for (_, a, b, _, c) ->
      List.iter [a;b;c] fun:(search_pos_expr :pos)
  | Texp_when (a, b) ->
      search_pos_expr a :pos; search_pos_expr b :pos
  | Texp_send (exp, _) -> search_pos_expr exp :pos
  | Texp_new (path, _) ->
      raise (Found_str (`Exp(`New path, exp.exp_type), exp.exp_env))
  | Texp_instvar (_,path) ->
      raise (Found_str (`Exp(`Var path, exp.exp_type), exp.exp_env))
  | Texp_setinstvar (_, path, exp) ->
      search_pos_expr exp :pos;
      raise (Found_str (`Exp(`Var path, exp.exp_type), exp.exp_env))
  | Texp_override (_, l) ->
      List.iter l fun:(fun (_, exp) -> search_pos_expr exp :pos)
  | Texp_letmodule (id, modexp, exp) ->
      search_pos_module_expr modexp :pos;
      search_pos_expr exp :pos
  end;
  raise (Found_str (`Exp(`Expr, exp.exp_type), exp.exp_env))
  end

and search_pos_pat :pos :env pat =
  if in_loc pat.pat_loc :pos then begin
  begin match pat.pat_desc with
    Tpat_any -> ()
  | Tpat_var id ->
      raise (Found_str (`Exp(`Val (Pident id), pat.pat_type), env))
  | Tpat_alias (pat, _) -> search_pos_pat pat :pos :env
  | Tpat_constant _ ->
      raise (Found_str (`Exp(`Const, pat.pat_type), env))
  | Tpat_tuple l ->
      List.iter l fun:(search_pos_pat :pos :env)
  | Tpat_construct (_, l) ->
      List.iter l fun:(search_pos_pat :pos :env)
  | Tpat_variant (_, None, _) -> ()
  | Tpat_variant (_, Some pat, _) -> search_pos_pat pat :pos :env
  | Tpat_record l ->
      List.iter l fun:(fun (_, pat) -> search_pos_pat pat :pos :env)
  | Tpat_array l ->
      List.iter l fun:(search_pos_pat :pos :env)
  | Tpat_or (a, b) ->
      search_pos_pat a :pos :env; search_pos_pat b :pos :env
  end;
  raise (Found_str (`Exp(`Pat, pat.pat_type), env))
  end

and search_pos_module_expr :pos m =
  if in_loc m.mod_loc :pos then begin
    begin match m.mod_desc with
      Tmod_ident path ->
	raise
	  (Found_str (`Module (path, m.mod_type), m.mod_env))
    | Tmod_structure str -> search_pos_structure str :pos
    | Tmod_functor (_, _, m) -> search_pos_module_expr m :pos
    | Tmod_apply (a, b, _) ->
	search_pos_module_expr a :pos; search_pos_module_expr b :pos
    | Tmod_constraint (m, _, _) -> search_pos_module_expr m :pos
    end;
    raise (Found_str (`Module (Pident (Ident.create "M"), m.mod_type),
		      m.mod_env))
  end
