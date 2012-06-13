(* An example of a simple AST -> AST rewriter *)


open Location
open Config
open Parsetree
open Asttypes

(* First, some helpers to build AST fragments *)

let map_flatten f l = List.flatten (List.map f l)

let str ?(loc = Location.none) x = {pstr_desc = x; pstr_loc = loc}
let str_eval ?loc e = str ?loc (Pstr_eval e)
let str_value ?loc r pel = str ?loc (Pstr_value (r, pel))
let str_module ?loc s m = str ?loc (Pstr_module (s, m))

module E = struct
  let mk ?(loc = Location.none) x = {pexp_desc = x; pexp_loc = loc}
  let ident ?loc x = mk ?loc (Pexp_ident x)
  let lid ?(loc = Location.none) lid = ident ~loc (mkloc (Longident.parse lid) loc)
  let let_ ?loc r pel e = mk ?loc (Pexp_let (r, pel, e))
  let app ?loc f el = mk ?loc (Pexp_apply (f, List.map (fun e -> ("", e)) el))
  let const ?loc x = mk ?loc (Pexp_constant x)
  let strconst ?loc x = const ?loc (Const_string x)
end

let pmod ?(loc = Location.none) x = {pmod_desc = x; pmod_loc = loc}
let mod_ident ?loc x = pmod ?loc (Pmod_ident x)
let mod_structure ?loc x = pmod ?loc (Pmod_structure x)


(* Now, a generic AST mapper class, to be extended to cover all kinds
   and cases of the OCaml grammar.  The default behavior of the mapper
   is the identity. *)

class ast_mapper =
  object(this)
    method run fn_in fn_out =
      let ic = open_in_bin fn_in in
      let magic = String.create (String.length ast_impl_magic_number) in
      really_input ic magic 0 (String.length magic);
      if magic <> ast_impl_magic_number && magic <> ast_intf_magic_number then
        failwith "Bad magic";
      let input_name = input_value ic in
      let ast = input_value ic in
      close_in ic;

      let (input_name, ast) =
        if magic = ast_impl_magic_number
        then Obj.magic (this # implementation input_name (Obj.magic ast))
        else Obj.magic (this # interface input_name (Obj.magic ast))
      in
      let oc = open_out_bin fn_out in
      output_string oc magic;
      output_value oc input_name;
      output_value oc ast;
      close_out oc

    method implementation = this # default_implementation
    method default_implementation (input_name : string) ast = (input_name, this # structure ast)

    method interface = this # default_interface
    method default_interface (input_name : string) ast = (input_name, this # signature ast)

    method structure = this # default_structure
    method default_structure l = map_flatten (this # structure_item) l

    method signature = this # default_signature
    method default_signature l = map_flatten (this # signature_item) l

        (* signature items *)
    method signature_item = this # default_signature_item
    method default_signature_item (x : signature_item) = [ x ] (* todo *)

        (* structure items *)
    method structure_item = this # default_structure_item
    method default_structure_item ({pstr_loc = loc; pstr_desc = desc} as x) : structure_item list =
      match desc with
      | Pstr_eval x -> this # str_eval loc x
      | Pstr_value (r, pel) -> this # str_value loc r pel
      | Pstr_module (s, m) -> this # str_module loc s m
            (* ... *)
      | _ -> [ x ]

    method str_eval = this # default_str_eval
    method default_str_eval loc x = [ str_eval ~loc (this # expr x) ]

    method str_value = this # default_str_value
    method default_str_value loc r pel = [ str_value ~loc r (List.map (fun (p, e) -> this # pat p, this # expr e) pel) ]

    method str_module = this # default_str_module
    method default_str_module loc s m = [ str_module ~loc s (this # module_expr m) ]

        (* patterns *)
    method pat = this # default_pat
    method default_pat p = p

        (* expressions *)
    method expr = this # default_expr
    method default_expr ({pexp_loc = loc; pexp_desc = desc} as x) =
      match desc with
      | Pexp_ident x -> this # exp_ident loc x
      | Pexp_let (r, pel, e) -> this # exp_let loc r pel e
            (* ... *)
      | _ -> x

    method exp_ident = this # default_exp_ident
    method default_exp_ident loc x = E.ident ~loc x

    method exp_let = this # default_exp_let
    method default_exp_let loc r pel e = E.let_ ~loc r pel e

        (* module exprs *)

    method module_expr = this # default_module_expr
    method default_module_expr ({pmod_loc = loc; pmod_desc = desc} as x) =
      match desc with
      | Pmod_ident x -> this # mod_ident loc x
      | Pmod_structure str -> this # mod_structure loc str
            (* ... *)
      | _ -> x

    method mod_ident = this # default_mod_ident
    method default_mod_ident loc x = mod_ident ~loc x

    method mod_structure = this # default_mod_structure
    method default_mod_structure loc x = mod_structure ~loc (this # structure x)
  end



(*********************************************************************)

(* To define a concrete AST rewriter, we can inherit from the generic
   mapper, and redefine the cases we are interested in.  In the
   example below, we insert in the AST some debug statements around
   each module structure. We also keep track of the current "path" in
   the compilation unit.  *)

let trace s =
  str_eval E.(app (lid "Pervasives.print_endline") [strconst s])

class tracer =
  object
    inherit ast_mapper as super
    val path = ""

    method! implementation input_name structure =
      let path = String.capitalize (Filename.chop_extension input_name) in
      {< path = path >} # default_implementation input_name structure

    method! str_module loc s m =
      {< path = path ^ "." ^ s.txt >} # default_str_module loc s m

    method! structure l =
      trace (Printf.sprintf "Entering module %s" path) ::
      (super # structure l) @
      [ trace (Printf.sprintf "Leaving module %s" path) ]
  end

let () =
  try
    match Sys.argv with
    | [| _; fn_in; fn_out |] -> new tracer # run fn_in fn_out
    | _ -> prerr_endline "Usage: tracer <infile> <outfile>"; exit 1
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2
