open Location
open Config
open Parsetree
open Asttypes

(* First, some helpers to build AST fragments *)

let map_flatten f l = List.flatten (List.map f l)
let map_snd f (x, y) = (x, f y)

module SI = struct
  (* Structure items *)

  let mk ?(loc = Location.none) x = {pstr_desc = x; pstr_loc = loc}
  let eval ?loc e = mk ?loc (Pstr_eval e)
  let value ?loc r pel = mk ?loc (Pstr_value (r, pel))
  let primitive ?loc name vd = mk ?loc (Pstr_primitive (name, vd))
  let typ ?loc tdecls = mk ?loc (Pstr_type tdecls)
  let exn ?loc name edecl = mk ?loc (Pstr_exception (name, edecl))
  let exn_rebind ?loc name lid = mk ?loc (Pstr_exn_rebind (name, lid))
  let module_ ?loc s m = mk ?loc (Pstr_module (s, m))
  let rec_module ?loc rm = mk ?loc (Pstr_recmodule rm)
  let modtype ?loc s mty = mk ?loc (Pstr_modtype (s, mty))
  let open_ ?loc lid = mk ?loc (Pstr_open lid)
  let class_ ?loc l = mk ?loc (Pstr_class l)
  let class_type ?loc l = mk ?loc (Pstr_class_type l)
  let include_ ?loc me = mk ?loc (Pstr_include me)
end

module E = struct
  (* Expressions *)

  let mk ?(loc = Location.none) x = {pexp_desc = x; pexp_loc = loc}
  let ident ?loc x = mk ?loc (Pexp_ident x)
  let lid ?(loc = Location.none) lid = ident ~loc (mkloc (Longident.parse lid) loc)
  let let_ ?loc r pel e = mk ?loc (Pexp_let (r, pel, e))
  let apply_with_labels ?loc f el = mk ?loc (Pexp_apply (f, el))
  let apply ?loc f el = apply_with_labels ?loc f (List.map (fun e -> ("", e)) el)
  let const ?loc x = mk ?loc (Pexp_constant x)
  let strconst ?loc x = const ?loc (Const_string x)
end

module T = struct
  (* Core types *)

  let mk ?(loc = Location.none) x = {ptyp_desc = x; ptyp_loc = loc}
  let any ?loc () = mk ?loc Ptyp_any
  let var ?loc s = mk ?loc (Ptyp_var s)
  let arrow ?loc ?(lab = "") t1 t2 = mk ?loc (Ptyp_arrow (lab, t1, t2))
  let tuple ?loc tyl = mk ?loc (Ptyp_tuple tyl)
end

module M = struct
  (* Module expressions *)

  let mk ?(loc = Location.none) x = {pmod_desc = x; pmod_loc = loc}
  let ident ?loc x = mk ?loc (Pmod_ident x)
  let structure ?loc x = mk ?loc (Pmod_structure x)
  let funct ?loc arg arg_ty body = mk ?loc (Pmod_functor (arg, arg_ty, body))
  let apply ?loc m1 m2 = mk ?loc (Pmod_apply (m1, m2))
  let constr ?loc m mty = mk ?loc (Pmod_constraint (m, mty))
  let unpack ?loc e = mk ?loc (Pmod_unpack e)
end


(* Now, a generic AST mapper class, to be extended to cover all kinds
   and cases of the OCaml grammar.  The default behavior of the mapper
   is the identity. *)

class create =
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

    method main =
      try
        if Array.length Sys.argv > 2 then
          this # run Sys.argv.(1) Sys.argv.(2)
        else begin
          Printf.eprintf "Usage: %s <infile> <outfile>" Sys.executable_name;
          exit 1
        end
      with exn ->
        prerr_endline (Printexc.to_string exn);
        exit 2

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
    method default_signature_item (x : signature_item) = [ x ] (* ... *)

        (* structure items *)
    method structure_item = this # default_structure_item
    method default_structure_item {pstr_loc = loc; pstr_desc = desc} : structure_item list =
      match desc with
      | Pstr_eval x -> this # str_eval ~loc x
      | Pstr_value (r, pel) -> this # str_value ~loc r pel
      | Pstr_primitive (name, vd) -> this # str_primitive ~loc name vd
      | Pstr_type l -> this # str_type ~loc l
      | Pstr_exception (s, e) -> this # str_exception ~loc s e
      | Pstr_exn_rebind (s, lid) -> this # str_exn_rebind ~loc s lid
      | Pstr_module (s, m) -> this # str_module ~loc s m
      | Pstr_recmodule l -> this # str_recmodule ~loc l
      | Pstr_modtype (s, mty) -> this # str_modtype ~loc s mty
      | Pstr_open lid -> this # str_open ~loc lid
      | Pstr_class l -> this # str_class ~loc l
      | Pstr_class_type l -> this # str_class_type ~loc l
      | Pstr_include e -> this # str_include ~loc e

    method str_eval = this # default_str_eval
    method default_str_eval ~loc x = [ SI.eval ~loc (this # expr x) ]

    method str_value = this # default_str_value
    method default_str_value ~loc r pel = [ SI.value ~loc r (List.map (fun (p, e) -> this # pat p, this # expr e) pel) ]

    method str_primitive = this # default_str_primitive
    method default_str_primitive ~loc name vd = [ SI.primitive ~loc name (this # value_description vd) ]

    method str_type = this # default_str_type
    method default_str_type ~loc l =
      [ SI.typ ~loc (List.map (fun (s, d) -> (s, this # type_declaration d)) l) ]

    method str_exception = this # default_str_exception
    method default_str_exception ~loc s ed =
      [ SI.exn ~loc s (List.map (this # typ) ed) ]

    method str_exn_rebind = this # default_str_exn_rebind
    method default_str_exn_rebind ~loc s lid =
      [ SI.exn_rebind ~loc s lid ]

    method str_module = this # default_str_module
    method default_str_module ~loc s m = [ SI.module_ ~loc s (this # module_expr m) ]

    method str_recmodule = this # default_str_recmodule
    method default_str_recmodule ~loc l =
      let f (s, mty, me) = (s, this # module_type mty, this # module_expr me) in
      [ SI.rec_module ~loc (List.map f l) ]

    method str_modtype = this # default_str_modtype
    method default_str_modtype ~loc s mty =
      [ SI.modtype ~loc s (this # module_type mty) ]

    method str_open = this # default_str_open
    method default_str_open ~loc lid =
      [ SI.open_ ~loc lid ]

    method str_class = this # default_str_class
    method default_str_class ~loc l =
      [ SI.class_ ~loc (List.map (this # class_declaration) l) ]

    method str_class_type = this # default_str_class_type
    method default_str_class_type ~loc l =
      [ SI.class_type ~loc (List.map (this # class_type_declaration) l) ]

    method str_include = this # default_str_include
    method default_str_include ~loc me =
      [ SI.include_ ~loc (this # module_expr me) ]

        (* class declarations *)
    method class_declaration = this # default_class_declaration
    method default_class_declaration x = x (* ... *)

        (* class type declarations *)
    method class_type_declaration = this # default_class_type_declaration
    method default_class_type_declaration x = x (* ... *)

        (* type declarations *)
    method type_declaration = this # default_type_declaration
    method default_type_declaration x = x (* ... *)

        (* value descriptions *)
    method value_description = this # default_value_description
    method default_value_description vd =
      {vd with pval_type = this # typ vd.pval_type}

(* core types *)
    method typ x = this # default_typ x
    method default_typ ({ptyp_desc = desc; ptyp_loc = loc} as x) =
      match desc with
      | Ptyp_any -> this # typ_any ~loc
      | Ptyp_var s -> this # typ_var ~loc s
      | Ptyp_arrow (lab, t1, t2) -> this # typ_arrow ~loc lab t1 t2
      | Ptyp_tuple tyl -> this # typ_tuple ~loc tyl
            (* ... *)
      | _ -> x

    method typ_any = this # default_typ_any
    method default_typ_any ~loc = T.any ~loc ()

    method typ_var = this # default_typ_var
    method default_typ_var ~loc s = T.var ~loc s

    method typ_arrow = this # default_typ_arrow
    method default_typ_arrow ~loc lab t1 t2 =
      T.arrow ~loc ~lab (this # typ t1) (this # typ t2)

    method typ_tuple = this # default_typ_tuple
    method default_typ_tuple ~loc tyl = T.tuple ~loc (List.map (this # typ) tyl)


        (* patterns *)
    method pat = this # default_pat
    method default_pat p = p (* ... *)

        (* expressions *)
    method expr = this # default_expr
    method default_expr ({pexp_loc = loc; pexp_desc = desc} as x) =
      match desc with
      | Pexp_ident x -> this # exp_ident ~loc x
      | Pexp_let (r, pel, e) -> this # exp_let ~loc r pel e
      | Pexp_apply (e, l) -> this # exp_apply ~loc e l
            (* ... *)
      | _ -> x

    method exp_ident = this # default_exp_ident
    method default_exp_ident ~loc x = E.ident ~loc x

    method exp_let = this # default_exp_let
    method default_exp_let ~loc r pel e =
      E.let_ ~loc r
        (List.map (fun (p, e) -> this # pat p, this # expr e) pel)
        (this # expr e)

    method exp_apply = this # default_exp_apply
    method default_exp_apply ~loc e l =
      E.apply_with_labels ~loc (this # expr e) (List.map (map_snd (this # expr)) l)

        (* module exprs *)

    method module_expr = this # default_module_expr
    method default_module_expr {pmod_loc = loc; pmod_desc = desc} =
      match desc with
      | Pmod_ident x -> this # mod_ident ~loc x
      | Pmod_structure str -> this # mod_structure ~loc str
      | Pmod_functor (arg, arg_ty, body) -> this # mod_functor ~loc arg arg_ty body
      | Pmod_apply (m1, m2) -> this # mod_apply ~loc m1 m2
      | Pmod_constraint (m, mty) -> this # mod_constraint ~loc m mty
      | Pmod_unpack e -> this # mod_unpack ~loc e

    method mod_ident = this # default_mod_ident
    method default_mod_ident ~loc x = M.ident ~loc x

    method mod_structure = this # default_mod_structure
    method default_mod_structure ~loc x =
      M.structure ~loc (this # structure x)

    method mod_functor = this # default_mod_functor
    method default_mod_functor ~loc arg arg_ty body =
      M.funct ~loc arg (this # module_type arg_ty) (this # module_expr body)

    method mod_apply = this # default_mod_apply
    method default_mod_apply ~loc m1 m2 =
      M.apply ~loc (this # module_expr m1) (this # module_expr m2)

    method mod_constraint = this # default_mod_constraint
    method default_mod_constraint ~loc m mty =
      M.constr ~loc (this # module_expr m) (this # module_type mty)

    method mod_unpack = this # default_mod_unpack
    method default_mod_unpack ~loc e = M.unpack ~loc (this # expr e)

(* module types *)
    method module_type = this # default_module_type
    method default_module_type x = x (* ... *)
  end
