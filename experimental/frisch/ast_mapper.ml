open Location
open Config
open Parsetree
open Asttypes

(* First, some helpers to build AST fragments *)

let map_flatten f l = List.flatten (List.map f l)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_opt f = function None -> None | Some x -> Some (f x)

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

  let map sub {pstr_loc = loc; pstr_desc = desc} =
    match desc with
    | Pstr_eval x -> eval ~loc (sub # expr x)
    | Pstr_value (r, pel) -> value ~loc r (List.map (map_tuple (sub # pat) (sub # expr)) pel)
    | Pstr_primitive (name, vd) -> primitive ~loc name (sub # value_description vd)
    | Pstr_type l -> typ ~loc (List.map (fun (s, d) -> (s, sub # type_declaration d)) l)
    | Pstr_exception (name, ed) -> exn ~loc name (List.map (sub # typ) ed)
    | Pstr_exn_rebind (s, lid) -> exn_rebind ~loc s lid
    | Pstr_module (s, m) -> module_ ~loc s (sub # module_expr m)
    | Pstr_recmodule l -> rec_module ~loc (List.map (fun (s, mty, me) -> (s, sub # module_type mty, sub # module_expr me)) l)
    | Pstr_modtype (s, mty) -> modtype ~loc s (sub # module_type mty)
    | Pstr_open lid -> open_ ~loc lid
    | Pstr_class l -> class_ ~loc (List.map (sub # class_declaration) l)
    | Pstr_class_type l -> class_type ~loc (List.map (sub # class_type_declaration) l)
    | Pstr_include e -> include_ ~loc (sub # module_expr e)
end

module E = struct
  (* Expressions *)

  let mk ?(loc = Location.none) x = {pexp_desc = x; pexp_loc = loc}

  let ident ?loc a = mk ?loc (Pexp_ident a)
  let const ?loc a = mk ?loc (Pexp_constant a)
  let let_ ?loc a b c = mk ?loc (Pexp_let (a, b, c))
  let func ?loc a b c = mk ?loc (Pexp_function (a, b, c))
  let apply_with_labels ?loc a b = mk ?loc (Pexp_apply (a, b))
  let match_ ?loc a b = mk ?loc (Pexp_match (a, b))
  let try_ ?loc a b = mk ?loc (Pexp_try (a, b))
  let tuple ?loc a = mk ?loc (Pexp_tuple a)
  let constr ?loc a b c = mk ?loc (Pexp_construct (a, b, c))
  let variant ?loc a b = mk ?loc (Pexp_variant (a, b))
  let record ?loc a b = mk ?loc (Pexp_record (a, b))
  let field ?loc a b = mk ?loc (Pexp_field (a, b))
  let setfield ?loc a b c = mk ?loc (Pexp_setfield (a, b, c))
  let array ?loc a = mk ?loc (Pexp_array a)
  let ifthenelse ?loc a b c = mk ?loc (Pexp_ifthenelse (a, b, c))
  let sequence ?loc a b = mk ?loc (Pexp_sequence (a, b))
  let while_ ?loc a b = mk ?loc (Pexp_while (a, b))
  let for_ ?loc a b c d e = mk ?loc (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc a b c = mk ?loc (Pexp_constraint (a, b, c))
  let when_ ?loc a b = mk ?loc (Pexp_when (a, b))
  let send ?loc a b = mk ?loc (Pexp_send (a, b))
  let new_ ?loc a = mk ?loc (Pexp_new a)
  let setinstvar ?loc a b = mk ?loc (Pexp_setinstvar (a, b))
  let override ?loc a = mk ?loc (Pexp_override a)
  let letmodule ?loc (a, b, c)= mk ?loc (Pexp_letmodule (a, b, c))
  let assert_ ?loc a = mk ?loc (Pexp_assert a)
  let assertfalse ?loc () = mk ?loc Pexp_assertfalse
  let lazy_ ?loc a = mk ?loc (Pexp_lazy a)
  let poly ?loc a b = mk ?loc (Pexp_poly (a, b))
  let object_ ?loc a = mk ?loc (Pexp_object a)
  let newtype ?loc a b = mk ?loc (Pexp_newtype (a, b))
  let pack ?loc a = mk ?loc (Pexp_pack a)
  let open_ ?loc a b = mk ?loc (Pexp_open (a, b))

  let lid ?(loc = Location.none) lid = ident ~loc (mkloc (Longident.parse lid) loc)
  let apply ?loc f el = apply_with_labels ?loc f (List.map (fun e -> ("", e)) el)
  let strconst ?loc x = const ?loc (Const_string x)

  let map sub {pexp_loc = loc; pexp_desc = desc} =
    match desc with
    | Pexp_ident x -> ident ~loc x
    | Pexp_constant x -> const ~loc x
    | Pexp_let (r, pel, e) -> let_ ~loc r (List.map (map_tuple (sub # pat) (sub # expr)) pel) (sub # expr e)
    | Pexp_function (lab, def, pel) -> func ~loc lab (map_opt (sub # expr) def) (List.map (map_tuple (sub # pat) (sub # expr)) pel)
    | Pexp_apply (e, l) -> apply_with_labels ~loc (sub # expr e) (List.map (map_snd (sub # expr)) l)
    | Pexp_match (e, l) -> match_ ~loc (sub # expr e) (List.map (map_tuple (sub # pat) (sub # expr)) l)
    | Pexp_try (e, l) -> try_ ~loc (sub # expr e) (List.map (map_tuple (sub # pat) (sub # expr)) l)
    | Pexp_tuple el -> tuple ~loc (List.map (sub # expr) el)
    | Pexp_construct (lid, arg, b) -> constr ~loc lid (map_opt (sub # expr) arg) b
    | Pexp_variant (lab, eo) -> variant ~loc lab (map_opt (sub # expr) eo)
    | Pexp_record (l, eo) -> record ~loc (List.map (map_snd (sub # expr)) l) (map_opt (sub # expr) eo)
    | Pexp_field (e, lid) -> field ~loc (sub # expr e) lid
    | Pexp_setfield (e1, lid, e2) -> setfield ~loc (sub # expr e1) lid (sub # expr e2)
    | Pexp_array el -> array ~loc (List.map (sub # expr) el)
    | Pexp_ifthenelse (e1, e2, e3) -> ifthenelse ~loc (sub # expr e1) (sub # expr e2) (map_opt (sub # expr) e3)
    | Pexp_sequence (e1, e2) -> sequence ~loc (sub # expr e1) (sub # expr e2)
    | Pexp_while (e1, e2) -> while_ ~loc (sub # expr e1) (sub # expr e2)
    | Pexp_for (id, e1, e2, d, e3) -> for_ ~loc id (sub # expr e1) (sub # expr e2) d (sub # expr e3)
    | Pexp_constraint (e, t1, t2) -> constraint_ ~loc (sub # expr e) (map_opt (sub # typ) t1) (map_opt (sub # typ) t2)
    | Pexp_when (e1, e2) -> when_ ~loc (sub # expr e1) (sub # expr e2)
    | Pexp_send (e, s) -> send ~loc (sub # expr e) s
    | Pexp_new lid -> new_ ~loc lid
    | Pexp_setinstvar (s, e) -> setinstvar ~loc s (sub # expr e)
    | Pexp_override sel -> override ~loc (List.map (map_snd (sub # expr)) sel)
    | Pexp_letmodule (s, me, e) -> letmodule ~loc (s, sub # module_expr me, sub # expr e)
    | Pexp_assert e -> assert_ ~loc (sub # expr e)
    | Pexp_assertfalse -> assertfalse ~loc ()
    | Pexp_lazy e -> lazy_ ~loc (sub # expr e)
    | Pexp_poly (e, t) -> poly ~loc (sub # expr e) (map_opt (sub # typ) t)
    | Pexp_object cls -> object_ ~loc (sub # class_structure cls)
    | Pexp_newtype (s, e) -> newtype ~loc s (sub # expr e)
    | Pexp_pack me -> pack ~loc (sub # module_expr me)
    | Pexp_open (lid, e) -> open_ ~loc lid (sub # expr e)
end

module T = struct
  (* Core types *)

  let mk ?(loc = Location.none) x = {ptyp_desc = x; ptyp_loc = loc}
  let any ?loc () = mk ?loc Ptyp_any
  let var ?loc a = mk ?loc (Ptyp_var a)
  let arrow ?loc a b c = mk ?loc (Ptyp_arrow (a, b, c))
  let tuple ?loc a = mk ?loc (Ptyp_tuple a)
  let constr ?loc a b = mk ?loc (Ptyp_constr (a, b))
  let object_ ?loc a = mk ?loc (Ptyp_object a)
  let class_ ?loc a b c = mk ?loc (Ptyp_class (a, b, c))
  let alias ?loc a b = mk ?loc (Ptyp_alias (a, b))
  let variant ?loc a b c = mk ?loc (Ptyp_variant (a, b, c))
  let poly ?loc a b = mk ?loc (Ptyp_poly (a, b))
  let package ?loc a b = mk ?loc (Ptyp_package (a, b))

  let core_field_type sub = function
    | {pfield_desc = Pfield (s, d); pfield_loc} ->
        {pfield_desc = Pfield (s, sub # typ d); pfield_loc}
    | x -> x

  let row_field sub = function
    | Rtag (l, b, tl) -> Rtag (l, b, List.map (sub # typ) tl)
    | Rinherit t -> Rinherit (sub # typ t)

  let map sub {ptyp_desc = desc; ptyp_loc = loc} =
    match desc with
    | Ptyp_any -> any ~loc ()
    | Ptyp_var s -> var ~loc s
    | Ptyp_arrow (lab, t1, t2) -> arrow ~loc lab (sub # typ t1) (sub # typ t2)
    | Ptyp_tuple tyl -> tuple ~loc (List.map (sub # typ) tyl)
    | Ptyp_constr (lid, tl) -> constr ~loc lid (List.map (sub # typ) tl)
    | Ptyp_object l -> object_ ~loc (List.map (core_field_type sub) l)
    | Ptyp_class (lid, tl, ll) -> class_ ~loc lid (List.map (sub # typ) tl) ll
    | Ptyp_alias (t, s) -> alias ~loc (sub # typ t) s
    | Ptyp_variant (rl, b, ll) -> variant ~loc (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) -> poly ~loc sl (sub # typ t)
    | Ptyp_package (lid, l) -> package ~loc lid (List.map (map_snd (sub # typ)) l)
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

  let map sub {pmod_loc = loc; pmod_desc = desc} =
    match desc with
    | Pmod_ident x -> ident ~loc x
    | Pmod_structure str -> structure ~loc (sub # structure str)
    | Pmod_functor (arg, arg_ty, body) -> funct ~loc arg (sub # module_type arg_ty) (sub # module_expr body)
    | Pmod_apply (m1, m2) -> apply ~loc (sub # module_expr m1) (sub # module_expr m2)
    | Pmod_constraint (m, mty) -> constr ~loc (sub # module_expr m) (sub # module_type mty)
    | Pmod_unpack e -> unpack ~loc (sub # expr e)
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

    method implementation (input_name : string) ast = (input_name, this # structure ast)
    method interface (input_name: string) ast = (input_name, this # signature ast)
    method structure l = map_flatten (this # structure_item) l
    method structure_item si = [ SI.map this si ]
    method module_expr = M.map this

    method signature l = map_flatten (this # signature_item) l
    method signature_item (x : signature_item) = [ x ] (* todo *)
    method module_type x = x (* todo *)

    method class_declaration x = x (* todo *)
    method class_type_declaration x = x (* todo *)
    method class_structure {pcstr_pat; pcstr_fields} =
      {
       pcstr_pat = this # pat pcstr_pat;
       pcstr_fields = List.map (this # class_field) pcstr_fields;
      }
    method class_field x = x (* ... *)

    method type_declaration x = x (* todo *)
    method typ = T.map this

    method value_description vd =
      {vd with pval_type = this # typ vd.pval_type}
    method pat p = p (* todo *)
    method expr = E.map this
  end
