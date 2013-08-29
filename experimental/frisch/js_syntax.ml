(* This example shows how the AST mapping approach could be used
   instead of Camlp4 in order to give a nice syntax for js_of_ocaml
   (properties and method calls). The code below overloads regular
   syntax for field projection and assignment for Javascript
   properties, and (currified) method call for Javascript method
   calls. This is enabled under the scope of the [%js ...] extension:

     Get property:   [%js o.x]
     Set property:   [%js o.x <- e]
     Method call:    [%js o#x e1 e2]
 *)

open Asttypes
open! Location
open Parsetree
open Longident
open Ast_helper
open Ast_helper.Convenience

(* A few local helper functions to simplify the creation of AST nodes. *)
let apply_ f l = app (evar f) l
let oobject l = Typ.object_ l Open
let annot e t = Exp.constraint_ e t


let rnd = Random.State.make [|0x513511d4|]
let random_var () = Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L : Int64.t)
let fresh_type () = Typ.var (random_var ())

let unescape lab =
  assert (lab <> "");
  let lab =
    if lab.[0] = '_' then String.sub lab 1 (String.length lab - 1) else lab
  in
  try
    let i = String.rindex lab '_' in
    if i = 0 then raise Not_found;
    String.sub lab 0 i
  with Not_found ->
    lab

let method_literal meth = str (unescape meth)

let access_object loc e m m_typ f =
  let open Exp in
  with_default_loc loc
    (fun () ->
      let x = random_var () in
      let obj_type = random_var () in
      let obj = annot e Typ.(tconstr "Js.t" [alias (oobject []) obj_type]) in
      let y = random_var () in
      let o = annot (evar y) (Typ.var obj_type) in
      let constr = lam (pvar y) (annot (send o m) m_typ) in
      let_in [Vb.mk (pvar x) obj; Vb.mk (Pat.any ()) constr] (f (evar x))
    )

let method_call loc obj meth args =
  let args = List.map (fun e -> (e, fresh_type ())) args in
  let ret_type = fresh_type () in
  let method_type =
    List.fold_right
      (fun (_, arg_ty) rem_ty -> Typ.arrow "" arg_ty rem_ty)
      args
      (tconstr "Js.meth" [ret_type])
  in
  access_object loc obj meth method_type
    (fun x ->
      let args =
        List.map (fun (e, t) -> apply_ "Js.Unsafe.inject" [annot e t]) args
      in
      annot (apply_ "Js.Unsafe.meth_call" [x; method_literal meth; Exp.array args]) ret_type
    )


let mapper =
  object(this)
    inherit Ast_mapper.mapper as super

    val js = false

    method! expr e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_extension({txt="js";_}, PStr [{pstr_desc=Pstr_eval (e, _);_}]) ->
          {< js = true >} # expr e

      | Pexp_field (o, {txt = Lident meth; loc = _}) when js ->
          let o = this # expr o in
          let prop_type = fresh_type () in
          let meth_type = tconstr "Js.gen_prop" [oobject ["get", prop_type]] in
          access_object loc o meth meth_type
            (fun x -> annot (apply_ "Js.Unsafe.get" [x; method_literal meth]) prop_type)

      | Pexp_setfield (o, {txt = Lident meth; loc = _}, e) when js ->
          let o = this # expr o and e = this # expr e in
          let prop_type = fresh_type () in
          let meth_type = tconstr "Js.gen_prop" [oobject ["set", Typ.arrow "" prop_type (tconstr "unit" [])]] in
          access_object loc o meth meth_type
            (fun x -> apply_ "Js.Unsafe.set" [x; method_literal meth; annot e prop_type])

      | Pexp_apply ({pexp_desc = Pexp_send (o, meth); pexp_loc = loc; _}, args) when js ->
          method_call loc o meth (List.map (this # expr) (List.map snd args))

      | Pexp_send (o, meth) when js ->
          method_call loc o meth []

      | _ ->
          super # expr e
  end

let () = Ast_mapper.main mapper
