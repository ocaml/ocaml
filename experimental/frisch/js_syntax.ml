(* This example shows how the AST mapping approach could be used
   instead of Camlp4 in order to give a nice syntax for js_of_ocaml
   (properties and method calls). The code below overloads regular
   syntax for field projection and assignment for Javascript
   properties, and (currified) method call for Javascript method
   calls. This is enabled by a fake local open on pseudo module JS,
   i.e.  in a scope like "JS.(...)" or "let open JS in ...".
 *)

open Asttypes
open Ast_mapper
open Location
open Parsetree
open Longident

(* A few local helper functions to simplify the creation of AST nodes. *)
let constr_ c l = T.constr (mknoloc (Longident.parse c)) l
let apply_ f l = E.apply_nolabs (E.lid f) l
let oobject l = T.object_ (List.map (fun (s, t) -> T.field s t) l @ [T.field_var ()])
let eident x = E.ident (mknoloc (Lident x))
let pvar x = P.var (mknoloc x)
let annot e t = E.constraint_ e (Some t) None



let rnd = Random.State.make [|0x513511d4|]
let random_var () = Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L : Int64.t)
let fresh_type () = T.var (random_var ())

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

let method_literal meth = E.strconst (unescape meth)

let access_object loc e m m_typ f =
  let open E in
  let x = random_var () in
  let obj_type = random_var () in
  let obj = annot e T.(constr_ "Js.t" [alias (oobject []) obj_type]) in
  let y = random_var () in
  let o = annot (eident y) (T.var obj_type) in
  let constr = function_ "" None [pvar y, annot (send o m) m_typ] in
  let e = let_ Nonrecursive [pvar x, obj; P.any (), constr] (f (eident x)) in
  (set_loc loc) # expr e

let method_call loc obj meth args =
  let args = List.map (fun e -> (e, fresh_type ())) args in
  let ret_type = fresh_type () in
  let method_type =
    List.fold_right
      (fun (_, arg_ty) rem_ty -> T.arrow "" arg_ty rem_ty)
      args
      (constr_ "Js.meth" [ret_type])
  in
  access_object loc obj meth method_type
    (fun x ->
      let args =
        List.map (fun (e, t) -> apply_ "Js.Unsafe.inject" [annot e t]) args
      in
      annot (apply_ "Js.Unsafe.meth_call" [x; method_literal meth; E.array args]) ret_type
    )


let mapper =
  object(this)
    inherit Ast_mapper.create as super

    val js = false

    method! expr e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_open ({txt = Lident "JVS"; loc = _}, e) ->
          {< js = true >} # expr e

      | Pexp_field (o, {txt = Lident meth; loc = _}) when js ->
          let o = this # expr o in
          let prop_type = fresh_type () in
          let meth_type = constr_ "Js.gen_prop" [oobject ["get", prop_type]] in
          access_object loc o meth meth_type
            (fun x -> annot (apply_ "Js.Unsafe.get" [x; method_literal meth]) prop_type)

      | Pexp_setfield (o, {txt = Lident meth; loc = _}, e) when js ->
          let o = this # expr o and e = this # expr e in
          let prop_type = fresh_type () in
          let meth_type = constr_ "Js.gen_prop" [oobject ["set", T.arrow "" prop_type (constr_ "unit" [])]] in
          access_object loc o meth meth_type
            (fun x -> apply_ "Js.Unsafe.set" [x; method_literal meth; annot e prop_type])

      | Pexp_apply ({pexp_desc = Pexp_send (o, meth); pexp_loc = loc}, args) when js ->
          method_call loc o meth (List.map snd args)

      | Pexp_send (o, meth) when js ->
          method_call loc o meth []

      | _ ->
          super # expr e
  end

let () = mapper # main
