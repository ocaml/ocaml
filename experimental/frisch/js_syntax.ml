open Asttypes
open Ast_mapper
open Location
open Parsetree
open Longident

let rnd = Random.State.make [|0x513511d4|]
let random_var () =
  Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L)

let fresh_type () = T.var (random_var ())

let js_t t = T.constr (mknoloc (Longident.parse "Js.t")) [t]
let js_gen_prop t = T.constr (mknoloc (Longident.parse "Js.gen_prop")) [t]

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

let annot e t = E.constraint_ e (Some t) None

let access_object loc e m m_typ f =
  let open E in
  let x = random_var () in
  let obj_type = random_var () in
  let obj = annot e T.(js_t (alias (object_ [field_var ()]) obj_type)) in
  let y = random_var () in
  let o = annot (ident (mknoloc (Lident y))) (T.var obj_type) in
  let body = annot (send o m) m_typ in
  let constr = func "" None [P.var (mknoloc y), body] in
  let e = let_ Nonrecursive [P.var (mknoloc x), obj; P.any (), constr] (f (E.ident (mknoloc (Lident x)))) in
  (set_loc loc) # expr e


let mapper =
  object(this)
    inherit Ast_mapper.create as super

    val js = false

    method! expr e =
      match e.pexp_desc with
      | Pexp_open ({txt = Lident "JS"}, e) ->
          {< js = true >} # expr e

      | Pexp_field (o, {txt = Lident meth}) when js ->
          let loc = e.pexp_loc in
          let o = this # expr o in
          let prop_type = fresh_type () in
          let meth_type = js_gen_prop (T.(object_ [field "get" prop_type; field_var ()])) in
          access_object loc o meth meth_type
            (fun x ->
              let open E in
              annot (apply (lid "Js.Unsafe.get") [x; strconst (unescape meth)]) prop_type
            )

      | Pexp_setfield (o, {txt = Lident meth}, e) when js ->
          let loc = e.pexp_loc in
          let o = this # expr o in
          let e = this # expr e in
          let prop_type = fresh_type () in
          let meth_type = js_gen_prop (T.(object_ [field "set" (arrow "" prop_type (constr (mknoloc (Lident "unit")) [])); field_var ()])) in
          access_object loc o meth meth_type
            (fun x ->
              let open E in
              apply (lid "Js.Unsafe.set") [x; strconst (unescape meth); annot e prop_type]
            )

      | _ ->
          super # expr e
  end

let () = mapper # main
