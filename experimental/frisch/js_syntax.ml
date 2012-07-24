open Asttypes
open Ast_mapper
open Location
open Parsetree
open Longident

let constr1 c l = T.constr (mknoloc (Longident.parse c)) l
let apply1 f l = E.apply (E.lid f) l
let annot e t = E.constraint_ e (Some t) None


let rnd = Random.State.make [|0x513511d4|]
let random_var () =
  Format.sprintf "a%08Lx" (Random.State.int64 rnd 0x100000000L)

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

let access_object loc e m m_typ f =
  let open E in
  let x = random_var () in
  let obj_type = random_var () in
  let obj = annot e T.(constr1 "Js.t" [alias (object_ [field_var ()]) obj_type]) in
  let y = random_var () in
  let o = annot (ident (mknoloc (Lident y))) (T.var obj_type) in
  let body = annot (send o m) m_typ in
  let constr = func "" None [P.var (mknoloc y), body] in
  let e = let_ Nonrecursive [P.var (mknoloc x), obj; P.any (), constr] (f (E.ident (mknoloc (Lident x)))) in
  (set_loc loc) # expr e

let method_call loc obj meth args =
  let args = List.map (fun e -> (e, fresh_type ())) args in
  let ret_type = fresh_type () in
  let method_type =
    List.fold_right
      (fun (_, arg_ty) rem_ty -> T.arrow "" arg_ty rem_ty)
      args (constr1 "Js.meth" [ret_type])
  in
  access_object loc obj meth method_type
    (fun x ->
      let args =
        List.map (fun (e, t) -> apply1 "Js.Unsafe.inject" [annot e t]) args
      in
      annot E.(apply1 "Js.Unsafe.meth_call" [x; strconst (unescape meth); array args]) ret_type
    )


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
          let meth_type = constr1 "Js.gen_prop" [T.(object_ [field "get" prop_type; field_var ()])] in
          access_object loc o meth meth_type
            (fun x ->
              let open E in
              annot (apply1 "Js.Unsafe.get" [x; strconst (unescape meth)]) prop_type
            )

      | Pexp_setfield (o, {txt = Lident meth}, e) when js ->
          let loc = e.pexp_loc in
          let o = this # expr o in
          let e = this # expr e in
          let prop_type = fresh_type () in
          let meth_type = constr1 "Js.gen_prop" [T.(object_ [field "set" (arrow "" prop_type (constr (mknoloc (Lident "unit")) [])); field_var ()])] in
          access_object loc o meth meth_type
            (fun x ->
              let open E in
              apply1 "Js.Unsafe.set" [x; strconst (unescape meth); annot e prop_type]
            )

      | Pexp_apply ({pexp_desc = Pexp_send (o, meth); pexp_loc = loc}, args) when js ->
          method_call loc o meth (List.map snd args)
      | Pexp_send (o, meth) when js ->
          method_call e.pexp_loc o meth []

      | _ ->
          super # expr e
  end

let () = mapper # main
