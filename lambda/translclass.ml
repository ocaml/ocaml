(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*
   # Translation of class and object expressions

   ## Objects

   ### Memory layout

   Objects are represented in memory using two layers:
   - The outer layer is a block with tag [Obj.object_tag].
     It has a first field pointing to the inner layer (the methods),
     a second field acting as a unique identifier to allow
     polymorphic comparison, and the rest of the block contains
     the values of the instance variables, class parameters, and
     other values that can vary between two objects of the same class.

   - The inner layer is a regular block (with tag zero). It contains
     all values that are shared between all objects of the same class,
     which means mostly methods. The first field corresponds to the number of
     public methods, the second field is a mask used for optimising method
     access, the following fields are alternating between the method closures
     and the hash of their name (sorted in increasing hash order).
     Additional fields are used for private methods.

   +-------+------+-------+-------+-----+-------+-------+-----------+
   | n_pub | mask | met_1 | tag_1 | ... | met_n | tag_n | other ... |
   +-------+------+-------+-------+-----+-------+-------+-----------+

   ### Primitives

   Method access is compiled in one of three possible ways:
   - Generic method access (outside a class, or to an object that is not
     self or an ancestor) uses dynamic lookup. A dichotomic search in
     the part of the method array that stores public methods finds
     the expected closure and calls it on the current object.
     In most cases, a fast path also exists: each method access in the
     source code has an associated cache location that stores the offset
     of the last method called at this point in its method array.
     Before the dichotomic search, the last stored offset (clamped
     to the actual size of the method array using the mask) is checked,
     and if the tag matches the associated closure is called directly.
   - Method access through the self object inside a class:
     the (runtime) index of the method inside the method array
     has been computed at class creation time, so the method is fetched
     from the block through a dynamic block load (like an array load).
   - Accessing the method of an ancestor inside a class (ancestors are
     variables bound by [inherit ... as ancestor] constructions):
     at class creation time, the closure of the ancestor method is bound
     to a variable, and the method call just calls this function without
     any (further) dynamic lookup.

   Instance variable access (getting and setting) also computes offsets
   at class initialisation time, with those offsets used to index directly
   in the outer layer of the object.

   Functional object copy [ {< ... >} ] copies the outer layer, resets the
   unique ID, and performs the required instance variable updates.

   There are no other object primitives (objects cannot be allocated
   in the IR directly, they are allocated in [CamlinternalOO])

   ## Classes

   Classes are stored as module fields. The runtime value that represents
   classes is used in two contexts:

   - When using the [new] construction, to generate an object from a class.
   - When referencing a class inside another class (either through
     inheritance or other class expressions).

   This is done by storing classes as blocks where the first field
   is used to generate objects, and the second field is used to derive
   classes (in a general sense, not only for inheritance).
   In practice classes also contain one other field, which is used to
   implement some optimisations in the main compiler (to ensure that each
   class only runs its initialisation code once in the whole program, even
   if its definition is in a context that is expected to be run several
   times like a functor).
   So the block layout is the following:
   - A field named [obj_init] that is used for creating objects
   - A field named [class_init] that is used for deriving classes
   - A field named [env] containing values for all the variables
     captured by [Translobj.oo_wrap] calls.

   The module [CamlinternalOO] also defines a type [table] that represents
   class layouts. Such values are not stored in the class block directly,
   but the [obj_init] field captures the table for the class and [class_init]
   manipulates such tables.

   ### The [obj_init] field

   As described earlier, each object contains an inner layer that is computed
   only once at class initialisation time; it seems natural to store this
   block in the runtime value of the class (this block is one of the fields of
   the [CamlinternalOO.table] type). However, given that creating an
   object also involves setting up the instance variables and running the
   initialisers, in practice the class only exports a function that creates
   objects, and the table is captured in this function's closure along with
   any value necessary to properly initialise the object.
   Classes can have parameters, so in practice this object creation function
   takes a first unit parameter (to ensure that it is always a function)
   and returns a regular OCaml value that is either an object (if the class
   doesn't have parameters) or a function which, given values
   for the class parameters, will return an object.

   Here is the type of the [obj_init] function for a class which type is
   [p1 -> ... -> pn -> object method m1 : t1 ... method mn : tn end]:
   [unit -> p1 -> ... -> pn -> < m1 : t1; ... mn : tn >]
   (If the class has instance variables or initialisers, they are not
   reflected in the type of [obj_init]).

   ### The [class_init] field

   This field is used in two cases:
   - When a class is defined in terms of another class, for instance as an
     alias, a partial application, or some other kind of wrapper.
   - When a class structure (i.e. the [object ... end] syntactic construction)
     contains inheritance fields (e.g. [inherit cl as super]).

   In both cases, we only have access to the other class' public type at
   compile time, but we must still make sure all of the private fields
   are setup correctly, in a way that is compatible with the current
   class.

   This is where tables come into play: the [class_init] field is a function
   taking a table as parameter, updates it in-place, and returns a function
   that is very similar to the [obj_init] function, except that instead of
   taking [unit] as its first parameter and returning an object, it takes
   a partially initialised object, and updates the parts of it that are
   relevant for the corresponding class. It also takes the [env] field as
   a parameter, so that different instances of the class can share the
   same [class_init] function.

   Thus, the type of [class_init] is:
   [table -> env -> Obj.t -> p1 -> ... -> pn -> unit]

   ### The [env] field

   The [env] field is a structure similar to a function's closure, storing
   the value of free variables of the class expression. The actual
   representation is a bit complex and not very important.

   ### Compilation scheme

   The algorithm implemented below aims at sharing code as much as possible
   between the various similar parts of the class.

   - The code of the [obj_init] function is very similar to the code of
     the function returned by [class_init]. The main difference is that
     [obj_init] starts from scratch, allocating then initialising the object,
     while inside [class_init] we want to run initialisation code on an already
     allocated object (that we don't need to return).
     So in practice we will build a single function that, depending on the value
     of its first parameter, will either do the allocation and return the object
     (if the parameter is the integer constant 0), or assume the parameter is
     an already allocated and update it.
     The body of this function is returned by [build_object_init].
   - The table for the current class (that [obj_init] will read from) is
     computed by allocating a basic table, then passing it to [class_init],
     and finally calling [CamlinternalOO.init_class] on it.
     This means that all the code for setting up the class (computing instance
     variable indices, calling inherited class initialisers, and so on) is only
     generated once, in the [class_init] function.
     After building [obj_init], [build_class_init] wraps it with the class
     initialization code to build the [class_init] function.

   That's all for the high-level algorithm; the rest will be detailed close to
   the corresponding code.

*)

open Asttypes
open Types
open Typedtree
open Lambda
open Translobj
open Translcore
open Debuginfo.Scoped_location

(* XXX Rajouter des evenements... | Add more events... *)

type error = Tags of label * label

exception Error of Location.t * error

let lfunction params body =
  if params = [] then body else
  match body with
  | Lfunction {kind = Curried; params = params'; body = body'; attr; loc}
    when attr.may_fuse_arity &&
         List.length params + List.length params' <= Lambda.max_arity() ->
      lfunction ~kind:Curried ~params:(params @ params')
                ~return:Pgenval
                ~body:body'
                ~attr
                ~loc
  |  _ ->
      lfunction ~kind:Curried ~params ~return:Pgenval
                ~body
                ~attr:default_function_attribute
                ~loc:Loc_unknown

let lapply ap =
  match ap.ap_func with
    Lapply ap' ->
      Lapply {ap with ap_func = ap'.ap_func; ap_args = ap'.ap_args @ ap.ap_args}
  | _ ->
      Lapply ap

let mkappl (func, args) =
  Lapply {
    ap_loc=Loc_unknown;
    ap_func=func;
    ap_args=args;
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inline;
    ap_specialised=Default_specialise;
  }

let lsequence l1 l2 =
  if l2 = lambda_unit then l1 else Lsequence(l1, l2)

let lfield v i = Lprim(Pfield (i, Pointer, Mutable),
                       [Lvar v], Loc_unknown)

let transl_label l = share (Const_immstring l)

let transl_meth_list lst =
  if lst = [] then Lconst (const_int 0) else
  share (Const_block
            (0, List.map (fun lab -> Const_immstring lab) lst))

let set_inst_var ~scopes obj id expr =
  Lprim(Psetfield_computed (Typeopt.maybe_pointer expr, Assignment),
    [Lvar obj; Lvar id; transl_exp ~scopes expr], Loc_unknown)

let transl_val tbl create name =
  mkappl (oo_prim (if create then "new_variable" else "get_variable"),
          [Lvar tbl; transl_label name])

let transl_vals tbl create strict vals rem =
  List.fold_right
    (fun (name, id) rem ->
      Llet(strict, Pgenval, id, transl_val tbl create name, rem))
    vals rem

let meths_super tbl meths inh_meths =
  List.fold_right
    (fun (nm, id) rem ->
       try
         (nm, id,
          mkappl(oo_prim "get_method", [Lvar tbl; Lvar (Meths.find nm meths)]))
         :: rem
       with Not_found -> rem)
    inh_meths []

(*
[build_class_init] has two parameters ([cstr] and [super]) that are set when
translating the expression of a class that will be inherited in an outer class.

They could be replaced with the following type:

```
type inheritance_status =
  | Normal (** Not under an [inherit] construct *)
  | Inheriting of {
      must_narrow : bool;
      (** [false] if we already went through a call to [narrow] *)
      method_getters : (Ident.t * lambda) list;
      (** Ancestor methods are accessed through identifiers.
          These identifiers are bound at class initialisation time,
          by fetching the actual closures from the table just
          after setting up the inherited class. *)
      instance_vars : (string * Ident.t) list;
      (** Inherited instance variables need to have their index bound
          in the scope of the child class *)
    }
```

[cstr] is the negation of [must_narrow], and [super] is the pair
[(instance_vars, method_getters)].
*)

let bind_super tbl (vals, meths) cl_init =
  transl_vals tbl false StrictOpt vals
    (List.fold_right (fun (_nm, id, def) rem ->
         Llet(StrictOpt, Pgenval, id, def, rem))
       meths cl_init)

let create_object cl obj init =
  let obj' = Ident.create_local "self" in
  let (inh_init, obj_init, has_init) = init obj' in
  if obj_init = lambda_unit then
    (inh_init,
     mkappl (oo_prim (if has_init then "create_object_and_run_initializers"
                      else "create_object_opt"),
             [obj; Lvar cl]))
  else begin
   (inh_init,
    Llet(Strict, Pgenval, obj',
            mkappl (oo_prim "create_object_opt", [obj; Lvar cl]),
         Lsequence(obj_init,
                   if not has_init then Lvar obj' else
                   mkappl (oo_prim "run_initializers_opt",
                           [obj; Lvar obj'; Lvar cl]))))
  end

let name_pattern default p =
  match p.pat_desc with
  | Tpat_var (id, _, _) -> id
  | Tpat_alias(_, id, _, _) -> id
  | _ -> Ident.create_local default

(*
   [build_object_init] returns an expression that creates and initialises new
   objects. If the class takes parameters, it is a function that, given values
   for the parameters, performs the initialisations and (if needed) object
   creation.
   The [obj] expression will be bound to either the integer 0, in which case
   [obj_init] must allocate the object and return it, or to an already allocated
   object, in which case [obj_init] will initialize the relevant parts of it
   through side-effects. In the case of an immediate object it is always 0.
   Parameters:
   - [scopes] corresponds to the location scopes (as in the rest of the
     translation code)
   - [cl_table] is the variable to which the table for the current class is
     bound
   - [obj] is the parameter of the [obj_init] function we want to create.
     As explained above at runtime it might point to either an already allocated
     object, when inheriting, or a dummy zero value, when calling [new].
   - [params] stores the anonymous instance variables associated with all
     variables that occur inside the class definition but outside the
     [object ... end] structure: class parameters and class let bindings.
     The definition is always the identifier corresponding to the original
     variable.
   - [inh_init] accumulates data about the class identifiers encountered, and is
     returned at the end to be reused in [build_class_init].
   - [cl] is the class we're compiling *)

let rec build_object_init ~scopes cl_table obj params inh_init obj_init cl =
  match cl.cl_desc with
    Tcl_ident (path, _, _) ->
      (* The object initialiser for the class in [path], specialised
         to the class being defined *)
      let obj_init = Ident.create_local "obj_init" in
      let envs, inh_init = inh_init in
      let env =
        match envs with None -> []
        | Some envs ->
            [Lprim(Pfield (List.length inh_init + 1, Pointer, Mutable),
                   [Lvar envs],
                   Loc_unknown)]
      in
      let loc = of_location ~scopes cl.cl_loc in
      let path_lam = transl_class_path loc cl.cl_env path in
      (* Note: we don't need to bind [params] here, as they are
         only used in structures. Outside structures (in class lets or
         applications) we use the regular identifiers. *)
      ((envs, (path, path_lam, obj_init) :: inh_init),
       mkappl(Lvar obj_init, env @ [obj]))
  | Tcl_structure str ->
      (* Initialising a concrete class structure *)
      create_object cl_table obj (fun obj ->
        (* [obj] will be bound to the allocated object,
           unlike the original [obj] which might be zero if called directly
           from an object creation expression. *)
        let (inh_init, obj_init, has_init) =
          List.fold_right
            (fun field (inh_init, obj_init, has_init) ->
               match field.cf_desc with
                 Tcf_inherit (_, cl, _, _, _) ->
                   let (inh_init, obj_init') =
                     (* Reset [params]. The current ones will be bound
                        outside the structure. *)
                     build_object_init ~scopes cl_table (Lvar obj) [] inh_init
                       (fun _ -> lambda_unit) cl
                   in
                   (* Since [obj] is bound to a concrete object,
                      only the side-effects of [obj_init'] are relevant. *)
                   (inh_init, lsequence obj_init' obj_init, true)
               | Tcf_val (_, _, id, Tcfk_concrete (_, exp), _) ->
                   (inh_init,
                    lsequence (set_inst_var ~scopes obj id exp) obj_init,
                    has_init)
               | Tcf_method _ | Tcf_val _ | Tcf_constraint _ | Tcf_attribute _->
                   (inh_init, obj_init, has_init)
               | Tcf_initializer _ ->
                   (inh_init, obj_init, true)
            )
            str.cstr_fields
            (inh_init, obj_init obj, false)
        in
        (* Set the instance variables associated to the class parameters and
           let bindings to their expected value. *)
        (inh_init,
         List.fold_right
           (fun (id, expr) rem ->
              lsequence (Lifused (id, set_inst_var ~scopes obj id expr)) rem)
           params obj_init,
         has_init))
  | Tcl_fun (_, pat, vals, cl, partial) ->
      let (inh_init, obj_init) =
        (* [vals] maps all pattern variables to idents for use inside methods *)
        build_object_init ~scopes cl_table obj (vals @ params)
          inh_init obj_init cl
      in
      (inh_init,
       let build params rem =
         let param = name_pattern "param" pat in
         Lambda.lfunction
                   ~kind:Curried ~params:((param, Pgenval)::params)
                   ~return:Pgenval
                   ~attr:default_function_attribute
                   ~loc:(of_location ~scopes pat.pat_loc)
                   ~body:(Matching.for_function ~scopes pat.pat_loc
                             None (Lvar param) [pat, rem] partial)
       in
       begin match obj_init with
         Lfunction {kind = Curried; params; body = rem} -> build params rem
       | rem                                            -> build [] rem
       end)
  | Tcl_apply (cl, oexprs) ->
      let (inh_init, obj_init) =
        build_object_init ~scopes cl_table obj params inh_init obj_init cl
      in
      (inh_init, transl_apply ~scopes obj_init oexprs Loc_unknown)
  | Tcl_let (rec_flag, defs, vals, cl) ->
      (* See comment on the [Tcl_fun] case for the meaning of [vals] *)
      let (inh_init, obj_init) =
        build_object_init ~scopes cl_table obj (vals @ params)
          inh_init obj_init cl
      in
      (inh_init, Translcore.transl_let ~scopes rec_flag defs obj_init)
  | Tcl_open (_, cl)
    (* Class local opens are restricted to paths only, so no code is generated
     *)
  | Tcl_constraint (cl, _, _, _, _) ->
      build_object_init ~scopes cl_table obj params inh_init obj_init cl

(* The manual specifies that toplevel lets *must* be evaluated outside of the
   class. This piece of code makes sure we skip them. *)
let rec build_object_init_0
          ~scopes cl_table params cl copy_env subst_env top ids =
  match cl.cl_desc with
    Tcl_let (_rec_flag, _defs, vals, cl) ->
      build_object_init_0
        ~scopes cl_table (vals@params) cl copy_env subst_env top ids
  | Tcl_open (_descr, cl) ->
      build_object_init_0
        ~scopes cl_table params cl copy_env subst_env top ids
  | _ ->
      let self = Ident.create_local "self" in
      let env = Ident.create_local "env" in
      let obj = if ids = [] then lambda_unit else Lvar self in
      let envs = if top then None else Some env in
      let ((_,inh_init), obj_init) =
        build_object_init ~scopes cl_table obj params (envs,[]) copy_env cl in
      let obj_init =
        if ids = [] then obj_init else lfunction [self, Pgenval] obj_init in
      (inh_init, lfunction [env, Pgenval] (subst_env env inh_init obj_init))


let bind_method tbl lab id cl_init =
  Llet(Strict, Pgenval, id, mkappl (oo_prim "get_method_label",
                           [Lvar tbl; transl_label lab]),
       cl_init)

let bind_methods tbl meths vals cl_init =
  let methl = Meths.fold (fun lab id tl -> (lab,id) :: tl) meths [] in
  let len = List.length methl and nvals = List.length vals in
  if len < 2 && nvals = 0 then Meths.fold (bind_method tbl) meths cl_init else
  if len = 0 && nvals < 2 then transl_vals tbl true Strict vals cl_init else
  let ids = Ident.create_local "ids" in
  let i = ref (len + nvals) in
  let getter, names =
    if nvals = 0 then "get_method_labels", [] else
    "new_methods_variables", [transl_meth_list (List.map fst vals)]
  in
  Llet(Strict, Pgenval, ids,
       mkappl (oo_prim getter,
               [Lvar tbl; transl_meth_list (List.map fst methl)] @ names),
       List.fold_right
         (fun (_lab,id) lam -> decr i; Llet(StrictOpt, Pgenval, id,
                                           lfield ids !i, lam))
         (methl @ vals) cl_init)

let output_methods tbl methods lam =
  match methods with
    [] -> lam
  | [lab; code] ->
      lsequence (mkappl(oo_prim "set_method", [Lvar tbl; lab; code])) lam
  | _ ->
      lsequence (mkappl(oo_prim "set_methods",
                        [Lvar tbl; Lprim(Pmakeblock(0,Immutable,None),
                                         methods, Loc_unknown)]))
        lam

let rec ignore_cstrs cl =
  match cl.cl_desc with
    Tcl_constraint (cl, _, _, _, _) -> ignore_cstrs cl
  | Tcl_apply (cl, _) -> ignore_cstrs cl
  | _ -> cl

let rec index a = function
    [] -> raise Not_found
  | b :: l ->
      if b = a then 0 else 1 + index a l

let bind_id_as_val (id, _) = ("", id)

(** Build the class initialisation code.
    Parameters:
    - [scopes] corresponds to the location scopes (as in the rest of the
      translation code)
    - [cla] is the variable to which the table for the current class is bound
    - [cstr] is [true] when called from outside, but [false] when called
      from an [inherit] field. Narrowing is necessary during inheritance to
      prevent clashes between methods/variables in the child class and private
      methods/variables in the parent.
    - [super] stores, if we're building an inherited class, the variables and
      methods exposed to the child. The variables need to have their associated
      index exposed, and methods have to be bound in case the child refers to
      them through the ancestor variables.
    - [inh_init] is the sequence of inheritance paths computed during
      [build_object_init].
    - [cl_init] is the expression we're building.
    - [msubst] replaces methods with builtin methods when possible.
    - [top] is [false] if the current class is under [Translobj.oo_wrap].
    - [cl] is the class we're compiling *)
let rec build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl =
  match cl.cl_desc with
  | Tcl_ident _ ->
      begin match inh_init with
      | (_, path_lam, obj_init)::inh_init ->
          (inh_init,
           Llet (Strict, Pgenval, obj_init,
              (* Load the [class_init] field of the class,
                 and apply it to our current table and the class' environment.
                 This gets us the object initialiser. *)
                 mkappl(Lprim(Pfield (1, Pointer, Mutable),
                              [path_lam], Loc_unknown), Lvar cla ::
                        if top then [Lprim(Pfield (2, Pointer, Mutable),
                                     [path_lam], Loc_unknown)]
                        else []),
              (* The methods and variables for this class are fully registered
                 in the table. If we are in an inheritance context, we can now
                 bind everything. *)
                 bind_super cla super cl_init))
      | _ ->
          assert false
      end
  | Tcl_structure str ->
      let cl_init = bind_super cla super cl_init in
      let (inh_init, cl_init, methods, values) =
        List.fold_right
          (fun field (inh_init, cl_init, methods, values) ->
            match field.cf_desc with
              Tcf_inherit (_, cl, _, vals, meths) ->
                let cl_init = output_methods cla methods cl_init in
                let inh_init, cl_init =
                 (* Build the initialisation code for the inherited class,
                    plus its wrappers.
                    Make sure the wrappers bind the inherited methods
                    and variables. *)
                  build_class_init ~scopes cla false
                    (vals, meths_super cla str.cstr_meths meths)
                    inh_init cl_init msubst top cl in
                (inh_init, cl_init, [], values)
            | Tcf_val (name, _, id, _, over) ->
                (* If this is an override, the variable is the same as
                   the one from the earlier definition, and must not be
                   bound again. *)
                let values =
                  if over then values else (name.txt, id) :: values
                in
                (inh_init, cl_init, methods, values)
            | Tcf_method (_, _, Tcfk_virtual _)
            | Tcf_constraint _
              ->
                (inh_init, cl_init, methods, values)
            | Tcf_method (name, _, Tcfk_concrete (_, exp)) ->
                let scopes = enter_method_definition ~scopes name.txt in
                let met_code =
                  msubst true (transl_scoped_exp ~scopes exp) in
                let met_code =
                  if !Clflags.native_code && List.length met_code = 1 then
                    (* Force correct naming of method for profiles *)
                    let met = Ident.create_local ("method_" ^ name.txt) in
                    [Llet(Strict, Pgenval, met, List.hd met_code, Lvar met)]
                  else met_code
                in
                (inh_init, cl_init,
                 Lvar(Meths.find name.txt str.cstr_meths) :: met_code @ methods,
                 values)
            | Tcf_initializer exp ->
                (inh_init,
                 Lsequence(mkappl (oo_prim "add_initializer",
                                   Lvar cla :: msubst false
                                                 (transl_exp ~scopes exp)),
                           cl_init),
                 methods, values)
            | Tcf_attribute _ ->
                (inh_init, cl_init, methods, values))
          str.cstr_fields
          (inh_init, cl_init, [], [])
      in
        (* In order of execution at runtime:
           - Bind the method and variable indices for the current class
             ([bind_methods])
           - Run the code for setting up the individual fields ([cl_init], plus
             [output_methods] for the remaining unset methods)
           - If we are in an inheritance context, bind the inherited variables
             and methods for use in the child ([bind_super] at the top of this
             branch) *)
      let cl_init = output_methods cla methods cl_init in
      (inh_init, bind_methods cla str.cstr_meths values cl_init)
  | Tcl_fun (_, _pat, vals, cl, _) ->
      let (inh_init, cl_init) =
        build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl
      in
      (* Create anonymous instance variables and define them in the table *)
      let vals = List.map bind_id_as_val vals in
      (inh_init, transl_vals cla true StrictOpt vals cl_init)
  | Tcl_apply (cl, _exprs) ->
      build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl
  | Tcl_let (_rec_flag, _defs, vals, cl) ->
      let (inh_init, cl_init) =
        build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl
      in
      (* Create anonymous instance variables and define them in the table *)
      let vals = List.map bind_id_as_val vals in
      (inh_init, transl_vals cla true StrictOpt vals cl_init)
  | Tcl_constraint (cl, _, vals, meths, concr_meths) ->
      let virt_meths =
        List.filter (fun lab -> not (MethSet.mem lab concr_meths)) meths in
      let concr_meths = MethSet.elements concr_meths in
      let narrow_args =
        [Lvar cla;
         transl_meth_list vals;
         transl_meth_list virt_meths;
         transl_meth_list concr_meths] in
      let cl = ignore_cstrs cl in
      begin match cl.cl_desc, inh_init with
      | Tcl_ident (path, _, _), (path', path_lam, obj_init)::inh_init ->
          assert (Path.same path path');
          let inh = Ident.create_local "inh"
          and ofs = List.length vals + 1
          and valids, methids = super in
          let cl_init =
            List.fold_left
              (fun init (nm, id, _) ->
                Llet(StrictOpt, Pgenval, id,
                     lfield inh (index nm concr_meths + ofs),
                     init))
              cl_init methids in
          let cl_init =
            List.fold_left
              (fun init (nm, id) ->
                Llet(StrictOpt, Pgenval, id,
                     lfield inh (index nm vals + 1), init))
              cl_init valids in
          (inh_init,
           Llet (Strict, Pgenval, inh,
                 mkappl(oo_prim "inherits", narrow_args @
                        [path_lam;
                         Lconst(const_int (if top then 1 else 0))]),
                 Llet(StrictOpt, Pgenval, obj_init, lfield inh 0, cl_init)))
      | _ ->
          let core cl_init =
            build_class_init
              ~scopes cla true super inh_init cl_init msubst top cl
          in
          (* Skip narrowing if we're not directly under [inherit] *)
          if cstr then core cl_init else
          let (inh_init, cl_init) =
            core (Lsequence (mkappl (oo_prim "widen", [Lvar cla]), cl_init))
          in
          (inh_init,
           Lsequence(mkappl (oo_prim "narrow", narrow_args),
                     cl_init))
      end
  | Tcl_open (_, cl) ->
      build_class_init ~scopes cla cstr super inh_init cl_init msubst top cl

let rec build_class_lets ~scopes cl =
  match cl.cl_desc with
    Tcl_let (rec_flag, defs, _vals, cl') ->
      let env, wrap = build_class_lets ~scopes cl' in
      (env, fun lam_and_kind ->
          let lam, rkind = wrap lam_and_kind in
          Translcore.transl_let ~scopes rec_flag defs lam, rkind)
  | Tcl_open (open_descr, cl) ->
      (* Failsafe to ensure we get a compilation error if arbitrary
         module expressions become allowed *)
      let _ : Path.t * Longident.t loc = open_descr.open_expr in
      build_class_lets ~scopes cl
  | _ ->
      (cl.cl_env, fun lam_and_kind -> lam_and_kind)

let rec get_class_meths cl =
  match cl.cl_desc with
    Tcl_structure cl ->
      Meths.fold (fun _ -> Ident.Set.add) cl.cstr_meths Ident.Set.empty
  | Tcl_ident _ -> Ident.Set.empty
  | Tcl_fun (_, _, _, cl, _)
  | Tcl_let (_, _, _, cl)
  | Tcl_apply (cl, _)
  | Tcl_open (_, cl)
  | Tcl_constraint (cl, _, _, _, _) -> get_class_meths cl

(*
   XXX Il devrait etre peu couteux d'ecrire des classes :
   |   Writing classes should be cheap
     class c x y = d e f
*)
let rec transl_class_rebind ~scopes obj_init cl vf =
  match cl.cl_desc with
    Tcl_ident (path, _, _) ->
      if vf = Concrete then begin
        try if (Env.find_class path cl.cl_env).cty_new = None then raise Exit
        with Not_found -> raise Exit
      end;
      let cl_loc = of_location ~scopes cl.cl_loc in
      let path_lam = transl_class_path cl_loc cl.cl_env path in
      (path, path_lam, obj_init)
  | Tcl_fun (_, pat, _, cl, partial) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      let build params rem =
        let param = name_pattern "param" pat in
        Lambda.lfunction
                  ~kind:Curried ~params:((param, Pgenval)::params)
                  ~return:Pgenval
                  ~attr:default_function_attribute
                  ~loc:(of_location ~scopes pat.pat_loc)
                  ~body:(Matching.for_function ~scopes pat.pat_loc
                            None (Lvar param) [pat, rem] partial)
      in
      (path, path_lam,
       match obj_init with
         Lfunction {kind = Curried; params; body} -> build params body
       | rem                                      -> build [] rem)
  | Tcl_apply (cl, oexprs) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      (path, path_lam, transl_apply ~scopes obj_init oexprs Loc_unknown)
  | Tcl_let (rec_flag, defs, _vals, cl) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      (path, path_lam, Translcore.transl_let ~scopes rec_flag defs obj_init)
  | Tcl_structure _ -> raise Exit
  | Tcl_constraint (cl', _, _, _, _) ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl' vf in
      let rec check_constraint = function
          Cty_constr(path', _, _) when Path.same path path' -> ()
        | Cty_arrow (_, _, cty) -> check_constraint cty
        | _ -> raise Exit
      in
      check_constraint cl.cl_type;
      (path, path_lam, obj_init)
  | Tcl_open (_, cl) ->
      transl_class_rebind ~scopes obj_init cl vf

let rec transl_class_rebind_0 ~scopes (self:Ident.t) obj_init cl vf =
  match cl.cl_desc with
    Tcl_let (rec_flag, defs, _vals, cl) ->
      let path, path_lam, obj_init =
        transl_class_rebind_0 ~scopes self obj_init cl vf
      in
      (path, path_lam, Translcore.transl_let ~scopes rec_flag defs obj_init)
  | _ ->
      let path, path_lam, obj_init =
        transl_class_rebind ~scopes obj_init cl vf in
      (path, path_lam, lfunction [self, Pgenval] obj_init)

let transl_class_rebind ~scopes cl vf =
  try
    let obj_init = Ident.create_local "obj_init"
    and self = Ident.create_local "self" in
    let obj_init0 =
      lapply {
        ap_loc=Loc_unknown;
        ap_func=Lvar obj_init;
        ap_args=[Lvar self];
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inline;
        ap_specialised=Default_specialise;
      }
    in
    let _, path_lam, obj_init' =
      transl_class_rebind_0 ~scopes self obj_init0 cl vf in
    let id = (obj_init' = lfunction [self, Pgenval] obj_init0) in
    if id then path_lam else

    let cla = Ident.create_local "class"
    and new_init = Ident.create_local "new_init"
    and env_init = Ident.create_local "env_init"
    and table = Ident.create_local "table"
    and envs = Ident.create_local "envs" in
    Llet(
    Strict, Pgenval, new_init, lfunction [obj_init, Pgenval] obj_init',
    Llet(
    Alias, Pgenval, cla, path_lam,
    Lprim(Pmakeblock(0, Immutable, None),
          [mkappl(Lvar new_init, [lfield cla 0]);
           lfunction [table, Pgenval]
             (Llet(Strict, Pgenval, env_init,
                   mkappl(lfield cla 1, [Lvar table]),
                   lfunction [envs, Pgenval]
                     (mkappl(Lvar new_init,
                             [mkappl(Lvar env_init, [Lvar envs])]))));
           lfield cla 2],
          Loc_unknown)))
  with Exit ->
    lambda_unit

(* Rewrite a closure using builtins. Improves native code size. *)

let const_path local = function
    Lvar id -> not (List.mem id local)
  | Lconst _ -> true
  | Lfunction {kind = Curried; body} ->
      let fv = free_variables body in
      List.for_all (fun x -> not (Ident.Set.mem x fv)) local
  | _ -> false

let rec builtin_meths self env env2 body =
  let const_path = const_path (env::self) in
  let conv = function
    (* Lvar s when List.mem s self ->  "_self", [] *)
    | p when const_path p -> "const", [p]
    | Lprim(Parrayrefu _, [Lvar s; Lvar n], _) when List.mem s self ->
        "var", [Lvar n]
    | Lprim(Pfield(n, _, _), [Lvar e], _) when Ident.same e env ->
        "env", [Lvar env2; Lconst(const_int n)]
    | Lsend(Self, met, Lvar s, [], _) when List.mem s self ->
        "meth", [met]
    | _ -> raise Not_found
  in
  match body with
  | Llet(_str, _k, s', Lvar s, body) when List.mem s self ->
      builtin_meths (s'::self) env env2 body
  | Lapply{ap_func = f; ap_args = [arg]} when const_path f ->
      let s, args = conv arg in ("app_"^s, f :: args)
  | Lapply{ap_func = f; ap_args = [arg; p]} when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_"^s^"_const", f :: args @ [p])
  | Lapply{ap_func = f; ap_args = [p; arg]} when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_const_"^s, f :: p :: args)
  | Lsend(Self, Lvar n, Lvar s, [arg], _) when List.mem s self ->
      let s, args = conv arg in
      ("meth_app_"^s, Lvar n :: args)
  | Lsend(Self, met, Lvar s, [], _) when List.mem s self ->
      ("get_meth", [met])
  | Lsend(Public, met, arg, [], _) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lsend(Cached, met, arg, [_;_], _) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lfunction {kind = Curried; params = [x, _]; body} ->
      let rec enter self = function
        | Lprim(Parraysetu _, [Lvar s; Lvar n; Lvar x'], _)
          when Ident.same x x' && List.mem s self ->
            ("set_var", [Lvar n])
        | Llet(_str, _k, s', Lvar s, body) when List.mem s self ->
            enter (s'::self) body
        | _ -> raise Not_found
      in enter self body
  | Lfunction _ -> raise Not_found
  | _ ->
      let s, args = conv body in ("get_"^s, args)

module M = struct
  open CamlinternalOO
  let builtin_meths self env env2 body =
    let builtin, args = builtin_meths self env env2 body in
    (* if not arr then [mkappl(oo_prim builtin, args)] else *)
    let tag = match builtin with
      "get_const" -> GetConst
    | "get_var"   -> GetVar
    | "get_env"   -> GetEnv
    | "get_meth"  -> GetMeth
    | "set_var"   -> SetVar
    | "app_const" -> AppConst
    | "app_var"   -> AppVar
    | "app_env"   -> AppEnv
    | "app_meth"  -> AppMeth
    | "app_const_const" -> AppConstConst
    | "app_const_var"   -> AppConstVar
    | "app_const_env"   -> AppConstEnv
    | "app_const_meth"  -> AppConstMeth
    | "app_var_const"   -> AppVarConst
    | "app_env_const"   -> AppEnvConst
    | "app_meth_const"  -> AppMethConst
    | "meth_app_const"  -> MethAppConst
    | "meth_app_var"    -> MethAppVar
    | "meth_app_env"    -> MethAppEnv
    | "meth_app_meth"   -> MethAppMeth
    | "send_const" -> SendConst
    | "send_var"   -> SendVar
    | "send_env"   -> SendEnv
    | "send_meth"  -> SendMeth
    | _ -> assert false
    in Lconst(const_int (Obj.magic tag)) :: args
end
open M


(*
   Class translation.
   Three subcases:
    * reapplication of a known class -> transl_class_rebind
    * class without local dependencies -> direct translation
    * with local dependencies -> generate a stubs tree,
      with a node for every local classes inherited
   A class is a 3-tuple:
    (obj_init, class_init, env)
    obj_init: creation function (unit -> params -> obj)
    class_init: inheritance function (table -> env -> obj_init)
      (one by source code)
    env: local environment

   The local environment is used for cached classes. When a
   class definition occurs under a call to Translobj.oo_wrap
   (typically inside a functor), the class creation code is
   split between a static part (depending only on toplevel names)
   and a dynamic part, the environment. The static part is cached
   in a toplevel structure, so that only the first class creation
   computes it and the subsequent classes can reuse it.
   Because of that, the (static) [class_init] function takes both
   the class table to be filled and the environment as parameters,
   and when called is given the [env] field of the class.
   For the [obj_init] part, an [env_init] function (of type [env -> obj_init])
   is stored in the cache, and called on the environment to generate
   the [obj_init] at class creation time.
*)

(*
let prerr_ids msg ids =
  let names = List.map Ident.unique_toplevel_name ids in
  prerr_endline (String.concat " " (msg :: names))
*)

let free_methods l =
  let fv = ref Ident.Set.empty in
  let rec free l =
    Lambda.iter_head_constructor free l;
    match l with
    | Lsend(Self, Lvar meth, _, _, _) ->
        fv := Ident.Set.add meth !fv
    | Lsend _ -> ()
    | Lfunction{params} ->
        List.iter (fun (param, _) -> fv := Ident.Set.remove param !fv) params
    | Llet(_, _k, id, _arg, _body)
    | Lmutlet(_k, id, _arg, _body) ->
        fv := Ident.Set.remove id !fv
    | Lletrec(decl, _body) ->
        List.iter (fun { id } -> fv := Ident.Set.remove id !fv) decl
    | Lstaticcatch(_e1, (_,vars), _e2) ->
        List.iter (fun (id, _) -> fv := Ident.Set.remove id !fv) vars
    | Ltrywith(_e1, exn, _e2) ->
        fv := Ident.Set.remove exn !fv
    | Lfor(v, _e1, _e2, _dir, _e3) ->
        fv := Ident.Set.remove v !fv
    | Lassign _
    | Lvar _ | Lmutvar _ | Lconst _ | Lapply _
    | Lprim _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
    | Lifthenelse _ | Lsequence _ | Lwhile _
    | Levent _ | Lifused _ -> ()
  in free l; !fv

let transl_class ~scopes ids cl_id pub_meths cl vflag =
  let open Value_rec_types in
  (* First check if it is not only a rebind *)
  let rebind = transl_class_rebind ~scopes cl vflag in
  if rebind <> lambda_unit then rebind, Dynamic else

  (* Prepare for heavy environment handling *)
  let scopes = enter_class_definition ~scopes cl_id in
  let tables = Ident.create_local (Ident.name cl_id ^ "_tables") in
  let (top_env, req) = oo_add_class tables in
  let top = not req in
  (* The manual specifies that toplevel lets *must* be evaluated outside of the
     class *)
  let cl_env, llets = build_class_lets ~scopes cl in
  let new_ids = if top then [] else Env.diff top_env cl_env in
  let env2 = Ident.create_local "env" in
  let meth_ids = get_class_meths cl in
  let subst env lam i0 new_ids' =
    let fv = free_variables lam in
    (* prerr_ids "cl_id =" [cl_id]; prerr_ids "fv =" (Ident.Set.elements fv); *)
    let fv = List.fold_right Ident.Set.remove !new_ids' fv in
    (* We need to handle method ids specially, as they do not appear
       in the typing environment (PR#3576, PR#4560) *)
    (* very hacky: we add and remove free method ids on the fly,
       depending on the visit order... *)
    method_ids :=
      Ident.Set.diff (Ident.Set.union (free_methods lam) !method_ids) meth_ids;
    (* prerr_ids "meth_ids =" (Ident.Set.elements meth_ids);
       prerr_ids "method_ids =" (Ident.Set.elements !method_ids); *)
    let new_ids = List.fold_right Ident.Set.add new_ids !method_ids in
    let fv = Ident.Set.inter fv new_ids in
    new_ids' := !new_ids' @ Ident.Set.elements fv;
    (* prerr_ids "new_ids' =" !new_ids'; *)
    let i = ref (i0-1) in
    List.fold_left
      (fun subst id ->
        incr i; Ident.Map.add id (lfield env !i)  subst)
      Ident.Map.empty !new_ids'
  in
  let new_ids_meths = ref [] in
  let no_env_update _ _ env = env in
  let msubst arr = function
      Lfunction {kind = Curried; params = (self, Pgenval) :: args; body} ->
        let env = Ident.create_local "env" in
        let body' =
          if new_ids = [] then body else
          Lambda.subst no_env_update (subst env body 0 new_ids_meths) body in
        begin try
          (* Doesn't seem to improve size for bytecode *)
          (* if not !Clflags.native_code then raise Not_found; *)
          if not arr || !Clflags.debug then raise Not_found;
          builtin_meths [self] env env2 (lfunction args body')
        with Not_found ->
          [lfunction ((self, Pgenval) :: args)
             (if not (Ident.Set.mem env (free_variables body')) then body' else
              Llet(Alias, Pgenval, env,
                   Lprim(Pfield_computed,
                         [Lvar self; Lvar env2],
                         Loc_unknown),
                   body'))]
        end
      | _ -> assert false
  in
  let new_ids_init = ref [] in
  let env1 = Ident.create_local "env" and env1' = Ident.create_local "env'" in
  let copy_env self =
    if top then lambda_unit else
    Lifused(env2, Lprim(Psetfield_computed (Pointer, Assignment),
                        [Lvar self; Lvar env2; Lvar env1'],
                        Loc_unknown))
  and subst_env envs l lam =
    if top then lam else
    (* must be called only once! *)
    let lam = Lambda.subst no_env_update (subst env1 lam 1 new_ids_init) lam in
    Llet(Alias, Pgenval, env1, (if l = [] then Lvar envs else lfield envs 0),
    Llet(Alias, Pgenval, env1',
         (if !new_ids_init = [] then Lvar env1 else lfield env1 0),
         lam))
  in

  (* Now we start compiling the class *)
  let cla = Ident.create_local "class" in
  let (inh_init, obj_init) =
    build_object_init_0 ~scopes cla [] cl copy_env subst_env top ids in
  let inh_init' = List.rev inh_init in
  let (inh_init', cl_init) =
    build_class_init ~scopes cla true ([],[]) inh_init' obj_init msubst top cl
  in
  assert (inh_init' = []);
  let table = Ident.create_local "table"
  and class_init = Ident.create_local (Ident.name cl_id ^ "_init")
  and env_init = Ident.create_local "env_init"
  and obj_init = Ident.create_local "obj_init" in
  (* Sort methods by hash *)
  let pub_meths =
    List.sort
      (fun s s' -> compare (Btype.hash_variant s) (Btype.hash_variant s'))
      pub_meths in
  (* Check for hash conflicts *)
  let tags = List.map Btype.hash_variant pub_meths in
  let rev_map = List.combine tags pub_meths in
  List.iter2
    (fun tag name ->
      let name' = List.assoc tag rev_map in
      if name' <> name then raise(Error(cl.cl_loc, Tags(name, name'))))
    tags pub_meths;
  let ltable table lam =
    Llet(Strict, Pgenval, table,
         mkappl (oo_prim "create_table", [transl_meth_list pub_meths]), lam)
  and ldirect obj_init =
    Llet(Strict, Pgenval, obj_init, cl_init,
         Lsequence(mkappl (oo_prim "init_class", [Lvar cla]),
                   mkappl (Lvar obj_init, [lambda_unit])))
  in
  (* Simplest case: an object defined at toplevel (ids=[]) *)
  if top && ids = [] then llets (ltable cla (ldirect obj_init), Dynamic) else

  let concrete = (vflag = Concrete)
  and lclass mk_lam_and_kind =
    let cl_init, _ =
      llets (Lambda.lfunction
               ~kind:Curried
               ~attr:default_function_attribute
               ~loc:Loc_unknown
               ~return:Pgenval
               ~params:[cla, Pgenval]
               ~body:cl_init,
            Dynamic (* Placeholder, real kind is computed in [lbody] below *))
    in
    let lam, rkind = mk_lam_and_kind (free_variables cl_init) in
    Llet(Strict, Pgenval, class_init, cl_init, lam), rkind
  and lbody fv =
    if List.for_all (fun id -> not (Ident.Set.mem id fv)) ids then
      (* Not recursive: can use make_class directly *)
      mkappl (oo_prim "make_class",[transl_meth_list pub_meths;
                                    Lvar class_init]),
      Dynamic
    else
      (* Recursive: need to have an actual allocation for let rec compilation
         to work, so hardcode make_class *)
      ltable table (
      Llet(
      Strict, Pgenval, env_init, mkappl (Lvar class_init, [Lvar table]),
      Lsequence(
      mkappl (oo_prim "init_class", [Lvar table]),
      Lprim(Pmakeblock(0, Immutable, None),
            [mkappl (Lvar env_init, [lambda_unit]);
             Lvar class_init; lambda_unit],
            Loc_unknown)))),
      Static
  and lbody_virt lenvs =
    (* Virtual classes only need to provide the [class_init] and [env]
       fields. [obj_init] is filled with a dummy [lambda_unit] value. *)
    Lprim(Pmakeblock(0, Immutable, None),
          [lambda_unit; Lambda.lfunction
                          ~kind:Curried
                          ~attr:default_function_attribute
                          ~loc:Loc_unknown
                          ~return:Pgenval
                          ~params:[cla, Pgenval] ~body:cl_init;
           lenvs],
         Loc_unknown),
    Static
  in
  (* Still easy: a class defined at toplevel *)
  if top && concrete then lclass lbody else
  if top then llets (lbody_virt lambda_unit) else

  (* Now for the hard stuff: prepare for table caching *)
  let envs = Ident.create_local "envs"
  and cached = Ident.create_local "cached" in
  let lenvs =
    if !new_ids_meths = [] && !new_ids_init = [] && inh_init = []
    then lambda_unit
    else Lvar envs in
  let lenv =
    let menv =
      if !new_ids_meths = [] then lambda_unit else
      Lprim(Pmakeblock(0, Immutable, None),
            List.map (fun id -> Lvar id) !new_ids_meths,
            Loc_unknown) in
    if !new_ids_init = [] then menv else
    Lprim(Pmakeblock(0, Immutable, None),
          menv :: List.map (fun id -> Lvar id) !new_ids_init,
          Loc_unknown)
  and linh_envs =
    List.map
      (fun (_, path_lam, _) ->
        Lprim(Pfield (2, Pointer, Mutable), [path_lam], Loc_unknown))
      (List.rev inh_init)
  in
  let make_envs (lam, rkind) =
    Llet(StrictOpt, Pgenval, envs,
         (if linh_envs = [] then lenv else
         Lprim(Pmakeblock(0, Immutable, None),
               lenv :: linh_envs, Loc_unknown)),
         lam),
    rkind
  and def_ids cla lam =
    Llet(StrictOpt, Pgenval, env2,
         mkappl (oo_prim "new_variable", [Lvar cla; transl_label ""]),
         lam)
  in
  let inh_paths =
    List.filter
      (fun (path, _, _) -> List.mem (Path.head path) new_ids) inh_init
  in
  let inh_keys =
    List.map
      (fun (_, path_lam, _) ->
        Lprim(Pfield (1, Pointer, Mutable), [path_lam], Loc_unknown))
      inh_paths
  in
  let lclass lam =
    Llet(Strict, Pgenval, class_init,
         Lambda.lfunction
                   ~kind:Curried ~params:[cla, Pgenval]
                   ~return:Pgenval
                   ~attr:default_function_attribute
                   ~loc:Loc_unknown
                   ~body:(def_ids cla cl_init), lam)
  and lset cached i lam =
    Lprim(Psetfield(i, Pointer, Assignment),
          [Lvar cached; lam], Loc_unknown)
  in
  let ldirect () =
    ltable cla
      (Llet(Strict, Pgenval, env_init, def_ids cla cl_init,
            Lsequence(mkappl (oo_prim "init_class", [Lvar cla]),
                      lset cached 0 (Lvar env_init))))
  and lclass_virt () =
    lset cached 0
      (Lambda.lfunction
         ~kind:Curried
         ~attr:default_function_attribute
         ~loc:Loc_unknown
         ~return:Pgenval
         ~params:[cla, Pgenval]
         ~body:(def_ids cla cl_init))
  in
  let lupdate_cache =
    if ids = [] then ldirect () else
      if not concrete then lclass_virt () else
        lclass (
            mkappl (oo_prim "make_class_store",
                    [transl_meth_list pub_meths;
                     Lvar class_init; Lvar cached])) in
  let lcheck_cache =
    if !Clflags.native_code && !Clflags.afl_instrument then
      (* When afl-fuzz instrumentation is enabled, ignore the cache
         so that the program's behaviour does not change between runs *)
      lupdate_cache
    else
      Lifthenelse(lfield cached 0, lambda_unit, lupdate_cache) in
  let lcache (lam, rkind) =
    let lam = Lsequence (lcheck_cache, lam) in
    let lam =
      if inh_keys = []
      then Llet(Alias, Pgenval, cached, Lvar tables, lam)
      else
        Llet(Strict, Pgenval, cached,
             mkappl (oo_prim "lookup_tables",
                     [Lvar tables; Lprim(Pmakeblock(0, Immutable, None),
                                         inh_keys, Loc_unknown)]),
             lam)
    in
    lam, rkind
  in
  llets (
  lcache (
  make_envs (
  if ids = []
  then mkappl (lfield cached 0, [lenvs]), Dynamic
  else
    Lprim(Pmakeblock(0, Immutable, None),
        (if concrete then
          [mkappl (lfield cached 0, [lenvs]);
           lfield cached 1;
           lenvs]
        else [lambda_unit; lfield cached 0; lenvs]),
        Loc_unknown
       ),
    Static)))

(* Wrapper for class compilation *)
(*
    let cl_id = ci.ci_id_class in
(* TODO: cl_id is used somewhere else as typesharp ? *)
  let _arity = List.length ci.ci_params in
  let pub_meths = m in
  let cl = ci.ci_expr in
  let vflag = vf in
*)

let transl_class ~scopes ids id pub_meths cl vf =
  oo_wrap_gen cl.cl_env false (transl_class ~scopes ids id pub_meths cl) vf

let () =
  transl_object := (fun ~scopes id meths cl ->
    let lam, _rkind = transl_class ~scopes [] id meths cl Concrete in
    lam)

(* Error report *)

open Format_doc
module Style = Misc.Style

let report_error_doc ppf = function
  | Tags (lab1, lab2) ->
      fprintf ppf "Method labels %a and %a are incompatible.@ %s"
        Style.inline_code lab1
        Style.inline_code lab2
        "Change one of them."

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error_doc err)
      | _ ->
        None
    )

let report_error = Format_doc.compat report_error_doc
