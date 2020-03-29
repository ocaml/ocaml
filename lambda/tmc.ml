open Lambda

(** TMC (Tail Modulo Cons) is a code transformation that
    rewrites transformed functions in destination-passing-style, in
    such a way that certain calls that were not in tail position in the
    original program become tail-calls in the transformed program.

    As a classic example, the following program
    {|
     let[@tail_mod_cons] rec map f = function
     | [] -> []
     | x :: xs ->
       let y = f x in
       y :: map f xs
    |}
    becomes (expressed in almost-source-form; the translation is in
    fact at the Lambda-level)
    {|
     let rec map f = function
     | [] -> []
     | x :: xs ->
       let y = f x in
       let dst = y :: Placeholder in
       map_dps dst 1 f xs; dst
     and map_dps dst offset f = function
     | [] ->
       dst.offset <- []
     | x :: xs ->
       let y = f x in
       let dst' = y :: Placeholder in
       dst.offset <- dst';
       map_dps dst 1 f fx
    |}

    In this example, the expression (y :: map f xs) had a call in
    non-tail-position, and it gets rewritten into tail-calls. TMC
    handles all such cases where the continuation of the call
    (what needs to be done after the return) is a "construction", the
    creation of a (possibly nested) data block.

    The code transformation generates two versions of the
    input function, the "direct" version with the same type and
    behavior as the original one (here just [map]), and
    the "destination-passing-style" version (here [map_dps]).

    Any call to the original function from outside the let..rec
    declaration gets transformed into a call into the direct version,
    which will itself call the destination-passing-style versions on
    recursive calls that may benefit from it (they are in tail-position
    modulo constructors).

    Because of this inherent code duplication, the transformation may
    not always improve performance. In this implementation, TMC is
    opt-in, we only transform functions that the user has annotated
    with an attribute to request the transformation.
*)

type 'offset destination = {
  var: Ident.t;
  offset: 'offset;
  loc : Debuginfo.Scoped_location.t;
}
and offset = lambda
(** In the OCaml value model, interior pointers are not allowed.  To
    represent the "placeholder to mutate" in DPS code, we thus use a pair
    of the block containing the placeholder, and the offset of the
    placeholder within the block.

    In the common case, this offset is an arbitrary lambda expression, typically
    a constant integer or a variable. We define ['a destination] as parametrized
    over the offset type to represent formal destination parameters (where
    the offset is an Ident.t), and maybe in the future statically-known
    offsets (where the offset is an integer).
*)

let add_dst_params ({var; offset} : Ident.t destination) params =
  (var, Pgenval) :: (offset, Pintval) :: params

let add_dst_args ({var; offset} : offset destination) args =
  Lvar var :: offset :: args

let assign_to_dst {var; offset; loc} lam =
  Lprim(Psetfield_computed(Pointer, Heap_initialization),
        [Lvar var; offset; lam], loc)

(** The type ['a Dps.t] (destination-passing-style) represents a
    version of ['a] that is parametrized over a [lambda destination]. *)
module Dps = struct
  type 'a t =
    tail:bool -> dst:lambda destination -> 'a
  (** A term parameterized over a destination.  The [tail] argument
      is passed by the caller to indicate whether the term will be placed
      in tail-position -- this allows to generate correct @tailcall
      annotations. *)

  (** Create a new destination-passing-style term which is simply
      setting the destination with the given [v], hence "returning"
      it. *)
  let return (v : lambda): lambda t = fun ~tail:_ ~dst ->
    assign_to_dst dst v

  let map (f : 'a -> 'b) (dps : 'a t) : 'b t = fun ~tail ~dst ->
    f @@ dps ~tail ~dst

  let pair (fa : 'a t) (fb : 'b t) : ('a * 'b) t = fun ~tail ~dst ->
    (fa ~tail ~dst, fb ~tail ~dst)

  let unit : unit t = fun ~tail:_ ~dst:_ ->
    ()
end

(** The TMC transformation requires information flows in two opposite
    directions: the information of which callsites can be rewritten in
    destination-passing-style flows from the leaves of the code to the
    root, and the information on whether we remain in tail-position
    flows from the root to the leaves -- and also the knowledge of
    which version of the function we currently want to generate, the
    direct version or a destination-passing-style version.

    To clarify this double flow of information, we split the TMC
    transform in two steps:

    1. A function [choice t] that takes a term and processes it from
    leaves to root; it produces a "code choice", a piece of data of
    type [Choice.t], that contains information on how to transform the
    input term [t] *parameterized* over the (still missing) contextual
    information.

    2. Code-production operators that have contextual information
    to transform a "code choice" into the final code.

    The code-production choices for a single term have type [lambda Choice.t];
    using a parametrized type ['a Choice.t] is useful to represent
    simultaneous choices over several subterms; for example
    [(lambda * lambda) Choice.t] makes a choice for a pair of terms,
    for example the [then] and [else] cases of a conditional. With
    this parameter, ['a Choice.t] has an applicative structure, which
    is useful to write the actual code transformation in the {!choice}
    function.
*)
module Choice = struct
  type 'a t = {
    dps : 'a Dps.t;
    direct : unit -> 'a;
    has_tmc_calls : bool;
  }
  (**
     An ['a Choice.t] represents code that may be written
     in destination-passing style if its usage context allows it.
     More precisely:

     - If the surrounding context is already in destination-passing
       style, it has a destination available, we should produce the
       code in [dps] -- a function parametrized over the destination.

     - If the surrounding context is in direct style (no destination
       is available), we should produce the fallback code from
       [direct].

       (Note: [direct] is also a function (on [unit]) to ensure that any
       effects performed during code production will only happen once we
       do know that we want to produce the direct-style code.)

     - [has_tmc_calls] is true when there are TMC opportunities
       in the subterm -- if some calls are in tail-modulo-cons
       position and are rewritten into tailcalls in the [dps] version.
   *)

  let return (v : lambda) : lambda t = {
    dps = Dps.return v;
    direct = (fun () -> v);
    has_tmc_calls = false;
  }

  let map f s = {
    dps = Dps.map f s.dps;
    direct = (fun () -> f (s.direct ()));
    has_tmc_calls = s.has_tmc_calls;
  }
  (** Apply function [f] to the transformed term. *)

  let direct (c : lambda t) : lambda =
    c.direct ()

  let dps (c : lambda t) ~tail ~dst =
    c.dps ~tail:tail ~dst:dst

  let pair ((c1, c2) : 'a t * 'b t) : ('a * 'b) t = {
    dps = Dps.pair c1.dps c2.dps;
    direct = (fun () -> (c1.direct (), c2.direct ()));
    has_tmc_calls =
      c1.has_tmc_calls || c2.has_tmc_calls;
  }

  let unit = {
    dps = Dps.unit;
    direct = (fun () -> ());
    has_tmc_calls = false;
  }

  module Syntax = struct
    let (let+) a f = map f a
    let (and+) a1 a2 = pair (a1, a2)
  end
  open Syntax

  let option (c : 'a t option) : 'a option t =
    match c with
    | None -> let+ () = unit in None
    | Some c -> let+ v = c in Some v

  let rec list (c : 'a t list) : 'a list t =
    match c with
    | [] -> let+ () = unit in []
    | c :: cs ->
        let+ v = c
        and+ vs = list cs
        in v :: vs

  (** Finds the first [Choice.t] in a list that [has_tmc_calls] *)
  type 'a zipper = {
    rev_before : 'a list;
    choice : 'a t;
    after: 'a t list
  }
  let find_tmc_calls : 'a t list -> ('a zipper, 'a list) result =
    let rec find rev_before = function
      | [] -> Error (List.rev rev_before)
      | choice :: after ->
          if choice.has_tmc_calls
          then Ok { rev_before; choice; after }
          else find (choice.direct () :: rev_before) after
    in find []
end

open Choice.Syntax

type context = {
  specialized: specialized Ident.Map.t;
}
and specialized = {
  arity: int;
  dps_id: Ident.t;
}

let tmc_placeholder = Lconst (Const_base (Const_int 0))
(* TODO consider using a more magical constant like 42, for debugging? *)

let find_candidate = function
  | Lfunction lfun when lfun.attr.tmc_candidate -> Some lfun
  | _ -> None

let declare_binding ctx (var, def) =
  match find_candidate def with
  | None -> ctx
  | Some lfun ->
  let arity = List.length lfun.params in
  let dps_id = Ident.create_local (Ident.name var ^ "_dps") in
  let cand = { arity; dps_id } in
  { specialized = Ident.Map.add var cand ctx.specialized }

let rec choice ctx t =
  let rec choice ctx t =
    match t with
    | (Lvar _ | Lmutvar _ | Lconst _ | Lfunction _ | Lsend _
      | Lassign _ | Lfor _ | Lwhile _) ->
        let t = traverse ctx t in
        Choice.return t

    (* [choice_prim] handles most primitives, but the important case of construction
       [Lprim(Pmakeblock(...), ...)] is handled by [choice_makeblock] *)
    | Lprim (prim, primargs, loc) ->
        choice_prim ctx prim primargs loc

    (* [choice_apply] handles applications, in particular tail-calls which
       generate Set choices at the leaves *)
    | Lapply apply ->
        choice_apply ctx apply
    (* other cases use the [lift] helper that takes the sub-terms in tail
       position and the context around them, and generates a choice for
       the whole term from choices for the tail subterms. *)
    | Lsequence (l1, l2) ->
        let l1 = traverse ctx l1 in
        let+ l2 = choice ctx l2 in
        Lsequence (l1, l2)
    | Lifthenelse (l1, l2, l3) ->
        let l1 = traverse ctx l1 in
        let+ (l2, l3) = choice_pair ctx (l2, l3) in
        Lifthenelse (l1, l2, l3)
    | Lmutlet (vk, var, def, body) ->
        (* non-recursive bindings are not specialized *)
        let def = traverse ctx def in
        let+ body = choice ctx body in
        Lmutlet (vk, var, def, body)
    | Llet (lk, vk, var, def, body) ->
        (* non-recursive bindings are not specialized *)
        let def = traverse ctx def in
        let+ body = choice ctx body in
        Llet (lk, vk, var, def, body)
    | Lletrec (bindings, body) ->
        let ctx, bindings = traverse_letrec ctx bindings in
        let+ body = choice ctx body in
        Lletrec(bindings, body)
    | Lswitch (l1, sw, loc) ->
        (* decompose *)
        let consts_lhs, consts_rhs = List.split sw.sw_consts in
        let blocks_lhs, blocks_rhs = List.split sw.sw_blocks in
        (* transform *)
        let l1 = traverse ctx l1 in
        let+ consts_rhs = choice_list ctx consts_rhs
        and+ blocks_rhs = choice_list ctx blocks_rhs
        and+ sw_failaction = choice_option ctx sw.sw_failaction in
        (* rebuild *)
        let sw_consts = List.combine consts_lhs consts_rhs in
        let sw_blocks = List.combine blocks_lhs blocks_rhs in
        let sw = { sw with sw_consts; sw_blocks; sw_failaction; } in
        Lswitch (l1, sw, loc)
    | Lstringswitch (l1, cases, fail, loc) ->
        (* decompose *)
        let cases_lhs, cases_rhs = List.split cases in
        (* transform *)
        let l1 = traverse ctx l1 in
        let+ cases_rhs = choice_list ctx cases_rhs
        and+ fail = choice_option ctx fail in
        (* rebuild *)
        let cases = List.combine cases_lhs cases_rhs in
        Lstringswitch (l1, cases, fail, loc)
    | Lstaticraise (id, ls) ->
        let ls = traverse_list ctx ls in
        Choice.return (Lstaticraise (id, ls))
    | Ltrywith (l1, id, l2) ->
        (* in [try l1 with id -> l2], the term [l1] is
           not in tail-call position (after it returns
           we need to remove the exception handler),
           so it is not transformed here *)
        let l1 = traverse ctx l1 in
        let+ l2 = choice ctx l2 in
        Ltrywith (l1, id, l2)
    | Lstaticcatch (l1, ids, l2) ->
        (* In [static-catch l1 with ids -> l2],
           the term [l1] is in fact in tail-position *)
        let+ l1 = choice ctx l1
        and+ l2 = choice ctx l2 in
        Lstaticcatch (l1, ids, l2)
    | Levent (lam, lev) ->
        let+ lam = choice ctx lam in
        Levent (lam, lev)
    | Lifused (x, lam) ->
        let+ lam = choice ctx lam in
        Lifused (x, lam)

  and choice_apply ctx apply =
    let exception No_tmc in
    try
      match apply.ap_func with
      | Lvar f ->
          (* TODO: if [@tailcall false] then raise No_tmc; *)
          let specialized =
            try Ident.Map.find f ctx.specialized
            with Not_found ->
              (* TODO warn: tail-callness of the call is broken in
                 the destination-passing-style version; either the function [f]
                 should be marked as tmc-specializable at the callsite,
                 or the user should add [@tailcall false] to clarify
                 that they are aware of this limitation. *)
              raise No_tmc
          in
          {
            Choice.dps = (fun ~tail ~dst ->
              let f_dps = specialized.dps_id in
              Lapply { apply with
                       ap_func = Lvar f_dps;
                       ap_args = add_dst_args dst apply.ap_args;
                       ap_tailcall =
                         if tail
                         then Tailcall_expectation true
                         else Default_tailcall;
                     });
            direct = (fun () -> Lapply apply);
            has_tmc_calls = true;
          }
      | _nontail -> raise No_tmc
    with No_tmc -> Choice.return (Lapply apply)

  and choice_makeblock ctx (tag, flag, shape) blockargs loc =
    let k new_flag new_block_args =
      Lprim (Pmakeblock (tag, new_flag, shape), new_block_args, loc) in
    let choices = List.map (choice ctx) blockargs in
    match Choice.find_tmc_calls choices with
    | Error args -> Choice.return (k flag args)
    | Ok { Choice.rev_before; choice; after } ->
        begin
          (* fail if this settable position is not unique *)
          match Choice.find_tmc_calls after with
          | Error _ -> ()
          | Ok _another_choice ->
              failwith "TODO proper error/warning: ambiguous settable position"
        end;
        let k_with_placeholder =
          k Mutable
            (List.rev_append rev_before @@
             tmc_placeholder ::
             List.map Choice.direct after)
        in
        let placeholder_pos = List.length rev_before in
        let placeholder_pos_lam = Lconst (Const_base (Const_int placeholder_pos)) in
        let let_block_in body =
          let block_var = Ident.create_local "block" in
          Llet(Strict, Pgenval, block_var, k_with_placeholder,
               body block_var)
        in
        let block_dst block_var = {
          var = block_var;
          offset = placeholder_pos_lam;
          loc;
        } in
        {
          Choice.dps = (fun ~tail ~dst:old_dst ->
            let_block_in @@ fun block_var ->
            Lsequence(assign_to_dst old_dst (Lvar block_var),
                      choice.dps ~tail ~dst:(block_dst block_var))
          );
          direct = (fun () ->
            let_block_in @@ fun block_var ->
            Lsequence(choice.dps ~tail:false ~dst:(block_dst block_var),
                      Lvar block_var)
          );
          has_tmc_calls = choice.has_tmc_calls;
        }

  and choice_prim ctx prim primargs loc =
    match prim with
    (* The important case is the construction case *)
    | Pmakeblock (tag, flag, shape) ->
        choice_makeblock ctx (tag, flag, shape) primargs loc

    (* Some primitives have arguments in tail-position *)
    | Popaque ->
        let l1 = match primargs with
          |  [l1] -> l1
          | _ -> invalid_arg "choice_prim" in
        let+ l1 = choice ctx l1 in
        Lprim (Popaque, [l1], loc)
    | (Psequand | Psequor) as shortcutop ->
        let l1, l2 = match primargs with
          |  [l1; l2] -> l1, l2
          | _ -> invalid_arg "choice_prim" in
        let l1 = traverse ctx l1 in
        let+ l2 = choice ctx l2 in
        Lprim (shortcutop, [l1; l2], loc)

    (* in common cases we just return *)
    | Pbytes_to_string | Pbytes_of_string
    | Pgetglobal _ | Psetglobal _
    | Pfield _ | Pfield_computed
    | Psetfield _ | Psetfield_computed _
    | Pfloatfield _ | Psetfloatfield _
    | Pccall _
    | Praise _
    | Pnot
    | Pnegint | Paddint | Psubint | Pmulint | Pdivint _ | Pmodint _
    | Pandint | Porint | Pxorint
    | Plslint | Plsrint | Pasrint
    | Pintcomp _
    | Poffsetint _ | Poffsetref _
    | Pintoffloat | Pfloatofint
    | Pnegfloat | Pabsfloat
    | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
    | Pfloatcomp _
    | Pstringlength | Pstringrefu  | Pstringrefs
    | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
    | Parraylength _ | Parrayrefu _ | Parraysetu _ | Parrayrefs _ | Parraysets _
    | Pisint | Pisout
    | Pignore
    | Pcompare_ints | Pcompare_floats | Pcompare_bints _

    (* we don't handle array indices as destinations yet *)
    | (Pmakearray _ | Pduparray _)

    (* we don't handle { foo with x = ...; y = recursive-call } *)
    | Pduprecord _

    (* operations returning boxed values could be considered constructions someday *)
    | Pbintofint _ | Pintofbint _
    | Pcvtbint _
    | Pnegbint _
    | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _
    | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _
    | Pbintcomp _

    (* more common cases... *)
    | Pbigarrayref _ | Pbigarrayset _
    | Pbigarraydim _
    | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_64 _
    | Pbytes_load_16 _ | Pbytes_load_32 _ | Pbytes_load_64 _
    | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_64 _
    | Pbigstring_load_16 _ | Pbigstring_load_32 _ | Pbigstring_load_64 _
    | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _ | Pctconst _
    | Pbswap16
    | Pbbswap _
    | Pint_as_pointer
      ->
        let primargs = traverse_list ctx primargs in
        Choice.return (Lprim (prim, primargs, loc))

  and choice_list ctx terms =
    Choice.list (List.map (choice ctx) terms)
  and choice_pair ctx (t1, t2) =
    Choice.pair (choice ctx t1, choice ctx t2)
  and choice_option ctx t =
    Choice.option (Option.map (choice ctx) t)

  in choice ctx t

and traverse ctx = function
  | Lletrec (bindings, body) ->
      let ctx, bindings = traverse_letrec ctx bindings in
      Lletrec (bindings, traverse ctx body)
  | lam ->
      shallow_map (traverse ctx) lam

and traverse_letrec ctx bindings =
  let ctx = List.fold_left declare_binding ctx bindings in
  let bindings = List.concat_map (traverse_binding ctx) bindings in
  ctx, bindings

and traverse_binding ctx (var, def) =
  match find_candidate def with
  | None -> [(var, traverse ctx def)]
  | Some lfun ->
  let special = Ident.Map.find var ctx.specialized in
  let fun_choice = choice ctx lfun.body in
  let direct =
    Lfunction { lfun with body = Choice.direct fun_choice } in
  let dps =
    let dst = {
      var = Ident.create_local "dst";
      offset = Ident.create_local "offset";
      loc = lfun.loc;
    } in
    let dst_lam = { dst with offset = Lvar dst.offset } in
    Lambda.duplicate @@ Lfunction { lfun with (* TODO check function_kind *)
      params = add_dst_params dst lfun.params;
      body = Choice.dps ~tail:true ~dst:dst_lam fun_choice;
    } in
  let dps_var = special.dps_id in
  [(var, direct); (dps_var, dps)]

and traverse_list ctx terms =
  List.map (traverse ctx) terms

let rewrite t =
  let ctx = { specialized = Ident.Map.empty } in
  traverse ctx t
