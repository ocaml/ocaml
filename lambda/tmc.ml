(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Frédéric Bour                                              *)
(*             Gabriel Scherer, projet Partout, INRIA Saclay              *)
(*             Basile Clément, projet Cambium, INRIA Paris                *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lambda

(* Error-reporting information for ambiguous TMC calls *)
type tmc_call_information = {
  loc: scoped_location;
  explicit: bool;
}
type subterm_information = {
  tmc_calls: tmc_call_information list;
}
type ambiguous_arguments = {
  explicit: bool;
  (** When [explicit = true], we have an ambiguity between
      arguments containing calls that have been explicitly
      marked [@tailcall]. Otherwise we have an ambiguity
      between un-annotated calls. *)
  arguments: subterm_information list;
}

type error =
  | Ambiguous_constructor_arguments of ambiguous_arguments

exception Error of Location.t * error


type 'offset destination = {
  var: Ident.t;
  offset: 'offset;
  loc : Debuginfo.Scoped_location.t;
}
and offset = Offset of lambda
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

let offset_code (Offset t) = t

let add_dst_params ({var; offset} : Ident.t destination) params =
  (var, Pgenval) :: (offset, Pintval) :: params

let add_dst_args ({var; offset} : offset destination) args =
  Lvar var :: offset_code offset :: args

let assign_to_dst {var; offset; loc} lam =
  Lprim(Psetfield_computed(Pointer, Heap_initialization),
        [Lvar var; offset_code offset; lam], loc)

module Constr : sig
  (** The type [Constr.t] represents a reified constructor with
     a single hole, which can be either directly applied to a [lambda]
     term, or be used to create a fresh [lambda destination] with
     a placeholder. *)
  type t = {
    tag : int;
    flag: Asttypes.mutable_flag;
    shape : block_shape;
    before: lambda list;
    after: lambda list;
    loc : Debuginfo.Scoped_location.t;
  }

  (** [apply constr e] plugs the expression [e] in the hole of the
     constructor [const]. *)
  val apply : t -> lambda -> lambda

  (** [with_placeholder constr body] binds a placeholder
      for the constructor [constr] within the scope of [body]. *)
  val with_placeholder : t -> (offset destination -> lambda) -> lambda

  (** We may want to delay the application of a constructor to a later
      time. This may move the constructor application below some
      effectful expressions (for example if we move into a context of
      the form [foo; bar_with_tmc_inside]), and we want to preserve
      the evaluation order of the other arguments of the
      constructor. So we bind them before proceeding, unless they are
      obviously side-effect free.

      [delay_impure ~block_id constr body] binds all inpure arguments
      of the constructor [constr] within the scope of [body], which is
      passed a pure constructor.

      [block_id] is a counter that is used as a suffix in the generated
      variable names, for readability purposes. *)
  val delay_impure : block_id:int -> t -> (t -> lambda) -> lambda
end = struct
  type t = {
    tag : int;
    flag: Asttypes.mutable_flag;
    shape : block_shape;
    before: lambda list;
    after: lambda list;
    loc : Debuginfo.Scoped_location.t;
  }

  let apply constr t =
    let block_args = List.append constr.before @@ t :: constr.after in
    Lprim (Pmakeblock (constr.tag, constr.flag, constr.shape),
           block_args, constr.loc)

  let tmc_placeholder =
    (* we choose a placeholder whose tagged representation will be
       reconizable. *)
    Lambda.dummy_constant

  let with_placeholder constr (body : offset destination -> lambda) =
    let k_with_placeholder =
      apply { constr with flag = Mutable } tmc_placeholder in
    let placeholder_pos = List.length constr.before in
    let placeholder_pos_lam = Lconst (Const_base (Const_int placeholder_pos)) in
    let block_var = Ident.create_local "block" in
    Llet (Strict, Pgenval, block_var, k_with_placeholder,
          body {
            var = block_var;
            offset = Offset placeholder_pos_lam ;
            loc = constr.loc;
          })

  let delay_impure : block_id:int -> t -> (t -> lambda) -> lambda =
    let bind_list ~block_id ~arg_offset lambdas k =
      let can_be_delayed =
        (* Note that the delayed subterms will be used
           exactly once in the linear-static subterm. So
           we are happy to delay constants, which we would
           not want to duplicate. *)
        function
        | Lvar _ | Lconst _ -> true
        | _ -> false in
      let bindings, args =
        lambdas
        |> List.mapi (fun i lam ->
            if can_be_delayed lam then (None, lam)
            else begin
              let v = Ident.create_local
                  (Printf.sprintf "block%d_arg%d" block_id (arg_offset + i)) in
              (Some (v, lam), Lvar v)
            end)
        |> List.split in
      let body = k args in
      List.fold_right (fun binding body ->
          match binding with
          | None -> body
          | Some (v, lam) -> Llet(Strict, Pgenval, v, lam, body)
        ) bindings body in
    fun ~block_id constr body ->
    bind_list ~block_id ~arg_offset:0 constr.before @@ fun vbefore ->
    let arg_offset = List.length constr.before + 1 in
    bind_list ~block_id ~arg_offset constr.after @@ fun vafter ->
    body { constr with before = vbefore; after = vafter }
end

(** The type ['a Dps.t] (destination-passing-style) represents a
    version of ['a] that is parametrized over a [lambda destination].
    A [lambda Dps.t] is a code fragment in destination-passing-style,
    a [(lambda * lambda) Dps.t] represents two subterms parametrized
    over the same destination. *)
module Dps : sig
  type 'a dps = tail:bool -> dst:offset destination -> 'a
  (** A term parameterized over a destination.  The [tail] argument
      is passed by the caller to indicate whether the term will be placed
      in tail-position -- this allows to generate correct @tailcall
      annotations. *)

  type 'a t

  val make : lambda dps -> lambda t
  val run : lambda t -> lambda dps
  val delay_constructor : Constr.t -> lambda t -> lambda t

  val lambda : lambda -> lambda t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val unit : unit t
end = struct
  type 'a dps = tail:bool -> dst:offset destination -> 'a

  type 'a t = {
    code : delayed:Constr.t list -> 'a dps;
    delayed_use_count : int;
  }
  (** We want to optimize nested constructors, for example:

      {[
        (x () :: y () :: tmc call)
      ]}

      which would naively generate (in a DPS context parametrized
      over a location dst.i):

      {[
        let dstx = x () :: Placeholder in
        dst.i <- dstx;
        let dsty = y () :: Placeholder in
        dstx.1 <- dsty;
        tmc dsty.1 call
      ]}

      when we would rather hope for

      {[
        let vx = x () in
        let dsty = y () :: Placeholder in
        dst.i <- vx :: dsty;
        tmc dsty.1 call
      ]}

      The idea is that the unoptimized version first creates a
      destination site [dstx], which is then used by the following
      code.  If we keep track of the current destination:

      {[
        (* Destination is [dst.i] *)
        let dstx = x () :: Placeholder in
        dst.i (* Destination *) <- dstx;
        (* Destination is [dstx.1] *)
        let dsty = y () :: Placeholder in
        dstx.1 (* Destination *) <- dsty;
        (* Destination is [dsty.1] *)
        tmc dsty.1 call
      ]}

      Instead of binding the whole newly-created destination, we can
      simply let-bind the non-placeholder arguments (in order to
      preserve execution order), and keep track of a list of blocks to
      be created along with the current destination.  Instead of seeing
      a DPS fragment as writing to a destination, we see it as a term
      with shape [dst.i <- C .] where [C .] is a linear context consisting
      only of constructor applications.

      {[
        (* Destination is [dst.i <- C .] *)
        let vx = x () in
        (* Destination is [dst.i <- C (vx :: .)] *)
        let vy = y () in
        (* Destination is [dst.i <- C (vx :: vy :: .)] *)
        (* Making a call: reify the destination *)
        let dsty = vy :: Placeholder in
        dst.i <- vx :: dsty;
        tmc dsty.1 call
      ]}

      The [delayed] argument represents the context [C] as a list of
      reified constructors, to allow both to build the final holey
      block ([vy :: Placeholder]) at the recursive call site, and
      the delayed constructor applications ([vx :: dsty]).

      In practice, it is not desirable to perform this simplification
      when there are multiple TMC calls (e.g. in different branches of
      an [if] block), because it would cause duplication of the nested
      constructor applications.  The [delayed_use_count] field keeps track
      of this information, it counts the number of syntactic use sites
      of the delayed constructors, if any, in the generated code.
  *)

  let write_to_dst dst delayed t =
    assign_to_dst dst @@
    List.fold_left (fun t constr -> Constr.apply constr t) t delayed

  let lambda (v : lambda) : lambda t = {
    code = (fun ~delayed ~tail:_ ~dst ->
      write_to_dst dst delayed v
    );
    delayed_use_count = 1;
  }
  (** Create a new destination-passing-style term which is simply
      setting the destination with the given [v], hence "returning"
      it.
   *)

  let unit : unit t = {
    code = (fun ~delayed:_ ~tail:_ ~dst:_ ->
      ()
    );
    delayed_use_count = 0;
  }

  let map (f : 'a -> 'b) (d : 'a t) : 'b t = {
    code = (fun ~delayed ~tail ~dst  ->
      f @@ d.code ~delayed ~tail ~dst);
    delayed_use_count = d.delayed_use_count;
  }

  let pair (da : 'a t) (db : 'b t) : ('a * 'b) t = {
    code = (fun ~delayed ~tail ~dst ->
      (da.code ~delayed ~tail ~dst, db.code ~delayed ~tail ~dst));
    delayed_use_count =
      da.delayed_use_count + db.delayed_use_count;
  }

  let run (d : 'a t) : 'a dps =
    fun ~tail ~dst ->
    d.code ~tail ~dst ~delayed:[]

  let reify_delay (dps : lambda dps) : lambda t = {
    code = (fun ~delayed ~tail ~dst ->
      match delayed with
      | [] -> dps ~tail ~dst
      | x :: xs ->
          Constr.with_placeholder x @@ fun new_dst ->
          Lsequence (
            write_to_dst dst xs (Lvar new_dst.var),
            dps ~tail ~dst:new_dst)
    );
    delayed_use_count = 1;
  }

  let ensures_affine (d : lambda t) : lambda t =
    if d.delayed_use_count <= 1 then
      d
    else
      reify_delay (run d)
  (** Ensures that the resulting term does not duplicate delayed
      constructors by reifying them now if needed.
   *)

  let make (dps : 'a dps) : 'a t =
    reify_delay dps

  let delay_constructor constr d =
    let d = ensures_affine d in {
      code = (fun ~delayed ~tail ~dst ->
        let block_id = List.length delayed in
        Constr.delay_impure ~block_id constr @@ fun constr ->
        d.code ~tail ~dst ~delayed:(constr :: delayed));
      delayed_use_count = d.delayed_use_count;
    }
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
    type [lambda Choice.t], that contains information on how to transform the
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
    tmc_calls : tmc_call_information list;
    benefits_from_dps: bool;
    explicit_tailcall_request: bool;
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

     - [tmc_calls] tracks the function calls in the subterms that are
       in tail-modulo-cons position and get rewritten into tailcalls
       in the [dps] version.

     - [benefits_from_dps] is true when the [dps] calls strictly more
       TMC functions than the [direct] version. See the
       {!choice_makeblock} case.

     - [explicit_tailcall_request] is true when the user
       used a [@tailcall] annotation on the optimizable callsite.
       When one of several calls could be optimized, we expect that
       exactly one of them will be annotated by the user, or fail
       because the situation is ambiguous.
   *)

  let lambda (v : lambda) : lambda t = {
    dps = Dps.lambda v;
    direct = (fun () -> v);
    tmc_calls = [];
    benefits_from_dps = false;
    explicit_tailcall_request = false;
  }

  let map f s = {
    dps = Dps.map f s.dps;
    direct = (fun () -> f (s.direct ()));
    tmc_calls = s.tmc_calls;
    benefits_from_dps = s.benefits_from_dps;
    explicit_tailcall_request = s.explicit_tailcall_request;
  }
  (** Apply function [f] to the transformed term. *)

  let direct (c : 'a t) : 'a =
    c.direct ()

  let dps (c : lambda t) ~tail ~dst =
    Dps.run c.dps ~tail ~dst

  let pair ((c1, c2) : 'a t * 'b t) : ('a * 'b) t = {
    dps = Dps.pair c1.dps c2.dps;
    direct = (fun () -> (c1.direct (), c2.direct ()));
    tmc_calls =
      c1.tmc_calls @ c2.tmc_calls;
    benefits_from_dps =
      c1.benefits_from_dps || c2.benefits_from_dps;
    explicit_tailcall_request =
      c1.explicit_tailcall_request || c2.explicit_tailcall_request;
  }

  let unit = {
    dps = Dps.unit;
    direct = (fun () -> ());
    tmc_calls = [];
    benefits_from_dps = false;
    explicit_tailcall_request = false;
  }
  (* Remark: we could define [pure v] as [map (fun () -> v) unit],
     but we prefer to have the code explicit about using [unit],
     in particular as it ignores the destination argument. *)

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

  (** The [find_*] machinery is used to locate a single subterm to
      optimize among a list of subterms. If there are several possible
      choices, we require that exactly one of them be annotated with
      [@tailcall], or we report an ambiguity. *)
  type 'a tmc_call_search =
    | No_tmc_call of 'a list
    | Nonambiguous of 'a zipper
    | Ambiguous of { explicit: bool; subterms: 'a t list; }

  and 'a zipper = {
    rev_before : 'a list;
    choice : 'a t;
    after: 'a list
  }

  let find_nonambiguous_tmc_call choices =
    let has_tmc_calls c = c.tmc_calls <> [] in
    let is_explicit s = s.explicit_tailcall_request in
    let nonambiguous ~only_explicit_calls choices =
      (* here is how we will compute the result once we know that there
         is an unambiguously-determined tmc call, and whether
         an explicit request was necessary to disambiguate *)
      let rec split rev_before : 'a t list -> 'a zipper = function
        | [] -> assert false (* we know there is at least one choice *)
        | c :: rest ->
          if has_tmc_calls c && (not only_explicit_calls || is_explicit c) then
            { rev_before; choice = c; after = List.map direct rest }
          else
            split (direct c :: rev_before) rest
      in split [] choices
    in
    let tmc_call_subterms =
      List.filter (fun c -> has_tmc_calls c) choices
    in
    match tmc_call_subterms with
    | [] ->
        No_tmc_call (List.map direct choices)
    | [ _one ] ->
        Nonambiguous (nonambiguous ~only_explicit_calls:false choices)
    | several_subterms ->
        let explicit_subterms = List.filter is_explicit several_subterms in
        begin match explicit_subterms with
        | [] ->
            Ambiguous {
              explicit = false;
              subterms = several_subterms;
            }
        | [ _one ] ->
            Nonambiguous (nonambiguous ~only_explicit_calls:true choices)
        | several_explicit_subterms ->
            Ambiguous {
              explicit = true;
              subterms = several_explicit_subterms;
            }
        end
end

open Choice.Syntax

type context = {
  specialized: specialized Ident.Map.t;
}
and specialized = {
  arity: int;
  dps_id: Ident.t;
  direct_kind: function_kind;
}

let llets lk vk bindings body =
  List.fold_right (fun (var, def) body ->
    Llet (lk, vk, var, def, body)
  ) bindings body

let find_candidate = function
  | Lfunction lfun when lfun.attr.tmc_candidate -> Some lfun
  | _ -> None

let declare_binding ctx (var, def) =
  match find_candidate def with
  | None -> ctx
  | Some lfun ->
  let arity = List.length lfun.params in
  let dps_id = Ident.create_local (Ident.name var ^ "_dps") in
  let direct_kind = lfun.kind in
  let cand = { arity; dps_id; direct_kind; } in
  { specialized = Ident.Map.add var cand ctx.specialized }

let rec choice ctx t =
  let rec choice ctx ~tail t =
    match t with
    | (Lvar _ | Lmutvar _ | Lconst _ | Lfunction _ | Lsend _
      | Lassign _ | Lfor _ | Lwhile _) ->
        let t = traverse ctx t in
        Choice.lambda t

    (* [choice_prim] handles most primitives, but the important case
       of construction [Lprim(Pmakeblock(...), ...)] is handled by
       [choice_makeblock] *)
    | Lprim (prim, primargs, loc) ->
        choice_prim ctx ~tail prim primargs loc

    (* [choice_apply] handles applications, in particular tail-calls which
       generate Set choices at the leaves *)
    | Lapply apply ->
        choice_apply ctx ~tail apply
    (* other cases use the [lift] helper that takes the sub-terms in tail
       position and the context around them, and generates a choice for
       the whole term from choices for the tail subterms. *)
    | Lsequence (l1, l2) ->
        let l1 = traverse ctx l1 in
        let+ l2 = choice ctx ~tail l2 in
        Lsequence (l1, l2)
    | Lifthenelse (l1, l2, l3) ->
        let l1 = traverse ctx l1 in
        let+ (l2, l3) = choice_pair ctx ~tail (l2, l3) in
        Lifthenelse (l1, l2, l3)
    | Lmutlet (vk, var, def, body) ->
        (* mutable bindings are not TMC-specialized *)
        let def = traverse ctx def in
        let+ body = choice ctx ~tail body in
        Lmutlet (vk, var, def, body)
    | Llet (lk, vk, var, def, body) ->
        let ctx, bindings = traverse_let ctx var def in
        let+ body = choice ctx ~tail body in
        llets lk vk bindings body
    | Lletrec (bindings, body) ->
        let ctx, bindings = traverse_letrec ctx bindings in
        let+ body = choice ctx ~tail body in
        Lletrec(bindings, body)
    | Lswitch (l1, sw, loc) ->
        (* decompose *)
        let consts_lhs, consts_rhs = List.split sw.sw_consts in
        let blocks_lhs, blocks_rhs = List.split sw.sw_blocks in
        (* transform *)
        let l1 = traverse ctx l1 in
        let+ consts_rhs = choice_list ctx ~tail consts_rhs
        and+ blocks_rhs = choice_list ctx ~tail blocks_rhs
        and+ sw_failaction = choice_option ctx ~tail sw.sw_failaction in
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
        let+ cases_rhs = choice_list ctx ~tail cases_rhs
        and+ fail = choice_option ctx ~tail fail in
        (* rebuild *)
        let cases = List.combine cases_lhs cases_rhs in
        Lstringswitch (l1, cases, fail, loc)
    | Lstaticraise (id, ls) ->
        let ls = traverse_list ctx ls in
        Choice.lambda (Lstaticraise (id, ls))
    | Ltrywith (l1, id, l2) ->
        (* in [try l1 with id -> l2], the term [l1] is
           not in tail-call position (after it returns
           we need to remove the exception handler) *)
        let+ l1 = choice ctx ~tail:false l1
        and+ l2 = choice ctx ~tail l2 in
        Ltrywith (l1, id, l2)
    | Lstaticcatch (l1, ids, l2) ->
        (* In [static-catch l1 with ids -> l2],
           the term [l1] is in fact in tail-position *)
        let+ l1 = choice ctx ~tail l1
        and+ l2 = choice ctx ~tail l2 in
        Lstaticcatch (l1, ids, l2)
    | Levent (lam, lev) ->
        let+ lam = choice ctx ~tail lam in
        Levent (lam, lev)
    | Lifused (x, lam) ->
        let+ lam = choice ctx ~tail lam in
        Lifused (x, lam)

  and choice_apply ctx ~tail apply =
    let exception No_tmc in
    try
      let explicit_tailcall_request =
        match apply.ap_tailcall with
        | Default_tailcall -> false
        | Tailcall_expectation true -> true
        | Tailcall_expectation false -> raise No_tmc
      in
      match apply.ap_func with
      | Lvar f ->
          let specialized =
            try Ident.Map.find f ctx.specialized
            with Not_found ->
              if tail then
                Location.prerr_warning
                  (Debuginfo.Scoped_location.to_location apply.ap_loc)
                  Warnings.Tmc_breaks_tailcall;
              raise No_tmc;
          in
          let args =
            (* Support of tupled functions: the [function_kind] of the
               direct-style function is identical to the one of the
               input function, which may be Tupled, but the dps
               function is always Curried.

               [find_exact_application] is in charge of recovering the
               "real" argument list of a possibly-tupled call. *)
            let kind, arity = specialized.direct_kind, specialized.arity in
            match Lambda.find_exact_application kind ~arity apply.ap_args with
            | None -> raise No_tmc
            | Some args -> args
          in
          let tailcall tail =
            (* If we are calling a tmc-specializable function in tail
               context, then both the direct-style and dps-style calls
               must be tailcalls. *)
            if tail
            then Tailcall_expectation true
            else Default_tailcall
          in
          {
            Choice.dps = Dps.make (fun ~tail ~dst ->
              Lapply { apply with
                       ap_func = Lvar specialized.dps_id;
                       ap_args = add_dst_args dst args;
                       ap_tailcall = tailcall tail;
                     });
            direct = (fun () ->
              Lapply { apply with ap_tailcall = tailcall tail });
            explicit_tailcall_request;
            tmc_calls = [{
              loc = apply.ap_loc;
              explicit = explicit_tailcall_request;
            }];
            benefits_from_dps = true;
          }
      | _nontail -> raise No_tmc
    with No_tmc ->
      let apply_no_bailout =
        (* [@tailcall false] is interpreted as a bailout annotation: "we
           are (knowingly) leaving the dps calling convention". It only
           has sense in the DPS version of the generated code, not in
           direct style. *)
        let ap_tailcall =
          match apply.ap_tailcall with
          | Tailcall_expectation false when tail -> Default_tailcall
          | other -> other
        in
        { apply with ap_tailcall } in
      { (Choice.lambda (Lapply apply)) with
        direct = (fun () -> Lapply apply_no_bailout);
      }

  and choice_makeblock ctx ~tail:_ (tag, flag, shape) blockargs loc =
    let choices = List.map (choice ctx ~tail:false) blockargs in
    match Choice.find_nonambiguous_tmc_call choices with
    | Choice.No_tmc_call args ->
        Choice.lambda @@ Lprim (Pmakeblock (tag, flag, shape), args, loc)
    | Choice.Ambiguous { explicit; subterms = ambiguous_subterms } ->
        (* An ambiguous term should not lead to an error if it not
           used in TMC position. Consider for example:

           {[
             type t = ... | K of t * (t * t)
             let[@tail_mod_cons] rec map f = function
             | [...]
             | K (t, (u, v)) -> K ((map[@tailcall]) f t, (map f u, map f v))
           ]}

           Calling [choice_makeblock] on the K constructor, we need to
           determine whether its two arguments are ambiguous, which is
           done by calling [choice] on each argument to see if they
           would be TMC-able and if they are explicitly annotated.

           These calls give the following results:
           - there is an explicitly-requested tailcall in the first
             argument
           - the second argument is a nested pair whose arguments
             themselves are ambiguous -- with no explicit annotation.

           This determines that the arguments of K are not ambiguous,
           as only one of them is annotated. But note that the nested
           pair, in isolation, is ambiguous. This inner ambiguity is
           innocuous and should not result in an error, as we never
           use this inner pair in TMC position, only in direct style.

           This example shows that it would be incorrect to fail with
           an error whenever [choice] finds an ambiguity. Instead we
           only error when generating the [dps] version of the
           corresponding code; requesting the [direct] version is
           accepted and produces the expected direct code.
        *)
        let term_choice =
          let+ args = Choice.list choices in
          Lprim (Pmakeblock(tag, flag, shape), args, loc)
        in
        { term_choice with
          Choice.dps = Dps.make (fun ~tail:_ ~dst:_ ->
            let arguments =
              let info (t : lambda Choice.t) : subterm_information = {
                tmc_calls = t.tmc_calls;
              } in
              {
                explicit;
                arguments = List.map info ambiguous_subterms;
              }
            in
            raise (Error (Debuginfo.Scoped_location.to_location loc,
                          Ambiguous_constructor_arguments arguments))
          );
        }
    | Choice.Nonambiguous { Choice.rev_before; choice; after } ->
        let constr = Constr.{
            tag;
            flag;
            shape;
            before = List.rev rev_before;
            after;
            loc;
        } in
        assert (choice.tmc_calls <> []);
        {
          Choice.direct = (fun () ->
            if not choice.benefits_from_dps then
              Constr.apply constr (Choice.direct choice)
            else
              Constr.with_placeholder constr @@ fun new_dst ->
              Lsequence(Choice.dps choice ~tail:false ~dst:new_dst,
                        Lvar new_dst.var));
          benefits_from_dps =
            (* Whether or not the caller provides a destination,
               we can always provide a destination to our settable
               subterm, so the number of TMC sub-calls is identical
               in the [direct] and [dps] versions. *)
            false;
          dps = Dps.delay_constructor constr choice.dps;
          tmc_calls =
            choice.tmc_calls;
          explicit_tailcall_request =
            choice.explicit_tailcall_request;
        }

  and choice_prim ctx ~tail prim primargs loc =
    match prim with
    (* The important case is the construction case *)
    | Pmakeblock (tag, flag, shape) ->
        choice_makeblock ctx ~tail (tag, flag, shape) primargs loc

    (* Some primitives have arguments in tail-position *)
    | Popaque ->
        let l1 = match primargs with
          |  [l1] -> l1
          | _ -> invalid_arg "choice_prim" in
        let+ l1 = choice ctx ~tail l1 in
        Lprim (Popaque, [l1], loc)

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

    (* we don't handle effect or DLS primitives *)
    | Prunstack | Pperform | Presume | Preperform | Pdls_get

    (* we don't handle atomic primitives *)
    | Patomic_load

    (* we don't handle array indices as destinations yet *)
    | (Pmakearray _ | Pduparray _)

    (* we don't handle { foo with x = ...; y = recursive-call } *)
    | Pduprecord _

    (* operations returning boxed values could be considered
       constructions someday *)
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
    | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _
    | Pctconst _
    | Pbswap16
    | Pbbswap _
    | Pint_as_pointer
    | Psequand | Psequor
    | Ppoll
      ->
        let primargs = traverse_list ctx primargs in
        Choice.lambda (Lprim (prim, primargs, loc))

  and choice_list ctx ~tail terms =
    Choice.list (List.map (choice ctx ~tail) terms)
  and choice_pair ctx ~tail (t1, t2) =
    Choice.pair (choice ctx ~tail t1, choice ctx ~tail t2)
  and choice_option ctx ~tail t =
    Choice.option (Option.map (choice ctx ~tail) t)

  in choice ctx t

and traverse ctx = function
  | Llet (lk, vk, var, def, body) ->
      let ctx, bindings = traverse_let ctx var def in
      let body = traverse ctx body in
      llets lk vk bindings body
  | Lletrec (bindings, body) ->
      let ctx, bindings = traverse_letrec ctx bindings in
      Lletrec (bindings, traverse ctx body)
  | lam ->
      shallow_map (traverse ctx) lam

and traverse_lfunction ctx lfun =
  map_lfunction (traverse ctx) lfun

and traverse_let outer_ctx var def =
  let inner_ctx = declare_binding outer_ctx (var, def) in
  let bindings =
    traverse_let_binding outer_ctx inner_ctx var def
  in
  inner_ctx, bindings

and traverse_letrec ctx bindings =
  let ctx =
    List.fold_left (fun ctx { id; def } ->
        declare_binding ctx (id, Lfunction def)
      ) ctx bindings
  in
  let bindings =
    List.concat_map (traverse_letrec_binding ctx) bindings
  in
  ctx, bindings

and traverse_let_binding outer_ctx inner_ctx var def =
  match find_candidate def with
  | None -> [ var, traverse outer_ctx def ]
  | Some lfun ->
      let functions = make_dps_variant var inner_ctx outer_ctx lfun in
      List.map (fun (var, lfun) -> var, Lfunction lfun) functions

and traverse_letrec_binding ctx { id; def } =
  if def.attr.tmc_candidate
  then
    let functions = make_dps_variant id ctx ctx def in
    List.map (fun (id, def) -> { id; def }) functions
  else
    [ { id; def = traverse_lfunction ctx def } ]

and make_dps_variant var inner_ctx outer_ctx (lfun : lfunction) =
  let special = Ident.Map.find var inner_ctx.specialized in
  let fun_choice = choice outer_ctx ~tail:true lfun.body in
  if fun_choice.Choice.tmc_calls = [] then
    Location.prerr_warning
      (Debuginfo.Scoped_location.to_location lfun.loc)
      Warnings.Unused_tmc_attribute;
  let direct =
    let { kind; params; return; body = _; attr; loc } = lfun in
    let body = Choice.direct fun_choice in
    lfunction' ~kind ~params ~return ~body ~attr ~loc in
  let dps =
    let dst_param = {
      var = Ident.create_local "dst";
      offset = Ident.create_local "offset";
      loc = lfun.loc;
    } in
    let dst = { dst_param with offset = Offset (Lvar dst_param.offset) } in
    Lambda.duplicate_function @@ lfunction'
      ~kind:
        (* Support of Tupled function: see [choice_apply]. *)
        Curried
      ~params:(add_dst_params dst_param lfun.params)
      ~return:lfun.return
      ~body:(Choice.dps ~tail:true ~dst:dst fun_choice)
      ~attr:lfun.attr
      ~loc:lfun.loc
  in
  let dps_var = special.dps_id in
  [var, direct; dps_var, dps]

and traverse_list ctx terms =
  List.map (traverse ctx) terms

let rewrite t =
  let ctx = { specialized = Ident.Map.empty } in
  traverse ctx t

module Style = Misc.Style

let () =
  Location.register_error_of_exn
    (function
      | Error (loc,
               Ambiguous_constructor_arguments
                 { explicit = false; arguments }) ->
          let print_msg ppf =
            Format_doc.fprintf ppf
              "%a:@ this@ constructor@ application@ may@ be@ \
               TMC-transformed@ in@ several@ different@ ways.@ \
               Please@ disambiguate@ by@ adding@ an@ explicit@ %a \
               attribute@ to@ the@ call@ that@ should@ be@ made@ \
               tail-recursive,@ or@ a@ %a attribute@ on@ \
               calls@ that@ should@ not@ be@ transformed."
              Style.inline_code "[@tail_mod_cons]"
              Style.inline_code "[@tailcall]"
              Style.inline_code "[@tailcall false]"
          in
          let submgs =
            let sub (info : tmc_call_information) =
              let loc = Debuginfo.Scoped_location.to_location info.loc in
              Location.msg ~loc "This call could be annotated." in
            arguments
            |> List.map (fun t -> t.tmc_calls)
            |> List.flatten
            |> List.map sub
          in
          Some (Location.errorf ~loc ~sub:submgs "%t" print_msg)
      | Error (loc,
               Ambiguous_constructor_arguments
                 { explicit = true; arguments }) ->
          let print_msg ppf =
            Format_doc.fprintf ppf
              "%a:@ this@ constructor@ application@ may@ be@ \
               TMC-transformed@ in@ several@ different@ ways.@ Only@ one@ of@ \
               the@ arguments@ may@ become@ a@ TMC@ call,@ but@ several@ \
               arguments@ contain@ calls@ that@ are@ explicitly@ marked@ as@ \
               tail-recursive.@ Please@ fix@ the@ conflict@ by@ reviewing@ \
               and@ fixing@ the@ conflicting@ annotations."
              Style.inline_code "[@tail_mod_cons]"
          in
          let submgs =
            let sub (info : tmc_call_information) =
              let loc = Debuginfo.Scoped_location.to_location info.loc in
              Location.msg ~loc "This call is explicitly annotated." in
            arguments
            |> List.map (fun t -> t.tmc_calls)
            |> List.flatten
            |> List.filter (fun (info: tmc_call_information) -> info.explicit)
            |> List.map sub
          in
          Some (Location.errorf ~loc ~sub:submgs "%t" print_msg)
      | _ ->
        None
    )
