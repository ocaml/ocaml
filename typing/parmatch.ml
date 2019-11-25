(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Detection of partial matches and unused match cases. *)

open Misc
open Asttypes
open Types
open Typedtree

(*************************************)
(* Utilities for building patterns   *)
(*************************************)

let make_pat desc ty tenv =
  {pat_desc = desc; pat_loc = Location.none; pat_extra = [];
   pat_type = ty ; pat_env = tenv;
   pat_attributes = [];
  }

let omega = make_pat Tpat_any Ctype.none Env.empty

let extra_pat =
  make_pat
    (Tpat_var (Ident.create_local "+", mknoloc "+"))
    Ctype.none Env.empty

let rec omegas i =
  if i <= 0 then [] else omega :: omegas (i-1)

let omega_list l = List.map (fun _ -> omega) l

module Pattern_head : sig
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
    | Tuple of int
    | Record of label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
    | Array of int
    | Lazy

  type t

  val desc : t -> desc
  val env : t -> Env.t
  val loc : t -> Location.t
  val typ : t -> Types.type_expr

  (** [deconstruct p] returns the head of [p] and the list of sub patterns.

      @raises [Invalid_arg _] if [p] is an or- or an exception-pattern.  *)
  val deconstruct : pattern -> t * pattern list

  (** reconstructs a pattern, putting wildcards as sub-patterns. *)
  val to_omega_pattern : t -> pattern

  val make
    :  loc:Location.t
    -> typ:Types.type_expr
    -> env:Env.t
    -> desc
    -> t

  val omega : t

end = struct
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
    | Tuple of int
    | Record of label_description list
    | Variant of
        { tag: label;
          has_arg: bool;
          cstr_row: row_desc ref;
          type_row: unit -> row_desc; }
    | Array of int
    | Lazy

  type t = {
    desc: desc;
    typ : Types.type_expr;
    loc : Location.t;
    env : Env.t;
    attributes : attributes;
  }

  let desc { desc } = desc
  let env { env } = env
  let loc { loc } = loc
  let typ { typ } = typ

  let deconstruct q =
    let rec deconstruct_desc = function
      | Tpat_any
      | Tpat_var _ -> Any, []
      | Tpat_constant c -> Constant c, []
      | Tpat_alias (p,_,_) -> deconstruct_desc p.pat_desc
      | Tpat_tuple args ->
          Tuple (List.length args), args
      | Tpat_construct (_, c, args) ->
          Construct c, args
      | Tpat_variant (tag, arg, cstr_row) ->
          let has_arg, pats =
            match arg with
            | None -> false, []
            | Some a -> true, [a]
          in
          let type_row () =
            match Ctype.expand_head q.pat_env q.pat_type with
              | {desc = Tvariant type_row} -> Btype.row_repr type_row
              | _ -> assert false
          in
          Variant {tag; has_arg; cstr_row; type_row}, pats
      | Tpat_array args ->
          Array (List.length args), args
      | Tpat_record (largs, _) ->
          let lbls = List.map (fun (_,lbl,_) -> lbl) largs in
          let pats = List.map (fun (_,_,pat) -> pat) largs in
          Record lbls, pats
      | Tpat_lazy p ->
          Lazy, [p]
      | Tpat_or _ -> invalid_arg "Parmatch.Pattern_head.deconstruct: (P | Q)"
    in
    let desc, pats = deconstruct_desc q.pat_desc in
    { desc; typ = q.pat_type; loc = q.pat_loc;
      env = q.pat_env; attributes = q.pat_attributes }, pats

  let to_omega_pattern t =
    let pat_desc =
      match t.desc with
      | Any -> Tpat_any
      | Lazy -> Tpat_lazy omega
      | Constant c -> Tpat_constant c
      | Tuple n -> Tpat_tuple (omegas n)
      | Array n -> Tpat_array (omegas n)
      | Construct c ->
          let lid_loc = Location.mkloc (Longident.Lident c.cstr_name) t.loc in
          Tpat_construct (lid_loc, c, omegas c.cstr_arity)
      | Variant { tag; has_arg; cstr_row } ->
          let arg_opt = if has_arg then Some omega else None in
          Tpat_variant (tag, arg_opt, cstr_row)
      | Record lbls ->
          let lst =
            List.map (fun lbl ->
              let lid_loc =
                Location.mkloc (Longident.Lident lbl.lbl_name) t.loc
              in
              (lid_loc, lbl, omega)
            ) lbls
          in
          Tpat_record (lst, Closed)
    in
    { pat_desc; pat_type = t.typ; pat_loc = t.loc; pat_extra = [];
      pat_env = t.env; pat_attributes = t.attributes }

  let make ~loc ~typ ~env desc =
    { desc; loc; typ; env; attributes = [] }

  let omega =
    { desc = Any
    ; loc = Location.none
    ; typ = Ctype.none
    ; env = Env.empty
    ; attributes = []
    }
end

(*
  Normalize a pattern ->
   all arguments are omega (simple pattern) and no more variables
*)

let normalize_pat p = Pattern_head.(to_omega_pattern @@ fst @@ deconstruct p)

(*******************)
(* Coherence check *)
(*******************)

(* For some of the operations we do in this module, we would like (because it
   simplifies matters) to assume that patterns appearing on a given column in a
   pattern matrix are /coherent/ (think "of the same type").
   Unfortunately that is not always true.

   Consider the following (well-typed) example:
   {[
     type _ t = S : string t | U : unit t

     let f (type a) (t1 : a t) (t2 : a t) (a : a) =
       match t1, t2, a with
       | U, _, () -> ()
       | _, S, "" -> ()
   ]}

   Clearly the 3rd column contains incoherent patterns.

   On the example above, most of the algorithms will explore the pattern matrix
   as illustrated by the following tree:

   {v
                                                   S
                                                -------> | "" |
                             U     | S, "" | __/         | () |
                         --------> | _, () |   \ not S
        | U, _, () | __/                        -------> | () |
        | _, S, "" |   \
                        ---------> | S, "" | ----------> | "" |
                          not U                    S
   v}

   where following an edge labelled by a pattern P means "assuming the value I
   am matching on is filtered by [P] on the column I am currently looking at,
   then the following submatrix is still reachable".

   Notice that at any point of that tree, if the first column of a matrix is
   incoherent, then the branch leading to it can only be taken if the scrutinee
   is ill-typed.
   In the example above the only case where we have a matrix with an incoherent
   first column is when we consider [t1, t2, a] to be [U, S, ...]. However such
   a value would be ill-typed, so we can never actually get there.

   Checking the first column at each step of the recursion and making the
   conscious decision of "aborting" the algorithm whenever the first column
   becomes incoherent, allows us to retain the initial assumption in later
   stages of the algorithms.

   ---

   N.B. two patterns can be considered coherent even though they might not be of
   the same type.

   That's in part because we only care about the "head" of patterns and leave
   checking coherence of subpatterns for the next steps of the algorithm:
   ('a', 'b') and (1, ()) will be deemed coherent because they are both a tuples
   of arity 2 (we'll notice at a later stage the incoherence of 'a' and 1).

   But also because it can be hard/costly to determine exactly whether two
   patterns are of the same type or not (eg. in the example above with _ and S,
   but see also the module [Coherence_illustration] in
   testsuite/tests/basic-more/robustmatch.ml).

   For the moment our weak, loosely-syntactic, coherence check seems to be
   enough and we leave it to each user to consider (and document!) what happens
   when an "incoherence" is not detected by this check.
*)

(* Given the first column of a simplified matrix, this function first looks for
   a "discriminating" pattern on that column (i.e. a non-omega one) and then
   check that every other head pattern in the column is coherent with that one.
*)
let all_coherent column =
  let coherent_heads hp1 hp2 =
    match Pattern_head.desc hp1, Pattern_head.desc hp2 with
    | Construct c, Construct c' ->
      c.cstr_consts = c'.cstr_consts
      && c.cstr_nonconsts = c'.cstr_nonconsts
    | Constant c1, Constant c2 -> begin
        match c1, c2 with
        | Const_char _, Const_char _
        | Const_int _, Const_int _
        | Const_int32 _, Const_int32 _
        | Const_int64 _, Const_int64 _
        | Const_nativeint _, Const_nativeint _
        | Const_float _, Const_float _
        | Const_string _, Const_string _ -> true
        | ( Const_char _
          | Const_int _
          | Const_int32 _
          | Const_int64 _
          | Const_nativeint _
          | Const_float _
          | Const_string _), _ -> false
      end
    | Tuple l1, Tuple l2 -> l1 = l2
    | Record (lbl1 :: _), Record (lbl2 :: _) ->
      Array.length lbl1.lbl_all = Array.length lbl2.lbl_all
    | Any, _
    | _, Any
    | Record [], Record []
    | Variant _, Variant _
    | Array _, Array _
    | Lazy, Lazy -> true
    | _, _ -> false
  in
  match
    List.find (fun head_pat ->
      match Pattern_head.desc head_pat with
      | Any -> false
      | _ -> true
    ) column
  with
  | exception Not_found ->
    (* only omegas on the column: the column is coherent. *)
    true
  | discr_pat ->
    List.for_all (coherent_heads discr_pat) column

let first_column simplified_matrix =
  List.map (fun ((head, _args), _rest) -> head) simplified_matrix

(***********************)
(* Compatibility check *)
(***********************)

(* Patterns p and q compatible means:
   there exists value V that matches both, However....

  The case of extension types is dubious, as constructor rebind permits
  that different constructors are the same (and are thus compatible).

  Compilation must take this into account, consider:

  type t = ..
  type t += A|B
  type t += C=A

  let f x y = match x,y with
  | true,A  -> '1'
  | _,C     -> '2'
  | false,A -> '3'
  | _,_     -> '_'

  As C is bound to A the value of f false A is '2' (and not '3' as it would
  be in the absence of rebinding).

  Not considering rebinding, patterns "false,A" and "_,C" are incompatible
  and the compiler can swap the second and third clause, resulting in the
  (more efficiently compiled) matching

  match x,y with
  | true,A  -> '1'
  | false,A -> '3'
  | _,C     -> '2'
  | _,_     -> '_'

  This is not correct: when C is bound to A, "f false A" returns '2' (not '3')


  However, diagnostics do not take constructor rebinding into account.
  Notice, that due to module abstraction constructor rebinding is hidden.

  module X : sig type t = .. type t += A|B end = struct
    type t = ..
    type t += A
    type t += B=A
  end

  open X

  let f x = match x with
  | A -> '1'
  | B -> '2'
  | _ -> '_'

  The second clause above will NOT (and cannot) be flagged as useless.

  Finally, there are two compatibility functions:
   compat p q      ---> 'syntactic compatibility, used for diagnostics.
   may_compat p q --->   a safe approximation of possible compat,
                         for compilation

*)


let is_absent tag row = Btype.row_field tag !row = Rabsent

let is_absent_pat d =
  match Pattern_head.desc d with
  | Variant { tag; cstr_row; _ } -> is_absent tag cstr_row
  | _ -> false

let const_compare x y =
  match x,y with
  | Const_float f1, Const_float f2 ->
      Stdlib.compare (float_of_string f1) (float_of_string f2)
  | Const_string (s1, _, _), Const_string (s2, _, _) ->
      String.compare s1 s2
  | (Const_int _
    |Const_char _
    |Const_string (_, _, _)
    |Const_float _
    |Const_int32 _
    |Const_int64 _
    |Const_nativeint _
    ), _ -> Stdlib.compare x y

let records_args l1 l2 =
  (* Invariant: fields are already sorted by Typecore.type_label_a_list *)
  let rec combine r1 r2 l1 l2 = match l1,l2 with
  | [],[] -> List.rev r1, List.rev r2
  | [],(_,_,p2)::rem2 -> combine (omega::r1) (p2::r2) [] rem2
  | (_,_,p1)::rem1,[] -> combine (p1::r1) (omega::r2) rem1 []
  | (_,lbl1,p1)::rem1, ( _,lbl2,p2)::rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        combine (p1::r1) (omega::r2) rem1 l2
      else if lbl1.lbl_pos > lbl2.lbl_pos then
        combine (omega::r1) (p2::r2) l1 rem2
      else (* same label on both sides *)
        combine (p1::r1) (p2::r2) rem1 rem2 in
  combine [] [] l1 l2



module Compat
    (Constr:sig
      val equal :
          Types.constructor_description ->
            Types.constructor_description ->
              bool
    end) = struct

  let rec compat p q = match p.pat_desc,q.pat_desc with
(* Variables match any value *)
  | ((Tpat_any|Tpat_var _),_)
  | (_,(Tpat_any|Tpat_var _)) -> true
(* Structural induction *)
  | Tpat_alias (p,_,_),_      -> compat p q
  | _,Tpat_alias (q,_,_)      -> compat p q
  | Tpat_or (p1,p2,_),_ ->
      (compat p1 q || compat p2 q)
  | _,Tpat_or (q1,q2,_) ->
      (compat p q1 || compat p q2)
(* Constructors, with special case for extension *)
  | Tpat_construct (_, c1,ps1), Tpat_construct (_, c2,ps2) ->
      Constr.equal c1 c2 && compats ps1 ps2
(* More standard stuff *)
  | Tpat_variant(l1,op1, _), Tpat_variant(l2,op2,_) ->
      l1=l2 && ocompat op1 op2
  | Tpat_constant c1, Tpat_constant c2 ->
      const_compare c1 c2 = 0
  | Tpat_tuple ps, Tpat_tuple qs -> compats ps qs
  | Tpat_lazy p, Tpat_lazy q -> compat p q
  | Tpat_record (l1,_),Tpat_record (l2,_) ->
      let ps,qs = records_args l1 l2 in
      compats ps qs
  | Tpat_array ps, Tpat_array qs ->
      List.length ps = List.length qs &&
      compats ps qs
  | _,_  -> false

  and ocompat op oq = match op,oq with
  | None,None -> true
  | Some p,Some q -> compat p q
  | (None,Some _)|(Some _,None) -> false

  and compats ps qs = match ps,qs with
  | [], [] -> true
  | p::ps, q::qs -> compat p q && compats ps qs
  | _,_    -> false

end

module SyntacticCompat =
  Compat
    (struct
      let equal c1 c2 =  Types.equal_tag c1.cstr_tag c2.cstr_tag
    end)

let compat =  SyntacticCompat.compat
and compats = SyntacticCompat.compats

(* Due to (potential) rebinding, two extension constructors
   of the same arity type may equal *)

exception Empty (* Empty pattern *)

(****************************************)
(* Utilities for retrieving type paths  *)
(****************************************)

(* May need a clean copy, cf. PR#4745 *)
let clean_copy ty =
  if ty.level = Btype.generic_level then ty
  else Subst.type_expr Subst.identity ty

let get_constructor_type_path ty tenv =
  let ty = Ctype.repr (Ctype.expand_head tenv (clean_copy ty)) in
  match ty.desc with
  | Tconstr (path,_,_) -> path
  | _ -> assert false

(****************************)
(* Utilities for matching   *)
(****************************)

(* Check top matching *)
let simple_match d h =
  match Pattern_head.desc d, Pattern_head.desc h with
  | Construct c1, Construct c2 ->
      Types.equal_tag c1.cstr_tag c2.cstr_tag
  | Variant { tag = t1; _ }, Variant { tag = t2 } ->
      t1 = t2
  | Constant c1, Constant c2 -> const_compare c1 c2 = 0
  | Lazy, Lazy -> true
  | Record _, Record _ -> true
  | Tuple len1, Tuple len2
  | Array len1, Array len2 -> len1 = len2
  | _, Any -> true
  | _, _ -> false



(* extract record fields as a whole *)
let record_arg ph = match Pattern_head.desc ph with
| Any -> []
| Record args -> args
| _ -> fatal_error "Parmatch.as_record"


let extract_fields lbls arg =
  let get_field pos arg =
    match List.find (fun (lbl,_) -> pos = lbl.lbl_pos) arg with
    | _, p -> p
    | exception Not_found -> omega
  in
  List.map (fun lbl -> get_field lbl.lbl_pos arg) lbls

(* Build argument list when p2 >= p1, where p1 is a simple pattern *)
let simple_match_args discr head args = match Pattern_head.desc head with
| Constant _ -> []
| Construct _
| Variant _
| Tuple _
| Array _
| Lazy -> args
| Record lbls ->  extract_fields (record_arg discr) (List.combine lbls args)
| Any ->
    begin match Pattern_head.desc discr with
    | Construct cstr -> omegas cstr.cstr_arity
    | Variant { has_arg = true }
    | Lazy -> [omega]
    | Record lbls ->  omega_list lbls
    | Array len
    | Tuple len -> omegas len
    | Variant { has_arg = false }
    | Any
    | Constant _ -> []
    end

(* Consider a pattern matrix whose first column has been simplified to contain
   only _ or a head constructor
     | p1, r1...
     | p2, r2...
     | p3, r3...
     | ...

   We build a normalized /discriminating/ pattern from a pattern [q] by folding
   over the first column of the matrix, "refining" [q] as we go:

   - when we encounter a row starting with [Tuple] or [Lazy] then we
   can stop and return that head, as we cannot refine any further. Indeed,
   these constructors are alone in their signature, so they will subsume
   whatever other head we might find, as well as the head we're threading
   along.

   - when we find a [Record] then it is a bit more involved: it is also alone
   in its signature, however it might only be matching a subset of the
   record fields. We use these fields to refine our accumulator and keep going
   as another row might match on different fields.

   - rows starting with a wildcard do not bring any information, so we ignore
   them and keep going

   - if we encounter anything else (i.e. any other constructor), then we just
   stop and return our accumulator.
*)
let discr_pat q pss =
  let rec refine_pat acc = function
    | [] -> acc
    | ((head, _), _) :: rows ->
      match Pattern_head.desc head with
      | Any -> refine_pat acc rows
      | Tuple _ | Lazy -> head
      | Record lbls ->
        (* N.B. we could make this case "simpler" by refining the record case
           using [all_record_args].
           In which case we wouldn't need to fold over the first column for
           records.
           However it makes the witness we generate for the exhaustivity warning
           less pretty. *)
        let fields =
          List.fold_right (fun lbl r ->
            if List.exists (fun l -> l.lbl_pos = lbl.lbl_pos) r then
              r
            else
              lbl :: r
          ) lbls (record_arg acc)
        in
        let d =
          let open Pattern_head in
          make ~loc:(loc head) ~typ:(typ head) ~env:(env head) (Record fields)
        in
        refine_pat d rows
      | _ -> acc
  in
  let q, _ = Pattern_head.deconstruct q in
  match Pattern_head.desc q with
  (* short-circuiting: clearly if we have anything other than [Record] or
     [Any] to start with, we're not going to be able refine at all. So
     there's no point going over the matrix. *)
  | Any | Record _ -> refine_pat q pss
  | _ -> q

(*
   In case a matching value is found, set actual arguments
   of the matching pattern.
*)

let rec read_args xs r = match xs,r with
| [],_ -> [],r
| _::xs, arg::rest ->
   let args,rest = read_args xs rest in
   arg::args,rest
| _,_ ->
    fatal_error "Parmatch.read_args"

let do_set_args ~erase_mutable q r = match q with
| {pat_desc = Tpat_tuple omegas} ->
    let args,rest = read_args omegas r in
    make_pat (Tpat_tuple args) q.pat_type q.pat_env::rest
| {pat_desc = Tpat_record (omegas,closed)} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_record
         (List.map2 (fun (lid, lbl,_) arg ->
           if
             erase_mutable &&
             (match lbl.lbl_mut with
             | Mutable -> true | Immutable -> false)
           then
             lid, lbl, omega
           else
             lid, lbl, arg)
            omegas args, closed))
      q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_construct (lid, c,omegas)} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_construct (lid, c,args))
      q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_variant (l, omega, row)} ->
    let arg, rest =
      match omega, r with
        Some _, a::r -> Some a, r
      | None, r -> None, r
      | _ -> assert false
    in
    make_pat
      (Tpat_variant (l, arg, row)) q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_lazy _omega} ->
    begin match r with
      arg::rest ->
        make_pat (Tpat_lazy arg) q.pat_type q.pat_env::rest
    | _ -> fatal_error "Parmatch.do_set_args (lazy)"
    end
| {pat_desc = Tpat_array omegas} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_array args) q.pat_type q.pat_env::
    rest
| {pat_desc=Tpat_constant _|Tpat_any} ->
    q::r (* case any is used in matching.ml *)
| _ -> fatal_error "Parmatch.set_args"

let set_args q r = do_set_args ~erase_mutable:false q r
and set_args_erase_mutable q r = do_set_args ~erase_mutable:true q r

(* Given a matrix of non-empty rows
   p1 :: r1...
   p2 :: r2...
   p3 :: r3...

   Simplify the first column [p1 p2 p3] by splitting all or-patterns.
   The result is a list of pairs
     ((pattern head, arguments), rest of row)

   For example,
     x :: r1
     (Some _) as y :: r2
     (None as x) as y :: r3
     (Some x | (None as x)) :: r4
   becomes
     ((   _ , [ ] ), r1)
     (( Some, [_] ), r2)
     (( None, [ ] ), r3)
     (( Some, [x] ), r4)
     (( None, [ ] ), r4)
 *)
let simplify_head_pat ~add_column p ps k =
  let rec simplify_head_pat p ps k =
    match p.pat_desc with
    | Tpat_alias (p,_,_) ->
        (* We have to handle aliases here, because there can be or-patterns
           underneath, that [Pattern_head.deconstruct] won't handle. *)
        simplify_head_pat p ps k
    | Tpat_or (p1,p2,_) -> simplify_head_pat p1 ps (simplify_head_pat p2 ps k)
    | _ -> add_column (Pattern_head.deconstruct p) ps k
  in simplify_head_pat p ps k

let rec simplify_first_col = function
  | [] -> []
  | [] :: _ -> assert false (* the rows are non-empty! *)
  | (p::ps) :: rows ->
      let add_column p ps k = (p, ps) :: k in
      simplify_head_pat ~add_column p ps (simplify_first_col rows)


(* Builds the specialized matrix of [pss] according to the discriminating
   pattern head [d].
   See section 3.1 of http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

   NOTES:
   - we are polymorphic on the type of matrices we work on, in particular a row
   might not simply be a [pattern list]. That's why we have the [extend_row]
   parameter.
*)
let build_specialized_submatrix ~extend_row discr pss =
  let rec filter_rec = function
    | ((head, args), ps) :: pss ->
        if simple_match discr head
        then extend_row (simple_match_args discr head args) ps :: filter_rec pss
        else filter_rec pss
    | _ -> [] in
  filter_rec pss

(* The "default" and "specialized" matrices of a given matrix.
   See section 3.1 of http://moscova.inria.fr/~maranget/papers/warn/warn.pdf .
*)
type 'matrix specialized_matrices = {
  default : 'matrix;
  constrs : (Pattern_head.t * 'matrix) list;
}

(* Consider a pattern matrix whose first column has been simplified
   to contain only _ or a head constructor
     | p1, r1...
     | p2, r2...
     | p3, r3...
     | ...

   We split this matrix into a list of /specialized/ sub-matrices, one for
   each head constructor appearing in the first column. For each row whose
   first column starts with a head constructor, remove this head
   column, prepend one column for each argument of the constructor,
   and add the resulting row in the sub-matrix corresponding to this
   head constructor.

   Rows whose left column is omega (the Any pattern _) may match any
   head constructor, so they are added to all sub-matrices.

   In the case where all the rows in the matrix have an omega on their first
   column, then there is only one /specialized/ sub-matrix, formed of all these
   omega rows.
   This matrix is also called the /default/ matrix.

   See the documentation of [build_specialized_submatrix] for an explanation of
   the [extend_row] parameter.
*)
let build_specialized_submatrices ~extend_row discr rows =
  let extend_group discr p args r rs =
    let r = extend_row (simple_match_args discr p args) r in
    (discr, r :: rs)
  in

  (* insert a row of head [p] and rest [r] into the right group *)
  let rec insert_constr head args r = function
    | [] ->
      (* if no group matched this row, it has a head constructor that
         was never seen before; add a new sub-matrix for this head *)
      [extend_group head head args r []]
    | (q0,rs) as bd::env ->
      if simple_match q0 head
      then extend_group q0 head args r rs :: env
      else bd :: insert_constr head args r env
  in

  (* insert a row of head omega into all groups *)
  let insert_omega r env =
    List.map (fun (q0,rs) -> extend_group q0 Pattern_head.omega [] r rs) env
  in

  let rec form_groups constr_groups omega_tails = function
    | [] -> (constr_groups, omega_tails)
    | ((head, args), tail) :: rest ->
        match Pattern_head.desc head with
        | Any ->
            (* note that calling insert_omega here would be wrong
               as some groups may not have been formed yet, if the
               first row with this head pattern comes after in the list *)
            form_groups constr_groups (tail :: omega_tails) rest
        | _ ->
            form_groups
              (insert_constr head args tail constr_groups) omega_tails rest
  in

  let constr_groups, omega_tails =
    let initial_constr_group =
      match Pattern_head.desc discr with
      | Record _ | Tuple _ | Lazy ->
        (* [discr] comes from [discr_pat], and in this case subsumes any of the
           patterns we could find on the first column of [rows]. So it is better
           to use it for our initial environment than any of the normalized
           pattern we might obtain from the first column. *)
        [discr,[]]
      | _ -> []
    in
    form_groups initial_constr_group [] rows
  in
  {
    default = omega_tails;
    constrs =
      (* insert omega rows in all groups *)
      List.fold_right insert_omega omega_tails constr_groups;
  }

(* Variant related functions *)

let set_last a =
  let rec loop = function
    | [] -> assert false
    | [_] -> [a]
    | x::l -> x :: loop l
  in
  function
  | (_, []) -> (Pattern_head.deconstruct a, [])
  | (first, row) -> (first, loop row)

(* mark constructor lines for failure when they are incomplete *)
let mark_partial =
  let zero = make_pat (Tpat_constant (Const_int 0)) Ctype.none Env.empty in
  List.map (fun ((hp, _), _ as ps) ->
    match Pattern_head.desc hp with
    | Any -> ps
    | _ -> set_last zero ps
  )

let close_variant env row =
  let row = Btype.row_repr row in
  let nm =
    List.fold_left
      (fun nm (_tag,f) ->
        match Btype.row_field_repr f with
        | Reither(_, _, false, e) ->
            (* m=false means that this tag is not explicitly matched *)
            Btype.set_row_field e Rabsent;
            None
        | Rabsent | Reither (_, _, true, _) | Rpresent _ -> nm)
      row.row_name row.row_fields in
  if not row.row_closed || nm != row.row_name then begin
    (* this unification cannot fail *)
    Ctype.unify env row.row_more
      (Btype.newgenty
         (Tvariant {row with row_fields = []; row_more = Btype.newgenvar();
                    row_closed = true; row_name = nm}))
  end

(*
  Check whether the first column of env makes up a complete signature or
  not. We work on the discriminating pattern heads of each sub-matrix: they
  are not omega/Any.
*)
let full_match closing env =  match env with
| [] -> false
| (discr, _) :: _ ->
  match Pattern_head.desc discr with
  | Any -> assert false
  | Construct { cstr_tag = Cstr_extension _ ; _ } -> false
  | Construct c -> List.length env = c.cstr_consts + c.cstr_nonconsts
  | Variant { type_row; _ } ->
      let fields =
        List.map
          (fun (d, _) ->
            match Pattern_head.desc d with
            | Variant { tag } -> tag
            | _ -> assert false)
          env
      in
      let row = type_row () in
      if closing && not (Btype.row_fixed row) then
        (* closing=true, we are considering the variant as closed *)
        List.for_all
          (fun (tag,f) ->
            match Btype.row_field_repr f with
              Rabsent | Reither(_, _, false, _) -> true
            | Reither (_, _, true, _)
                (* m=true, do not discard matched tags, rather warn *)
            | Rpresent _ -> List.mem tag fields)
          row.row_fields
      else
        row.row_closed &&
        List.for_all
          (fun (tag,f) ->
            Btype.row_field_repr f = Rabsent || List.mem tag fields)
          row.row_fields
  | Constant Const_char _ ->
      List.length env = 256
  | Constant _
  | Array _ -> false
  | Tuple _
  | Record _
  | Lazy -> true

(* Written as a non-fragile matching, PR#7451 originated from a fragile matching
   below. *)
let should_extend ext env = match ext with
| None -> false
| Some ext -> begin match env with
  | [] -> assert false
  | (p,_)::_ ->
      begin match Pattern_head.desc p with
      | Construct {cstr_tag=(Cstr_constant _|Cstr_block _|Cstr_unboxed)} ->
          let path =
            get_constructor_type_path (Pattern_head.typ p) (Pattern_head.env p)
          in
          Path.same path ext
      | Construct {cstr_tag=(Cstr_extension _)} -> false
      | Constant _ | Tuple _ | Variant _ | Record _ | Array _ | Lazy -> false
      | Any -> assert false
      end
end

module ConstructorTagHashtbl = Hashtbl.Make(
  struct
    type t = Types.constructor_tag
    let hash = Hashtbl.hash
    let equal = Types.equal_tag
  end
)

(* complement constructor tags *)
let complete_tags nconsts nconstrs tags =
  let seen_const = Array.make nconsts false
  and seen_constr = Array.make nconstrs false in
  List.iter
    (function
      | Cstr_constant i -> seen_const.(i) <- true
      | Cstr_block i -> seen_constr.(i) <- true
      | _  -> assert false)
    tags ;
  let r = ConstructorTagHashtbl.create (nconsts+nconstrs) in
  for i = 0 to nconsts-1 do
    if not seen_const.(i) then
      ConstructorTagHashtbl.add r (Cstr_constant i) ()
  done ;
  for i = 0 to nconstrs-1 do
    if not seen_constr.(i) then
      ConstructorTagHashtbl.add r (Cstr_block i) ()
  done ;
  r

(* build a pattern from a constructor description *)
let pat_of_constr ex_pat cstr =
  {ex_pat with pat_desc =
   Tpat_construct (mknoloc (Longident.Lident cstr.cstr_name),
                   cstr, omegas cstr.cstr_arity)}

let orify x y = make_pat (Tpat_or (x, y, None)) x.pat_type x.pat_env

let rec orify_many = function
| [] -> assert false
| [x] -> x
| x :: xs -> orify x (orify_many xs)

(* build an or-pattern from a constructor list *)
let pat_of_constrs ex_pat cstrs =
  let ex_pat = Pattern_head.to_omega_pattern ex_pat in
  if cstrs = [] then raise Empty else
  orify_many (List.map (pat_of_constr ex_pat) cstrs)

let pats_of_type ?(always=false) env ty =
  let ty' = Ctype.expand_head env ty in
  match ty'.desc with
  | Tconstr (path, _, _) ->
      begin try match (Env.find_type path env).type_kind with
      | Type_variant cl when always || List.length cl <= 1 ||
        (* Only explode when all constructors are GADTs *)
        List.for_all (fun cd -> cd.Types.cd_res <> None) cl ->
          let cstrs = fst (Env.find_type_descrs path env) in
          List.map (pat_of_constr (make_pat Tpat_any ty env)) cstrs
      | Type_record _ ->
          let labels = snd (Env.find_type_descrs path env) in
          let fields =
            List.map (fun ld ->
              mknoloc (Longident.Lident ld.lbl_name), ld, omega)
              labels
          in
          [make_pat (Tpat_record (fields, Closed)) ty env]
      | _ -> [omega]
      with Not_found -> [omega]
      end
  | Ttuple tl ->
      [make_pat (Tpat_tuple (omegas (List.length tl))) ty env]
  | _ -> [omega]

let rec get_variant_constructors env ty =
  match (Ctype.repr ty).desc with
  | Tconstr (path,_,_) -> begin
      try match Env.find_type path env with
      | {type_kind=Type_variant _} ->
          fst (Env.find_type_descrs path env)
      | {type_manifest = Some _} ->
          get_variant_constructors env
            (Ctype.expand_head_once env (clean_copy ty))
      | _ -> fatal_error "Parmatch.get_variant_constructors"
      with Not_found ->
        fatal_error "Parmatch.get_variant_constructors"
    end
  | _ -> fatal_error "Parmatch.get_variant_constructors"

(* Sends back a pattern that complements constructor tags all_tag *)
let complete_constrs p all_tags =
  let c = match Pattern_head.desc p with Construct c -> c | _ -> assert false in
  let not_tags = complete_tags c.cstr_consts c.cstr_nonconsts all_tags in
  let constrs = get_variant_constructors (Pattern_head.env p) c.cstr_res in
  let others =
    List.filter
      (fun cnstr -> ConstructorTagHashtbl.mem not_tags cnstr.cstr_tag)
      constrs in
  let const, nonconst =
    List.partition (fun cnstr -> cnstr.cstr_arity = 0) others in
  const @ nonconst

let build_other_constrs env p =
  match Pattern_head.desc p with
  | Construct { cstr_tag = Cstr_constant _ | Cstr_block _ } ->
      let get_tag q =
        match Pattern_head.desc q with
        | Construct c -> c.cstr_tag
        | _ -> fatal_error "Parmatch.get_tag" in
      let all_tags =  List.map (fun (p,_) -> get_tag p) env in
      pat_of_constrs p (complete_constrs p all_tags)
  | _ -> extra_pat

let complete_constrs p all_tags =
  (* This wrapper is here for [Matching], which (indirectly) calls this function
     from [combine_constructor], and nowhere else.
     So we know patterns have been fully simplified. *)
  complete_constrs (fst @@ Pattern_head.deconstruct p) all_tags

(* Auxiliary for build_other *)

let build_other_constant proj make first next p env =
  let all = List.map (fun (p, _) -> proj (Pattern_head.desc p)) env in
  let rec try_const i =
    if List.mem i all
    then try_const (next i)
    else make_pat (make i) (Pattern_head.typ p) (Pattern_head.env p)
  in try_const first

(*
  Builds a pattern that is incompatible with all patterns in
  the first column of env
*)

let some_private_tag = "<some private tag>"

let build_other ext env =
  match env with
  | [] -> omega
  | (d, _) :: _ ->
      match Pattern_head.desc d with
      | Construct { cstr_tag = Cstr_extension _ } ->
          (* let c = {c with cstr_name = "*extension*"} in *) (* PR#7330 *)
          make_pat
            (Tpat_var (Ident.create_local "*extension*",
                       {txt="*extension*"; loc = Pattern_head.loc d}))
            Ctype.none Env.empty
      | Construct _ ->
          begin match ext with
          | Some ext ->
              if Path.same ext
                   (get_constructor_type_path
                      (Pattern_head.typ d) (Pattern_head.env d))
              then
                extra_pat
              else
                build_other_constrs env d
          | _ ->
              build_other_constrs env d
          end
      | Variant { cstr_row; type_row } ->
          let tags =
            List.map
              (fun (d, _) ->
                match Pattern_head.desc d with
                | Variant { tag } -> tag
                | _ -> assert false)
              env
            in
            let make_other_pat tag const =
              let arg = if const then None else Some omega in
              make_pat (Tpat_variant(tag, arg, cstr_row))
                (Pattern_head.typ d) (Pattern_head.env d)
            in
            let row = type_row () in
            begin match
              List.fold_left
                (fun others (tag,f) ->
                  if List.mem tag tags then others else
                  match Btype.row_field_repr f with
                    Rabsent (* | Reither _ *) -> others
                  (* This one is called after erasing pattern info *)
                  | Reither (c, _, _, _) -> make_other_pat tag c :: others
                  | Rpresent arg -> make_other_pat tag (arg = None) :: others)
                [] row.row_fields
            with
              [] ->
                let tag =
                  if Btype.row_fixed row then some_private_tag else
                  let rec mktag tag =
                    if List.mem tag tags then mktag (tag ^ "'") else tag in
                  mktag "AnyOtherTag"
                in make_other_pat tag true
            | pat::other_pats ->
                List.fold_left
                  (fun p_res pat ->
                    make_pat (Tpat_or (pat, p_res, None))
                      (Pattern_head.typ d) (Pattern_head.env d))
                  pat other_pats
            end
      | Constant Const_char _ ->
          let all_chars =
            List.map
              (fun (p,_) -> match Pattern_head.desc p with
              | Constant (Const_char c) -> c
              | _ -> assert false)
              env
          in
          let rec find_other i imax =
            if i > imax then raise Not_found
            else
              let ci = Char.chr i in
              if List.mem ci all_chars then
                find_other (i+1) imax
              else
                make_pat (Tpat_constant (Const_char ci))
                  (Pattern_head.typ d) (Pattern_head.env d)
          in
          let rec try_chars = function
            | [] -> omega
            | (c1,c2) :: rest ->
                try
                  find_other (Char.code c1) (Char.code c2)
                with
                | Not_found -> try_chars rest
          in
          try_chars
            [ 'a', 'z' ; 'A', 'Z' ; '0', '9' ;
              ' ', '~' ; Char.chr 0 , Char.chr 255]
      | Constant Const_int _ ->
          build_other_constant
            (function Constant(Const_int i) -> i | _ -> assert false)
            (function i -> Tpat_constant(Const_int i))
            0 succ d env
      | Constant Const_int32 _ ->
          build_other_constant
            (function Constant(Const_int32 i) -> i | _ -> assert false)
            (function i -> Tpat_constant(Const_int32 i))
            0l Int32.succ d env
      | Constant Const_int64 _ ->
          build_other_constant
            (function Constant(Const_int64 i) -> i | _ -> assert false)
            (function i -> Tpat_constant(Const_int64 i))
            0L Int64.succ d env
      | Constant Const_nativeint _ ->
          build_other_constant
            (function Constant(Const_nativeint i) -> i | _ -> assert false)
            (function i -> Tpat_constant(Const_nativeint i))
            0n Nativeint.succ d env
      | Constant Const_string _ ->
          build_other_constant
            (function Constant(Const_string (s, _, _)) -> String.length s
                    | _ -> assert false)
            (function i ->
               Tpat_constant
                 (Const_string(String.make i '*',Location.none,None)))
            0 succ d env
      | Constant Const_float _ ->
          build_other_constant
            (function Constant(Const_float f) -> float_of_string f
                    | _ -> assert false)
            (function f -> Tpat_constant(Const_float (string_of_float f)))
            0.0 (fun f -> f +. 1.0) d env
      | Array _ ->
          let all_lengths =
            List.map
              (fun (p,_) -> match Pattern_head.desc p with
              | Array len -> len
              | _ -> assert false)
              env in
          let rec try_arrays l =
            if List.mem l all_lengths then try_arrays (l+1)
            else
              make_pat
                (Tpat_array (omegas l))
                (Pattern_head.typ d) (Pattern_head.env d) in
          try_arrays 0
      | _ -> omega

let rec has_instance p = match p.pat_desc with
  | Tpat_variant (l,_,r) when is_absent l r -> false
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> true
  | Tpat_alias (p,_,_) | Tpat_variant (_,Some p,_) -> has_instance p
  | Tpat_or (p1,p2,_) -> has_instance p1 || has_instance p2
  | Tpat_construct (_,_,ps) | Tpat_tuple ps | Tpat_array ps ->
      has_instances ps
  | Tpat_record (lps,_) -> has_instances (List.map (fun (_,_,x) -> x) lps)
  | Tpat_lazy p
    -> has_instance p

and has_instances = function
  | [] -> true
  | q::rem -> has_instance q && has_instances rem

(*
  Core function :
  Is the last row of pattern matrix pss + qs satisfiable ?
  That is :
    Does there exists at least one value vector, es such that :
     1- for all ps in pss ps # es (ps and es are not compatible)
     2- qs <= es                  (es matches qs)

   ---

   In two places in the following function, we check the coherence of the first
   column of (pss + qs).
   If it is incoherent, then we exit early saying that (pss + qs) is not
   satisfiable (which is equivalent to saying "oh, we shouldn't have considered
   that branch, no good result came come from here").

   But what happens if we have a coherent but ill-typed column?
   - we might end up returning [false], which is equivalent to noticing the
   incompatibility: clearly this is fine.
   - if we end up returning [true] then we're saying that [qs] is useful while
   it is not. This is sad but not the end of the world, we're just allowing dead
   code to survive.
*)
let rec satisfiable pss qs = match pss with
| [] -> has_instances qs
| _  ->
    match qs with
    | [] -> false
    | {pat_desc = Tpat_or(q1,q2,_)}::qs ->
        satisfiable pss (q1::qs) || satisfiable pss (q2::qs)
    | {pat_desc = Tpat_alias(q,_,_)}::qs ->
          satisfiable pss (q::qs)
    | {pat_desc = (Tpat_any | Tpat_var(_))}::qs ->
        let pss = simplify_first_col pss in
        if not (all_coherent (first_column pss)) then
          false
        else begin
          let { default; constrs } =
            let q0 = discr_pat omega pss in
            build_specialized_submatrices ~extend_row:(@) q0 pss in
          if not (full_match false constrs) then
            satisfiable default qs
          else
            List.exists
              (fun (p,pss) ->
                 not (is_absent_pat p) &&
                 satisfiable pss
                   (simple_match_args p Pattern_head.omega [] @ qs))
              constrs
        end
    | {pat_desc=Tpat_variant (l,_,r)}::_ when is_absent l r -> false
    | q::qs ->
        let pss = simplify_first_col pss in
        let hq, qargs = Pattern_head.deconstruct q in
        if not (all_coherent (hq :: first_column pss)) then
          false
        else begin
          let q0 = discr_pat q pss in
          satisfiable (build_specialized_submatrix ~extend_row:(@) q0 pss)
            (simple_match_args q0 hq qargs @ qs)
        end

(* While [satisfiable] only checks whether the last row of [pss + qs] is
   satisfiable, this function returns the (possibly empty) list of vectors [es]
   which verify:
     1- for all ps in pss, ps # es (ps and es are not compatible)
     2- qs <= es                   (es matches qs)

   This is done to enable GADT handling

   For considerations regarding the coherence check, see the comment on
   [satisfiable] above.  *)
let rec list_satisfying_vectors pss qs =
  match pss with
  | [] -> if has_instances qs then [qs] else []
  | _  ->
      match qs with
      | [] -> []
      | {pat_desc = Tpat_or(q1,q2,_)}::qs ->
          list_satisfying_vectors pss (q1::qs) @
          list_satisfying_vectors pss (q2::qs)
      | {pat_desc = Tpat_alias(q,_,_)}::qs ->
          list_satisfying_vectors pss (q::qs)
      | {pat_desc = (Tpat_any | Tpat_var(_))}::qs ->
          let pss = simplify_first_col pss in
          if not (all_coherent (first_column pss)) then
            []
          else begin
            let q0 = discr_pat omega pss in
            let wild default_matrix p =
              List.map (fun qs -> p::qs)
                (list_satisfying_vectors default_matrix qs)
            in
            match build_specialized_submatrices ~extend_row:(@) q0 pss with
            | { default; constrs = [] } ->
                (* first column of pss is made of variables only *)
                wild default omega
            | { default; constrs = ((p,_)::_ as constrs) } ->
                let for_constrs () =
                  List.flatten (
                    List.map (fun (p,pss) ->
                      if is_absent_pat p then
                        []
                      else
                        let witnesses =
                          list_satisfying_vectors pss
                            (simple_match_args p Pattern_head.omega [] @ qs)
                        in
                        let p = Pattern_head.to_omega_pattern p in
                        List.map (set_args p) witnesses
                    ) constrs
                  )
                in
                if full_match false constrs then for_constrs () else
                begin match Pattern_head.desc p with
                | Construct _ ->
                    (* activate this code for checking non-gadt constructors *)
                    wild default (build_other_constrs constrs p)
                    @ for_constrs ()
                | _ ->
                    wild default omega
                end
          end
      | {pat_desc=Tpat_variant (l,_,r)}::_ when is_absent l r -> []
      | q::qs ->
          let hq, qargs = Pattern_head.deconstruct q in
          let pss = simplify_first_col pss in
          if not (all_coherent (hq :: first_column pss)) then
            []
          else begin
            let q0 = discr_pat q pss in
            List.map (set_args (Pattern_head.to_omega_pattern q0))
              (list_satisfying_vectors
                 (build_specialized_submatrix ~extend_row:(@) q0 pss)
                 (simple_match_args q0 hq qargs @ qs))
          end

(******************************************)
(* Look for a row that matches some value *)
(******************************************)

(*
  Useful for seeing if the example of
  non-matched value can indeed be matched
  (by a guarded clause)
*)

let rec do_match pss qs = match qs with
| [] ->
    begin match pss  with
    | []::_ -> true
    | _ -> false
    end
| q::qs -> match q with
  | {pat_desc = Tpat_or (q1,q2,_)} ->
      do_match pss (q1::qs) || do_match pss (q2::qs)
  | {pat_desc = Tpat_any} ->
      let rec remove_first_column = function
        | (_::ps)::rem -> ps::remove_first_column rem
        | _ -> []
      in
      do_match (remove_first_column pss) qs
  | _ ->
      (* [q] is generated by us, it doesn't come from the source. So we know
         it's not of the form [P as name].
         Therefore there is no risk of [deconstruct] raising. *)
      let q0, qargs = Pattern_head.deconstruct q in
      let pss = simplify_first_col pss in
      (* [pss] will (or won't) match [q0 :: qs] regardless of the coherence of
         its first column. *)
      do_match
        (build_specialized_submatrix ~extend_row:(@) q0 pss)
        (qargs @ qs)


type 'a exhaust_result =
  | No_matching_value
  | Witnesses of 'a list

let rappend r1 r2 =
  match r1, r2 with
  | No_matching_value, _ -> r2
  | _, No_matching_value -> r1
  | Witnesses l1, Witnesses l2 -> Witnesses (l1 @ l2)

let rec try_many  f = function
  | [] -> No_matching_value
  | (p,pss)::rest ->
      rappend (f (p, pss)) (try_many f rest)

(*
let print_pat pat =
  let rec string_of_pat pat =
    match pat.pat_desc with
        Tpat_var _ -> "v"
      | Tpat_any -> "_"
      | Tpat_alias (p, x) -> Printf.sprintf "(%s) as ?"  (string_of_pat p)
      | Tpat_constant n -> "0"
      | Tpat_construct (_, lid, _) ->
        Printf.sprintf "%s" (String.concat "." (Longident.flatten lid.txt))
      | Tpat_lazy p ->
        Printf.sprintf "(lazy %s)" (string_of_pat p)
      | Tpat_or (p1,p2,_) ->
        Printf.sprintf "(%s | %s)" (string_of_pat p1) (string_of_pat p2)
      | Tpat_tuple list ->
        Printf.sprintf "(%s)" (String.concat "," (List.map string_of_pat list))
      | Tpat_variant (_, _, _) -> "variant"
      | Tpat_record (_, _) -> "record"
      | Tpat_array _ -> "array"
  in
  Printf.fprintf stderr "PAT[%s]\n%!" (string_of_pat pat)
*)

(*
  Now another satisfiable function that additionally
  supplies an example of a matching value.

  This function should be called for exhaustiveness check only.
*)
let rec exhaust (ext:Path.t option) pss n = match pss with
| []    ->  Witnesses [omegas n]
| []::_ ->  No_matching_value
| pss   ->
    let pss = simplify_first_col pss in
    if not (all_coherent (first_column pss)) then
      (* We're considering an ill-typed branch, we won't actually be able to
         produce a well typed value taking that branch. *)
      No_matching_value
    else begin
      (* Assuming the first column is ill-typed but considered coherent, we
         might end up producing an ill-typed witness of non-exhaustivity
         corresponding to the current branch.

         If [exhaust] has been called by [do_check_partial], then the witnesses
         produced get typechecked and the ill-typed ones are discarded.

         If [exhaust] has been called by [do_check_fragile], then it is possible
         we might fail to warn the user that the matching is fragile. See for
         example testsuite/tests/warnings/w04_failure.ml. *)
      let q0 = discr_pat omega pss in
      match build_specialized_submatrices ~extend_row:(@) q0 pss with
      | { default; constrs = [] } ->
          (* first column of pss is made of variables only *)
          begin match exhaust ext default (n-1) with
          | Witnesses r ->
              let q0 = Pattern_head.to_omega_pattern q0 in
              Witnesses (List.map (fun row -> q0::row) r)
          | r -> r
        end
      | { default; constrs } ->
          let try_non_omega (p,pss) =
            if is_absent_pat p then
              No_matching_value
            else
              match
                exhaust
                  ext pss
                  (List.length (simple_match_args p Pattern_head.omega [])
                   + n - 1)
              with
              | Witnesses r ->
                  let p = Pattern_head.to_omega_pattern p in
                  Witnesses (List.map (set_args p) r)
              | r       -> r in
          let before = try_many try_non_omega constrs in
          if
            full_match false constrs && not (should_extend ext constrs)
          then
            before
          else
            let r =  exhaust ext default (n-1) in
            match r with
            | No_matching_value -> before
            | Witnesses r ->
                try
                  let p = build_other ext constrs in
                  let dug = List.map (fun tail -> p :: tail) r in
                  match before with
                  | No_matching_value -> Witnesses dug
                  | Witnesses x -> Witnesses (x @ dug)
                with
        (* cannot occur, since constructors don't make a full signature *)
                | Empty -> fatal_error "Parmatch.exhaust"
  end

let exhaust ext pss n =
  let ret = exhaust ext pss n in
  match ret with
    No_matching_value -> No_matching_value
  | Witnesses lst ->
      let singletons =
        List.map
          (function
              [x] -> x
            | _ -> assert false)
          lst
      in
      Witnesses [orify_many singletons]

(*
   Another exhaustiveness check, enforcing variant typing.
   Note that it does not check exact exhaustiveness, but whether a
   matching could be made exhaustive by closing all variant types.
   When this is true of all other columns, the current column is left
   open (even if it means that the whole matching is not exhaustive as
   a result).
   When this is false for the matrix minus the current column, and the
   current column is composed of variant tags, we close the variant
   (even if it doesn't help in making the matching exhaustive).
*)

let rec pressure_variants tdefs = function
  | []    -> false
  | []::_ -> true
  | pss   ->
      let pss = simplify_first_col pss in
      if not (all_coherent (first_column pss)) then
        true
      else begin
        let q0 = discr_pat omega pss in
        match build_specialized_submatrices ~extend_row:(@) q0 pss with
        | { default; constrs = [] } -> pressure_variants tdefs default
        | { default; constrs } ->
            let rec try_non_omega = function
              | (_p,pss) :: rem ->
                  let ok = pressure_variants tdefs pss in
                  (* The order below matters : we want [pressure_variants] to be
                    called on all the specialized submatrices because we might
                    close some variant in any of them regardless of whether [ok]
                    is true for [pss] or not *)
                  try_non_omega rem && ok
              | [] -> true
            in
            if full_match (tdefs=None) constrs then
              try_non_omega constrs
            else if tdefs = None then
              pressure_variants None default
            else
              let full = full_match true constrs in
              let ok =
                if full then
                  try_non_omega constrs
                else begin
                  let { constrs = partial_constrs; _ } =
                    build_specialized_submatrices ~extend_row:(@) q0
                      (mark_partial pss)
                  in
                  try_non_omega partial_constrs
                end
              in
              begin match constrs, tdefs with
              | [], _
              | _, None -> ()
              | (d, _) :: _, Some env ->
                match Pattern_head.desc d with
                | Variant { type_row; _ } ->
                  let row = type_row () in
                  if Btype.row_fixed row
                  || pressure_variants None default then ()
                  else close_variant env row
                | _ -> ()
              end;
              ok
      end


(* Yet another satisfiable function *)

(*
   This time every_satisfiable pss qs checks the
   utility of every expansion of qs.
   Expansion means expansion of or-patterns inside qs
*)

type answer =
  | Used                                (* Useful pattern *)
  | Unused                              (* Useless pattern *)
  | Upartial of Typedtree.pattern list  (* Mixed, with list of useless ones *)



(* this row type enable column processing inside the matrix
    - left  ->  elements not to be processed,
    - right ->  elements to be processed
*)
type usefulness_row =
  {no_ors : pattern list ; ors : pattern list ; active : pattern list}

(*
let pretty_row {ors=ors ; no_ors=no_ors; active=active} =
  pretty_line ors ; prerr_string " *" ;
  pretty_line no_ors ; prerr_string " *" ;
  pretty_line active

let pretty_rows rs =
  prerr_endline "begin matrix" ;
  List.iter
    (fun r ->
      pretty_row r ;
      prerr_endline "")
    rs ;
  prerr_endline "end matrix"
*)

(* Initial build *)
let make_row ps = {ors=[] ; no_ors=[]; active=ps}

let make_rows pss = List.map make_row pss


(* Useful to detect and expand  or pats inside as pats *)
let rec unalias p = match p.pat_desc with
| Tpat_alias (p,_,_) -> unalias p
| _ -> p


let is_var p = match (unalias p).pat_desc with
| Tpat_any|Tpat_var _ -> true
| _                   -> false

let is_var_column rs =
  List.for_all
    (fun r -> match r.active with
    | p::_ -> is_var p
    | []   -> assert false)
    rs

(* Standard or-args for left-to-right matching *)
let rec or_args p = match p.pat_desc with
| Tpat_or (p1,p2,_) -> p1,p2
| Tpat_alias (p,_,_)  -> or_args p
| _                 -> assert false

(* Just remove current column *)
let remove r = match r.active with
| _::rem -> {r with active=rem}
| []     -> assert false

let remove_column rs = List.map remove rs

(* Current column has been processed *)
let push_no_or r = match r.active with
| p::rem -> { r with no_ors = p::r.no_ors ; active=rem}
| [] -> assert false

let push_or r = match r.active with
| p::rem -> { r with ors = p::r.ors ; active=rem}
| [] -> assert false

let push_or_column rs = List.map push_or rs
and push_no_or_column rs = List.map push_no_or rs

let rec simplify_first_usefulness_col = function
  | [] -> []
  | row :: rows ->
    match row.active with
    | [] -> assert false (* the rows are non-empty! *)
    | p :: ps ->
      let add_column p ps k =
        (p, { row with active = ps }) :: k in
      simplify_head_pat ~add_column p ps
        (simplify_first_usefulness_col rows)

(* Back to normal matrices *)
let make_vector r = List.rev r.no_ors

let make_matrix rs = List.map make_vector rs


(* Standard union on answers *)
let union_res r1 r2 = match r1, r2 with
| (Unused,_)
| (_, Unused) -> Unused
| Used,_    -> r2
| _, Used   -> r1
| Upartial u1, Upartial u2 -> Upartial (u1@u2)

(* propose or pats for expansion *)
let extract_elements qs =
  let rec do_rec seen = function
    | [] -> []
    | q::rem ->
        {no_ors= List.rev_append seen rem @ qs.no_ors ;
        ors=[] ;
        active = [q]}::
        do_rec (q::seen) rem in
  do_rec [] qs.ors

(* idem for matrices *)
let transpose rs = match rs with
| [] -> assert false
| r::rem ->
    let i = List.map (fun x -> [x]) r in
    List.fold_left
      (List.map2 (fun r x -> x::r))
      i rem

let extract_columns pss qs = match pss with
| [] -> List.map (fun _ -> []) qs.ors
| _  ->
  let rows = List.map extract_elements pss in
  transpose rows

(* Core function
   The idea is to first look for or patterns (recursive case), then
   check or-patterns argument usefulness (terminal case)
*)

let rec every_satisfiables pss qs = match qs.active with
| []     ->
    (* qs is now partitionned,  check usefulness *)
    begin match qs.ors with
    | [] -> (* no or-patterns *)
        if satisfiable (make_matrix pss) (make_vector qs) then
          Used
        else
          Unused
    | _  -> (* n or-patterns -> 2n expansions *)
        List.fold_right2
          (fun pss qs r -> match r with
          | Unused -> Unused
          | _ ->
              match qs.active with
              | [q] ->
                  let q1,q2 = or_args q in
                  let r_loc = every_both pss qs q1 q2 in
                  union_res r r_loc
              | _   -> assert false)
          (extract_columns pss qs) (extract_elements qs)
          Used
    end
| q::rem ->
    let uq = unalias q in
    begin match uq.pat_desc with
    | Tpat_any | Tpat_var _ ->
        if is_var_column pss then
(* forget about ``all-variable''  columns now *)
          every_satisfiables (remove_column pss) (remove qs)
        else
(* otherwise this is direct food for satisfiable *)
          every_satisfiables (push_no_or_column pss) (push_no_or qs)
    | Tpat_or (q1,q2,_) ->
        if
          q1.pat_loc.Location.loc_ghost &&
          q2.pat_loc.Location.loc_ghost
        then
(* syntactically generated or-pats should not be expanded *)
          every_satisfiables (push_no_or_column pss) (push_no_or qs)
        else
(* this is a real or-pattern *)
          every_satisfiables (push_or_column pss) (push_or qs)
    | Tpat_variant (l,_,r) when is_absent l r -> (* Ah Jacques... *)
        Unused
    | _ ->
(* standard case, filter matrix *)
        let pss = simplify_first_usefulness_col pss in
        let huq, args = Pattern_head.deconstruct uq in
        (* The handling of incoherent matrices is kept in line with
           [satisfiable] *)
        if not (all_coherent (huq :: first_column pss)) then
          Unused
        else begin
          let q0 = discr_pat q pss in
          every_satisfiables
            (build_specialized_submatrix q0 pss
              ~extend_row:(fun ps r -> { r with active = ps @ r.active }))
            {qs with active=simple_match_args q0 huq args @ rem}
        end
    end

(*
  This function ``every_both'' performs the usefulness check
  of or-pat q1|q2.
  The trick is to call every_satisfied twice with
  current active columns restricted to q1 and q2,
  That way,
  - others orpats in qs.ors will not get expanded.
  - all matching work performed on qs.no_ors is not performed again.
  *)
and every_both pss qs q1 q2 =
  let qs1 = {qs with active=[q1]}
  and qs2 =  {qs with active=[q2]} in
  let r1 = every_satisfiables pss qs1
  and r2 =  every_satisfiables (if compat q1 q2 then qs1::pss else pss) qs2 in
  match r1 with
  | Unused ->
      begin match r2 with
      | Unused -> Unused
      | Used   -> Upartial [q1]
      | Upartial u2 -> Upartial (q1::u2)
      end
  | Used ->
      begin match r2 with
      | Unused -> Upartial [q2]
      | _      -> r2
      end
  | Upartial u1 ->
      begin match r2 with
      | Unused -> Upartial (u1@[q2])
      | Used   -> r1
      | Upartial u2 -> Upartial (u1 @ u2)
      end




(* le_pat p q  means, forall V,  V matches q implies V matches p *)
let rec le_pat p q =
  match (p.pat_desc, q.pat_desc) with
  | (Tpat_var _|Tpat_any),_ -> true
  | Tpat_alias(p,_,_), _ -> le_pat p q
  | _, Tpat_alias(q,_,_) -> le_pat p q
  | Tpat_constant(c1), Tpat_constant(c2) -> const_compare c1 c2 = 0
  | Tpat_construct(_,c1,ps), Tpat_construct(_,c2,qs) ->
      Types.equal_tag c1.cstr_tag c2.cstr_tag && le_pats ps qs
  | Tpat_variant(l1,Some p1,_), Tpat_variant(l2,Some p2,_) ->
      (l1 = l2 && le_pat p1 p2)
  | Tpat_variant(l1,None,_r1), Tpat_variant(l2,None,_) ->
      l1 = l2
  | Tpat_variant(_,_,_), Tpat_variant(_,_,_) -> false
  | Tpat_tuple(ps), Tpat_tuple(qs) -> le_pats ps qs
  | Tpat_lazy p, Tpat_lazy q -> le_pat p q
  | Tpat_record (l1,_), Tpat_record (l2,_) ->
      let ps,qs = records_args l1 l2 in
      le_pats ps qs
  | Tpat_array(ps), Tpat_array(qs) ->
      List.length ps = List.length qs && le_pats ps qs
(* In all other cases, enumeration is performed *)
  | _,_  -> not (satisfiable [[p]] [q])

and le_pats ps qs =
  match ps,qs with
    p::ps, q::qs -> le_pat p q && le_pats ps qs
  | _, _         -> true

let get_mins le ps =
  let rec select_rec r = function
      [] -> r
    | p::ps ->
        if List.exists (fun p0 -> le p0 p) ps
        then select_rec r ps
        else select_rec (p::r) ps in
  select_rec [] (select_rec [] ps)

(*
  lub p q is a pattern that matches all values matched by p and q
  may raise Empty, when p and q are not compatible
*)

let rec lub p q = match p.pat_desc,q.pat_desc with
| Tpat_alias (p,_,_),_      -> lub p q
| _,Tpat_alias (q,_,_)      -> lub p q
| (Tpat_any|Tpat_var _),_ -> q
| _,(Tpat_any|Tpat_var _) -> p
| Tpat_or (p1,p2,_),_     -> orlub p1 p2 q
| _,Tpat_or (q1,q2,_)     -> orlub q1 q2 p (* Thanks god, lub is commutative *)
| Tpat_constant c1, Tpat_constant c2 when const_compare c1 c2 = 0 -> p
| Tpat_tuple ps, Tpat_tuple qs ->
    let rs = lubs ps qs in
    make_pat (Tpat_tuple rs) p.pat_type p.pat_env
| Tpat_lazy p, Tpat_lazy q ->
    let r = lub p q in
    make_pat (Tpat_lazy r) p.pat_type p.pat_env
| Tpat_construct (lid, c1,ps1), Tpat_construct (_,c2,ps2)
      when  Types.equal_tag c1.cstr_tag c2.cstr_tag  ->
        let rs = lubs ps1 ps2 in
        make_pat (Tpat_construct (lid, c1,rs))
          p.pat_type p.pat_env
| Tpat_variant(l1,Some p1,row), Tpat_variant(l2,Some p2,_)
          when  l1=l2 ->
            let r=lub p1 p2 in
            make_pat (Tpat_variant (l1,Some r,row)) p.pat_type p.pat_env
| Tpat_variant (l1,None,_row), Tpat_variant(l2,None,_)
              when l1 = l2 -> p
| Tpat_record (l1,closed),Tpat_record (l2,_) ->
    let rs = record_lubs l1 l2 in
    make_pat (Tpat_record (rs, closed)) p.pat_type p.pat_env
| Tpat_array ps, Tpat_array qs
      when List.length ps = List.length qs ->
        let rs = lubs ps qs in
        make_pat (Tpat_array rs) p.pat_type p.pat_env
| _,_  ->
    raise Empty

and orlub p1 p2 q =
  try
    let r1 = lub p1 q in
    try
      {q with pat_desc=(Tpat_or (r1,lub p2 q,None))}
  with
  | Empty -> r1
with
| Empty -> lub p2 q

and record_lubs l1 l2 =
  let rec lub_rec l1 l2 = match l1,l2 with
  | [],_ -> l2
  | _,[] -> l1
  | (lid1, lbl1,p1)::rem1, (lid2, lbl2,p2)::rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        (lid1, lbl1,p1)::lub_rec rem1 l2
      else if lbl2.lbl_pos < lbl1.lbl_pos  then
        (lid2, lbl2,p2)::lub_rec l1 rem2
      else
        (lid1, lbl1,lub p1 p2)::lub_rec rem1 rem2 in
  lub_rec l1 l2

and lubs ps qs = match ps,qs with
| p::ps, q::qs -> lub p q :: lubs ps qs
| _,_ -> []


(******************************)
(* Exported variant closing   *)
(******************************)

(* Apply pressure to variants *)

let pressure_variants tdefs patl =
  ignore (pressure_variants
            (Some tdefs)
            (List.map (fun p -> [p; omega]) patl))

let pressure_variants_in_computation_pattern tdefs patl =
  let add_row pss p_opt =
    match p_opt with
    | None -> pss
    | Some p -> p :: pss
  in
  let val_pss, exn_pss =
    List.fold_right (fun pat (vpss, epss)->
      let (vp, ep) = split_pattern pat in
      add_row vpss vp, add_row epss ep
    ) patl ([], [])
  in
  pressure_variants tdefs val_pss;
  pressure_variants tdefs exn_pss

(*****************************)
(* Utilities for diagnostics *)
(*****************************)

(*
  Build up a working pattern matrix by forgetting
  about guarded patterns
*)

let rec initial_matrix = function
    [] -> []
  | {c_guard=Some _} :: rem -> initial_matrix rem
  | {c_guard=None; c_lhs=p} :: rem -> [p] :: initial_matrix rem

(*
   Build up a working pattern matrix by keeping
   only the patterns which are guarded
*)
let rec initial_only_guarded = function
  | [] -> []
  | { c_guard = None; _} :: rem ->
      initial_only_guarded rem
  | { c_lhs = pat; _ } :: rem ->
      [pat] :: initial_only_guarded rem


(************************)
(* Exhaustiveness check *)
(************************)

(* conversion from Typedtree.pattern to Parsetree.pattern list *)
module Conv = struct
  open Parsetree
  let mkpat desc = Ast_helper.Pat.mk desc

  let name_counter = ref 0
  let fresh name =
    let current = !name_counter in
    name_counter := !name_counter + 1;
    "#$" ^ name ^ Int.to_string current

  let conv typed =
    let constrs = Hashtbl.create 7 in
    let labels = Hashtbl.create 7 in
    let rec loop pat =
      match pat.pat_desc with
        Tpat_or (pa,pb,_) ->
          mkpat (Ppat_or (loop pa, loop pb))
      | Tpat_var (_, ({txt="*extension*"} as nm)) -> (* PR#7330 *)
          mkpat (Ppat_var nm)
      | Tpat_any
      | Tpat_var _ ->
          mkpat Ppat_any
      | Tpat_constant c ->
          mkpat (Ppat_constant (Untypeast.constant c))
      | Tpat_alias (p,_,_) -> loop p
      | Tpat_tuple lst ->
          mkpat (Ppat_tuple (List.map loop lst))
      | Tpat_construct (cstr_lid, cstr, lst) ->
          let id = fresh cstr.cstr_name in
          let lid = { cstr_lid with txt = Longident.Lident id } in
          Hashtbl.add constrs id cstr;
          let arg =
            match List.map loop lst with
            | []  -> None
            | [p] -> Some p
            | lst -> Some (mkpat (Ppat_tuple lst))
          in
          mkpat (Ppat_construct(lid, arg))
      | Tpat_variant(label,p_opt,_row_desc) ->
          let arg = Option.map loop p_opt in
          mkpat (Ppat_variant(label, arg))
      | Tpat_record (subpatterns, _closed_flag) ->
          let fields =
            List.map
              (fun (_, lbl, p) ->
                let id = fresh lbl.lbl_name in
                Hashtbl.add labels id lbl;
                (mknoloc (Longident.Lident id), loop p))
              subpatterns
          in
          mkpat (Ppat_record (fields, Open))
      | Tpat_array lst ->
          mkpat (Ppat_array (List.map loop lst))
      | Tpat_lazy p ->
          mkpat (Ppat_lazy (loop p))
    in
    let ps = loop typed in
    (ps, constrs, labels)
end


(* Whether the counter-example contains an extension pattern *)
let contains_extension pat =
  exists_pattern
    (function
     | {pat_desc=Tpat_var (_, {txt="*extension*"})} -> true
     | _ -> false)
    pat

(* Build a pattern from its expected type *)
type pat_explosion = PE_single | PE_gadt_cases
type ppat_of_type =
  | PT_empty
  | PT_any
  | PT_pattern of
      pat_explosion *
      Parsetree.pattern *
      (string, constructor_description) Hashtbl.t *
      (string, label_description) Hashtbl.t

let ppat_of_type env ty =
  match pats_of_type env ty with
  | [] -> PT_empty
  | [{pat_desc = Tpat_any}] -> PT_any
  | [pat] ->
      let (ppat, constrs, labels) = Conv.conv pat in
      PT_pattern (PE_single, ppat, constrs, labels)
  | pats ->
      let (ppat, constrs, labels) = Conv.conv (orify_many pats) in
      PT_pattern (PE_gadt_cases, ppat, constrs, labels)

let do_check_partial ~pred loc casel pss = match pss with
| [] ->
        (*
          This can occur
          - For empty matches generated by ocamlp4 (no warning)
          - when all patterns have guards (then, casel <> [])
          (specific warning)
          Then match MUST be considered non-exhaustive,
          otherwise compilation of PM is broken.
          *)
    begin match casel with
    | [] -> ()
    | _  ->
      if Warnings.is_active Warnings.All_clauses_guarded then
        Location.prerr_warning loc Warnings.All_clauses_guarded
    end ;
    Partial
| ps::_  ->
    begin match exhaust None pss (List.length ps) with
    | No_matching_value -> Total
    | Witnesses [u] ->
        let v =
          let (pattern,constrs,labels) = Conv.conv u in
          let u' = pred constrs labels pattern in
          (* pretty_pat u;
          begin match u' with
            None -> prerr_endline ": impossible"
          | Some _ -> prerr_endline ": possible"
          end; *)
          u'
        in
        begin match v with
          None -> Total
        | Some v ->
            if Warnings.is_active (Warnings.Partial_match "") then begin
              let errmsg =
                try
                  let buf = Buffer.create 16 in
                  let fmt = Format.formatter_of_buffer buf in
                  Printpat.top_pretty fmt v;
                  if do_match (initial_only_guarded casel) [v] then
                    Buffer.add_string buf
                      "\n(However, some guarded clause may match this value.)";
                  if contains_extension v then
                    Buffer.add_string buf
                      "\nMatching over values of extensible variant types \
                         (the *extension* above)\n\
                    must include a wild card pattern in order to be exhaustive."
                  ;
                  Buffer.contents buf
                with _ ->
                  ""
              in
                Location.prerr_warning loc (Warnings.Partial_match errmsg)
            end;
            Partial
        end
    | _ ->
        fatal_error "Parmatch.check_partial"
    end

(*****************)
(* Fragile check *)
(*****************)

(* Collect all data types in a pattern *)

let rec add_path path = function
  | [] -> [path]
  | x::rem as paths ->
      if Path.same path x then paths
      else x::add_path path rem

let extendable_path path =
  not
    (Path.same path Predef.path_bool ||
    Path.same path Predef.path_list ||
    Path.same path Predef.path_unit ||
    Path.same path Predef.path_option)

let rec collect_paths_from_pat r p = match p.pat_desc with
| Tpat_construct(_, {cstr_tag=(Cstr_constant _|Cstr_block _|Cstr_unboxed)},ps)
  ->
    let path = get_constructor_type_path p.pat_type p.pat_env in
    List.fold_left
      collect_paths_from_pat
      (if extendable_path path then add_path path r else r)
      ps
| Tpat_any|Tpat_var _|Tpat_constant _| Tpat_variant (_,None,_) -> r
| Tpat_tuple ps | Tpat_array ps
| Tpat_construct (_, {cstr_tag=Cstr_extension _}, ps)->
    List.fold_left collect_paths_from_pat r ps
| Tpat_record (lps,_) ->
    List.fold_left
      (fun r (_, _, p) -> collect_paths_from_pat r p)
      r lps
| Tpat_variant (_, Some p, _) | Tpat_alias (p,_,_) -> collect_paths_from_pat r p
| Tpat_or (p1,p2,_) ->
    collect_paths_from_pat (collect_paths_from_pat r p1) p2
| Tpat_lazy p
    ->
    collect_paths_from_pat r p


(*
  Actual fragile check
   1. Collect data types in the patterns of the match.
   2. One exhaustivity check per datatype, considering that
      the type is extended.
*)

let do_check_fragile loc casel pss =
  let exts =
    List.fold_left
      (fun r c -> collect_paths_from_pat r c.c_lhs)
      [] casel in
  match exts with
  | [] -> ()
  | _ -> match pss with
    | [] -> ()
    | ps::_ ->
        List.iter
          (fun ext ->
            match exhaust (Some ext) pss (List.length ps) with
            | No_matching_value ->
                Location.prerr_warning
                  loc
                  (Warnings.Fragile_match (Path.name ext))
            | Witnesses _ -> ())
          exts

(********************************)
(* Exported unused clause check *)
(********************************)

let check_unused pred casel =
  if Warnings.is_active Warnings.Unused_match
  || List.exists (fun c -> c.c_rhs.exp_desc = Texp_unreachable) casel then
    let rec do_rec pref = function
      | [] -> ()
      | {c_lhs=q; c_guard; c_rhs} :: rem ->
          let qs = [q] in
            begin try
              let pss =
                  get_mins le_pats (List.filter (compats qs) pref) in
              (* First look for redundant or partially redundant patterns *)
              let r = every_satisfiables (make_rows pss) (make_row qs) in
              let refute = (c_rhs.exp_desc = Texp_unreachable) in
              (* Do not warn for unused [pat -> .] *)
              if r = Unused && refute then () else
              let r =
                (* Do not refine if either:
                   - we already know the clause is unused
                   - the clause under consideration is not a refutation clause
                     and either:
                     + there are no other lines
                     + we do not care whether the types prevent this clause to
                       be reached.
                     If the clause under consideration *is* a refutation clause
                     then we do need to check more carefully whether it can be
                     refuted or not.  *)
                let skip =
                  r = Unused || (not refute && pref = []) ||
                  not(refute || Warnings.is_active Warnings.Unreachable_case) in
                if skip then r else
                (* Then look for empty patterns *)
                let sfs = list_satisfying_vectors pss qs in
                if sfs = [] then Unused else
                let sfs =
                  List.map (function [u] -> u | _ -> assert false) sfs in
                let u = orify_many sfs in
                (*Format.eprintf "%a@." pretty_val u;*)
                let (pattern,constrs,labels) = Conv.conv u in
                let pattern = {pattern with Parsetree.ppat_loc = q.pat_loc} in
                match pred refute constrs labels pattern with
                  None when not refute ->
                    Location.prerr_warning q.pat_loc Warnings.Unreachable_case;
                    Used
                | _ -> r
              in
              match r with
              | Unused ->
                  Location.prerr_warning
                    q.pat_loc Warnings.Unused_match
              | Upartial ps ->
                  List.iter
                    (fun p ->
                      Location.prerr_warning
                        p.pat_loc Warnings.Unused_pat)
                    ps
              | Used -> ()
            with Empty | Not_found -> assert false
            end ;

          if c_guard <> None then
            do_rec pref rem
          else
            do_rec ([q]::pref) rem in

    do_rec [] casel

(*********************************)
(* Exported irrefutability tests *)
(*********************************)

let irrefutable pat = le_pat pat omega

let inactive ~partial pat =
  match partial with
  | Partial -> false
  | Total -> begin
      let rec loop pat =
        match pat.pat_desc with
        | Tpat_lazy _ | Tpat_array _ ->
          false
        | Tpat_any | Tpat_var _ | Tpat_variant (_, None, _) ->
            true
        | Tpat_constant c -> begin
            match c with
            | Const_string _ -> Config.safe_string
            | Const_int _ | Const_char _ | Const_float _
            | Const_int32 _ | Const_int64 _ | Const_nativeint _ -> true
          end
        | Tpat_tuple ps | Tpat_construct (_, _, ps) ->
            List.for_all (fun p -> loop p) ps
        | Tpat_alias (p,_,_) | Tpat_variant (_, Some p, _) ->
            loop p
        | Tpat_record (ldps,_) ->
            List.for_all
              (fun (_, lbl, p) -> lbl.lbl_mut = Immutable && loop p)
              ldps
        | Tpat_or (p,q,_) ->
            loop p && loop q
      in
      loop pat
  end







(*********************************)
(* Exported exhaustiveness check *)
(*********************************)

(*
   Fragile check is performed when required and
   on exhaustive matches only.
*)

let check_partial pred loc casel =
  let pss = initial_matrix casel in
  let pss = get_mins le_pats pss in
  let total = do_check_partial ~pred loc casel pss in
  if
    total = Total && Warnings.is_active (Warnings.Fragile_match "")
  then begin
    do_check_fragile loc casel pss
  end ;
  total

(*************************************)
(* Ambiguous variable in or-patterns *)
(*************************************)

(* Specification: ambiguous variables in or-patterns.

   The semantics of or-patterns in OCaml is specified with
   a left-to-right bias: a value [v] matches the pattern [p | q] if it
   matches [p] or [q], but if it matches both, the environment
   captured by the match is the environment captured by [p], never the
   one captured by [q].

   While this property is generally well-understood, one specific case
   where users expect a different semantics is when a pattern is
   followed by a when-guard: [| p when g -> e]. Consider for example:

     | ((Const x, _) | (_, Const x)) when is_neutral x -> branch

   The semantics is clear: match the scrutinee against the pattern, if
   it matches, test the guard, and if the guard passes, take the
   branch.

   However, consider the input [(Const a, Const b)], where [a] fails
   the test [is_neutral f], while [b] passes the test [is_neutral
   b]. With the left-to-right semantics, the clause above is *not*
   taken by its input: matching [(Const a, Const b)] against the
   or-pattern succeeds in the left branch, it returns the environment
   [x -> a], and then the guard [is_neutral a] is tested and fails,
   the branch is not taken. Most users, however, intuitively expect
   that any pair that has one side passing the test will take the
   branch. They assume it is equivalent to the following:

     | (Const x, _) when is_neutral x -> branch
     | (_, Const x) when is_neutral x -> branch

   while it is not.

   The code below is dedicated to finding these confusing cases: the
   cases where a guard uses "ambiguous" variables, that are bound to
   different parts of the scrutinees by different sides of
   a or-pattern. In other words, it finds the cases where the
   specified left-to-right semantics is not equivalent to
   a non-deterministic semantics (any branch can be taken) relatively
   to a specific guard.
*)

let pattern_vars p = Ident.Set.of_list (Typedtree.pat_bound_idents p)

(* Row for ambiguous variable search,
   row is the traditional pattern row,
   varsets contain a list of head variable sets (varsets)

   A given varset contains all the variables that appeared at the head
   of a pattern in the row at some point during traversal: they would
   all be bound to the same value at matching time. On the contrary,
   two variables of different varsets appeared at different places in
   the pattern and may be bound to distinct sub-parts of the matched
   value.

   All rows of a (sub)matrix have rows of the same length,
   but also varsets of the same length.

   Varsets are populated when simplifying the first column
   -- the variables of the head pattern are collected in a new varset.
   For example,
     { row = x :: r1; varsets = s1 }
     { row = (Some _) as y :: r2; varsets  = s2 }
     { row = (None as x) as y :: r3; varsets = s3 }
     { row = (Some x | (None as x)) :: r4 with varsets = s4 }
   becomes
     (_, { row = r1; varsets = {x} :: s1 })
     (Some _, { row = r2; varsets = {y} :: s2 })
     (None, { row = r3; varsets = {x, y} :: s3 })
     (Some x, { row = r4; varsets = {} :: s4 })
     (None, { row = r4; varsets = {x} :: s4 })
*)
type amb_row = { row : pattern list ; varsets : Ident.Set.t list; }

let simplify_head_amb_pat head_bound_variables varsets ~add_column p ps k =
  let rec simpl head_bound_variables varsets p ps k =
    match p.pat_desc with
    | Tpat_alias (p,x,_) ->
      simpl (Ident.Set.add x head_bound_variables) varsets p ps k
    | Tpat_var (x,_) ->
      let rest_of_the_row =
        { row = ps; varsets = Ident.Set.add x head_bound_variables :: varsets; }
      in
      add_column (Pattern_head.deconstruct omega) rest_of_the_row k
    | Tpat_or (p1,p2,_) ->
      simpl head_bound_variables varsets p1 ps
        (simpl head_bound_variables varsets p2 ps k)
    | _ ->
      add_column (Pattern_head.deconstruct p)
        { row = ps; varsets = head_bound_variables :: varsets; } k
  in simpl head_bound_variables varsets p ps k

(*
   To accurately report ambiguous variables, one must consider
   that previous clauses have already matched some values.
   Consider for example:

     | (Foo x, Foo y) -> ...
     | ((Foo x, _) | (_, Foo x)) when bar x -> ...

   The second line taken in isolation uses an unstable variable,
   but the discriminating values, of the shape [(Foo v1, Foo v2)],
   would all be filtered by the line above.

   To track this information, the matrices we analyze contain both
   *positive* rows, that describe the rows currently being analyzed
   (of type Varsets.row, so that their varsets are tracked) and
   *negative rows*, that describe the cases already matched against.

   The values matched by a signed matrix are the values matched by
   some of the positive rows but none of the negative rows. In
   particular, a variable is stable if, for any value not matched by
   any of the negative rows, the environment captured by any of the
   matching positive rows is identical.
*)
type ('a, 'b) signed = Positive of 'a | Negative of 'b

let rec simplify_first_amb_col = function
  | [] -> []
  | (Negative [] | Positive { row = []; _ }) :: _  -> assert false
  | Negative (n :: ns) :: rem ->
      let add_column n ns k = (n, Negative ns) :: k in
      simplify_head_pat
        ~add_column n ns (simplify_first_amb_col rem)
  | Positive { row = p::ps; varsets; }::rem ->
      let add_column p ps k = (p, Positive ps) :: k in
      simplify_head_amb_pat
        Ident.Set.empty varsets
        ~add_column p ps (simplify_first_amb_col rem)

(* Compute stable bindings *)

type stable_vars =
  | All
  | Vars of Ident.Set.t

let stable_inter sv1 sv2 = match sv1, sv2 with
  | All, sv | sv, All -> sv
  | Vars s1, Vars s2 -> Vars (Ident.Set.inter s1 s2)

let reduce f = function
| [] -> invalid_arg "reduce"
| x::xs -> List.fold_left f x xs

let rec matrix_stable_vars m = match m with
  | [] -> All
  | ((Positive {row = []; _} | Negative []) :: _) as empty_rows ->
      let exception Negative_empty_row in
      (* if at least one empty row is negative, the matrix matches no value *)
      let get_varsets = function
        | Negative n ->
            (* All rows have the same number of columns;
               if the first row is empty, they all are. *)
            assert (n = []);
            raise Negative_empty_row
        | Positive p ->
            assert (p.row = []);
            p.varsets in
      begin match List.map get_varsets empty_rows with
      | exception Negative_empty_row -> All
      | rows_varsets ->
          let stables_in_varsets =
            reduce (List.map2 Ident.Set.inter) rows_varsets in
          (* The stable variables are those stable at any position *)
          Vars
            (List.fold_left Ident.Set.union Ident.Set.empty stables_in_varsets)
      end
  | m ->
      let is_negative = function
        | Negative _ -> true
        | Positive _ -> false in
      if List.for_all is_negative m then
        (* optimization: quit early if there are no positive rows.
           This may happen often when the initial matrix has many
           negative cases and few positive cases (a small guarded
           clause after a long list of clauses) *)
        All
      else begin
        let m = simplify_first_amb_col m in
        if not (all_coherent (first_column m)) then
          All
        else begin
          (* If the column is ill-typed but deemed coherent, we might
             spuriously warn about some variables being unstable.
             As sad as that might be, the warning can be silenced by
             splitting the or-pattern...  *)
          let submatrices =
            let extend_row columns = function
              | Negative r -> Negative (columns @ r)
              | Positive r -> Positive { r with row = columns @ r.row } in
            let q0 = discr_pat omega m in
            let { default; constrs } =
              build_specialized_submatrices ~extend_row q0 m in
            let non_default = List.map snd constrs in
            if full_match false constrs
            then non_default
            else default :: non_default in
          (* A stable variable must be stable in each submatrix. *)
          let submat_stable = List.map matrix_stable_vars submatrices in
          List.fold_left stable_inter All submat_stable
        end
      end

let pattern_stable_vars ns p =
  matrix_stable_vars
    (List.fold_left (fun m n -> Negative n :: m)
       [Positive {varsets = []; row = [p]}] ns)

(* All identifier paths that appear in an expression that occurs
   as a clause right hand side or guard.

  The function is rather complex due to the compilation of
  unpack patterns by introducing code in rhs expressions
  and **guards**.

  For pattern (module M:S)  -> e the code is
  let module M_mod = unpack M .. in e

  Hence M is "free" in e iff M_mod is free in e.

  Not doing so will yield excessive  warning in
  (module (M:S) } ...) when true -> ....
  as M is always present in
  let module M_mod = unpack M .. in true
*)

let all_rhs_idents exp =
  let ids = ref Ident.Set.empty in
(* Very hackish, detect unpack pattern  compilation
   and perform "indirect check for them" *)
  let is_unpack exp =
      List.exists
        (fun attr -> attr.Parsetree.attr_name.txt = "#modulepat")
        exp.exp_attributes in
  let open Tast_iterator in
  let expr_iter iter exp =
    (match exp.exp_desc with
      | Texp_ident (path, _lid, _descr) ->
        List.iter (fun id -> ids := Ident.Set.add id !ids) (Path.heads path)
      (* Use default iterator methods for rest of match.*)
      | _ -> Tast_iterator.default_iterator.expr iter exp);

    if is_unpack exp then begin match exp.exp_desc with
    | Texp_letmodule
        (id_mod,_,_,
         {mod_desc=
          Tmod_unpack ({exp_desc=Texp_ident (Path.Pident id_exp,_,_)},_)},
         _) ->
           assert (Ident.Set.mem id_exp !ids) ;
           begin match id_mod with
           | Some id_mod when not (Ident.Set.mem id_mod !ids) ->
             ids := Ident.Set.remove id_exp !ids
           | _ -> ()
           end
    | _ -> assert false
    end
  in
  let iterator = {Tast_iterator.default_iterator with expr = expr_iter} in
  iterator.expr iterator exp;
  !ids

let check_ambiguous_bindings =
  let open Warnings in
  let warn0 = Ambiguous_pattern [] in
  fun cases ->
    if is_active warn0 then
      let check_case ns case = match case with
        | { c_lhs = p; c_guard=None ; _} -> [p]::ns
        | { c_lhs=p; c_guard=Some g; _} ->
            let all =
              Ident.Set.inter (pattern_vars p) (all_rhs_idents g) in
            if not (Ident.Set.is_empty all) then begin
              match pattern_stable_vars ns p with
              | All -> ()
              | Vars stable ->
                  let ambiguous = Ident.Set.diff all stable in
                  if not (Ident.Set.is_empty ambiguous) then begin
                    let pps =
                      Ident.Set.elements ambiguous |> List.map Ident.name in
                    let warn = Ambiguous_pattern pps in
                    Location.prerr_warning p.pat_loc warn
                  end
            end;
            ns
      in
      ignore (List.fold_left check_case [] cases)
