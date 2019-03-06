(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Luc Maranget, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a shared = Shared of 'a | Single of 'a

type ('a, 'ctx) t_store =
  {act_get : unit -> 'a array ;
   act_get_shared : unit -> 'a shared array ;
   act_store : 'ctx -> 'a -> int ;
   act_store_shared : 'ctx -> 'a -> int ; }

exception Not_simple

module type Stored = sig
  type t
  type key
  val compare_key : key -> key -> int
  val make_key : t -> key option
end

module type CtxStored = sig
  include Stored
  type context
  val make_key : context -> t -> key option
end

module CtxStore(A:CtxStored) = struct
  module AMap =
    Map.Make(struct type t = A.key let compare = A.compare_key end)

  type intern =
    { mutable map : (bool * int)  AMap.t ;
      mutable next : int ;
      mutable acts : (bool * A.t) list; }

  let mk_store () =
    let st =
      { map = AMap.empty ;
        next = 0 ;
        acts = [] ; } in

    let add mustshare act =
      let i = st.next in
      st.acts <- (mustshare,act) :: st.acts ;
      st.next <- i+1 ;
      i in

    let store mustshare ctx act = match A.make_key ctx act with
      | Some key ->
          begin try
            let (shared,i) = AMap.find key st.map in
            if not shared then st.map <- AMap.add key (true,i) st.map ;
            i
          with Not_found ->
            let i = add mustshare act in
            st.map <- AMap.add key (mustshare,i) st.map ;
            i
          end
      | None ->
          add mustshare act

    and get () = Array.of_list (List.rev_map (fun (_,act) -> act) st.acts)

    and get_shared () =
      let acts =
        Array.of_list
          (List.rev_map
             (fun (shared,act) ->
                if shared then Shared act else Single act)
             st.acts) in
      AMap.iter
        (fun _ (shared,i) ->
           if shared then match acts.(i) with
             | Single act -> acts.(i) <- Shared act
             | Shared _ -> ())
        st.map ;
      acts in
    {act_store = store false ; act_store_shared = store true ;
     act_get = get; act_get_shared = get_shared; }
end

module Store(A:Stored) = struct
  module Me =
    CtxStore
      (struct
        include A
        type context = unit
        let make_key () = A.make_key
      end)

  let mk_store = Me.mk_store
end



module type S =
sig
  type primitive
  val eqint : primitive
  val neint : primitive
  val leint : primitive
  val ltint : primitive
  val geint : primitive
  val gtint : primitive
  type act

  type location
  val no_location : location
  val location_of_action : act -> location

  val bind : act -> (act -> act) -> act
  val make_const : location -> int -> act
  val make_offset : location -> act -> int -> act
  val make_prim : location -> primitive -> act list -> act
  val make_isout : location -> act -> act -> act
  val make_isin : location -> act -> act -> act
  val make_if : location -> act -> act -> act -> act
  val make_switch : location -> act -> int array -> act array -> act
  val make_catch : location -> act -> int * (act -> act)
  val make_exit : location -> int -> act
end

(* The module will ``produce good code for the case statement'' *)
(*
  Adaptation of
   R.L. Berstein
   ``Producing good code for the case statement''
   Software Practice and Experience, 15(10) (1985)
 and
   D.L. Spuler
    ``Two-Way Comparison Search Trees, a Generalisation of Binary Search Trees
      and Split Trees''
    ``Compiler Code Generation for Multiway Branch Statement as
      a Static Search Problem''
   Technical Reports, James Cook University
*)
(*
  Main adaptation is considering interval tests
 (implemented as one addition + one unsigned test and branch)
  which leads to exhaustive search for finding the optimal
  test sequence in small cases and heuristics otherwise.
*)
module Make (Arg : S) =
struct
  type case = {
    low_loc : Arg.location;
    low : int;
    high_plus_one_loc : Arg.location;
    high : int;
    action_index : int;
  }

  type cases = case array

  type 'a inter = {
    cases : cases;
    actions : 'a array;
  }

  type 'a t_ctx = {
    off : int;
    arg : 'a;
    loc : Arg.location;
  }

  let cut = ref 8
  and more_cut = ref 16

(*
let pint chan i =
  if i = min_int then Printf.fprintf chan "-oo"
  else if i=max_int then Printf.fprintf chan "oo"
  else Printf.fprintf chan "%d" i

let pcases chan cases =
  for i =0 to Array.length cases-1 do
    let l,h,act = cases.(i) in
    if l=h then
      Printf.fprintf chan "%d:%d " l act
    else
      Printf.fprintf chan "%a..%a:%d " pint l pint h act
  done

let prerr_inter i = Printf.fprintf stderr
        "cases=%a" pcases i.cases
*)

  let get_act cases i = cases.(i).action_index
  and get_low cases i = cases.(i).low

  type ctests = {
    mutable n : int ;
    mutable ni : int ;
  }

  let too_much = {n=max_int ; ni=max_int}

(*
let ptests chan {n=n ; ni=ni} =
  Printf.fprintf chan "{n=%d ; ni=%d}" n ni

let pta chan t =
  for i =0 to Array.length t-1 do
    Printf.fprintf chan "%d: %a\n" i ptests t.(i)
  done
*)

  let less_tests c1 c2 =
    if c1.n < c2.n then
      true
    else if c1.n = c2.n then begin
      if c1.ni < c2.ni then
        true
      else
        false
    end else
      false

  and eq_tests c1 c2 = c1.n = c2.n && c1.ni=c2.ni

  let less2tests (c1,d1) (c2,d2) =
    if eq_tests c1 c2 then
      less_tests d1 d2
    else
      less_tests c1 c2

  let add_test t1 t2 =
    t1.n <- t1.n + t2.n ;
    t1.ni <- t1.ni + t2.ni ;

  type t_ret = Inter of int * int  | Sep of int | No

(*
let pret chan = function
  | Inter (i,j)-> Printf.fprintf chan "Inter %d %d" i j
  | Sep i -> Printf.fprintf chan "Sep %d" i
  | No -> Printf.fprintf chan "No"
*)

  let split_cases cases i =
    assert (i >= 0 && i < Array.length cases);
    let pivot_loc = cases.(i).low_loc in
    pivot_loc,
      get_low cases i,
      Array.sub cases 0 i,
      Array.sub cases i (Array.length cases-i)


  let case_append (c1 : cases) (c2 : cases) =
    let len1 = Array.length c1
    and len2 = Array.length c2 in
    match len1,len2 with
    | 0,_ -> c2
    | _,0 -> c1
    | _,_ ->
        let {
          low_loc = l1_loc;
          low = l1;
          high_plus_one_loc = h1_plus_one_loc;
          high = h1;
          action_index = act1;
        } = c1.(Array.length c1-1)
        and {
          low_loc = l2_loc;
          low = l2;
          high_plus_one_loc = h2_plus_one_loc;
          high = h2;
          action_index = act2;
        } = c2.(0)
        in
        if act1 = act2 then begin
          let r = Array.make (len1+len2-1) c1.(0) in
          for i = 0 to len1-2 do
            r.(i) <- c1.(i)
          done ;
          let l_loc, l =
            if len1-2 >= 0 then begin
              let {
                low_loc = _;
                low = _;
                high_plus_one_loc = h_plus_one_loc;
                high = h;
                action_index = _;
              } = r.(len1-2)
              in
              if h+1 < l1 then
                h_plus_one_loc, h+1
              else
                l1_loc, l1
            end else
              l1_loc, l1
          and h_plus_one_loc, h =
            if 1 < len2-1 then begin
              let {
                low_loc = l_loc;
                low = l;
                high_plus_one_loc = _;
                high = _;
                action_index = _;
              } = c2.(1) in
              if h2+1 < l then
                l_loc, l-1
              else
                h2_plus_one_loc, h2
            end else
              h2_plus_one_loc, h2
          in
          r.(len1-1) <- {
            low_loc = l_loc;
            low = l;
            high_plus_one_loc = h_plus_one_loc;
            high = h;
            action_index = act1;
          };
          for i=1 to len2-1  do
            r.(len1-1+i) <- c2.(i)
          done ;
          r
        end else if h1 > l1 then begin
          let r = Array.make (len1+len2) c1.(0) in
          for i = 0 to len1-2 do
            r.(i) <- c1.(i)
          done ;
          r.(len1-1) <- {
            low_loc = l1_loc;
            low = l1;
            high_plus_one_loc = l2_loc;
            high = l2 - 1;
            action_index = act1;
          };
          for i=0 to len2-1  do
            r.(len1+i) <- c2.(i)
          done ;
          r
        end else if h2 > l2 then begin
          let r = Array.make (len1+len2) c1.(0) in
          for i = 0 to len1-1 do
            r.(i) <- c1.(i)
          done ;
          r.(len1) <- {
            low_loc = h1_plus_one_loc;
            low = h1 + 1;
            high_plus_one_loc = h2_plus_one_loc;
            high = h2;
            action_index = act2;
          };
          for i=1 to len2-1  do
            r.(len1+i) <- c2.(i)
          done ;
          r
        end else begin
          Array.append c1 c2
        end


  let slice_cases i j (cases : cases) =
    let lcases = Array.length cases in
    let { low; _ } = cases.(i)
    and { high; _ } = cases.(j) in
    assert (j >= i);
    let pivot_case_for_loc = cases.(i + ((j - i) / 2)) in
    let pivot_loc = pivot_case_for_loc.low_loc in
    pivot_loc, low, high,
      Array.sub cases i (j-i+1),
      case_append (Array.sub cases 0 i) (Array.sub cases (j+1) (lcases-(j+1)))

  type kind = Kvalue of int | Kinter of int | Kempty

(*
let pkind chan = function
  | Kvalue i ->Printf.fprintf chan "V%d" i
  | Kinter i -> Printf.fprintf chan "I%d" i
  | Kempty -> Printf.fprintf chan "E"

let rec pkey chan  = function
  | [] -> ()
  | [k] -> pkind chan k
  | k::rem ->
      Printf.fprintf chan "%a %a" pkey rem pkind k
*)

  let t = Hashtbl.create 17

  let make_key  cases =
    let seen = ref []
    and count = ref 0 in
    let rec got_it act = function
      | [] ->
          seen := (act,!count):: !seen ;
          let r = !count in
          incr count ;
          r
      | (act0,index) :: rem ->
          if act0 = act then
            index
          else
            got_it act rem in

    let make_one l h act =
      if l=h then
        Kvalue (got_it act !seen)
      else
        Kinter (got_it act !seen) in

    let rec make_rec i pl =
      if i < 0 then
        []
      else
        let { low = l; high = h; action_index = act; _ } = cases.(i) in
        if pl = h+1 then
          make_one l h act::make_rec (i-1) l
        else
          Kempty::make_one l h act::make_rec (i-1) l in

    let { low = l; high = h; action_index = act; _ } =
      cases.(Array.length cases-1)
    in
    make_one l h act::make_rec (Array.length cases-2) l


  let same_act t =
    let len = Array.length t in
    let a = get_act t (len-1) in
    let rec do_rec i =
      if i < 0 then true
      else
        let b = get_act t i in
        b=a && do_rec (i-1) in
    do_rec (len-2)


(*
  Interval test x in [l,h] works by checking x-l in [0,h-l]
   * This may be false for arithmetic modulo 2^31
   * Subtracting l may change the relative ordering of values
     and invalid the invariant that matched values are given in
     increasing order

   To avoid this, interval check is allowed only when the
   integers indeed present in the whole case interval are
   in [-2^16 ; 2^16]

   This condition is checked by zyva
*)

  let inter_limit = 1 lsl 16

  let ok_inter = ref false

  let rec opt_count top cases =
    let key = make_key cases in
    try
      Hashtbl.find t key
    with
    | Not_found ->
        let r =
          let lcases = Array.length cases in
          match lcases with
          | 0 -> assert false
          | _ when same_act cases -> No, ({n=0; ni=0},{n=0; ni=0})
          | _ ->
              if lcases < !cut then
                enum top cases
              else if lcases < !more_cut then
                heuristic cases
              else
                divide cases in
        Hashtbl.add t key r ;
        r

  and divide cases =
    let lcases = Array.length cases in
    let m = lcases/2 in
    let _, _, left, right = split_cases cases m in
    let ci = {n=1 ; ni=0}
    and cm = {n=1 ; ni=0}
    and _,(cml,cleft) = opt_count false left
    and _,(cmr,cright) = opt_count false right in
    add_test ci cleft ;
    add_test ci cright ;
    if less_tests cml cmr then
      add_test cm cmr
    else
      add_test cm cml ;
    Sep m,(cm, ci)

  and heuristic cases =
    let lcases = Array.length cases in

    let sep,csep = divide cases

    and inter,cinter =
      if !ok_inter then begin
        let { action_index = act0; _ } = cases.(0)
        and { action_index = act1; _ } = cases.(lcases-1) in
        if act0 = act1 then begin
          let _low_loc, low, high, inside, outside =
            slice_cases 1 (lcases-2) cases
          in
          let _,(cmi,cinside) = opt_count false inside
          and _,(cmo,coutside) = opt_count false outside
          and cmij = {n=1 ; ni=(if low=high then 0 else 1)}
          and cij = {n=1 ; ni=(if low=high then 0 else 1)} in
          add_test cij cinside ;
          add_test cij coutside ;
          if less_tests cmi cmo then
            add_test cmij cmo
          else
            add_test cmij cmi ;
          Inter (1,lcases-2),(cmij,cij)
        end else
          Inter (-1,-1),(too_much, too_much)
      end else
        Inter (-1,-1),(too_much, too_much) in
    if less2tests csep cinter then
      sep,csep
    else
      inter,cinter


  and enum top cases =
    let lcases = Array.length cases in
    let lim, with_sep =
      let best = ref (-1) and best_cost = ref (too_much,too_much) in

      for i = 1 to lcases-(1) do
        let _, _, left, right = split_cases cases i in
        let ci = {n=1 ; ni=0}
        and cm = {n=1 ; ni=0}
        and _,(cml,cleft) = opt_count false left
        and _,(cmr,cright) = opt_count false right in
        add_test ci cleft ;
        add_test ci cright ;
        if less_tests cml cmr then
          add_test cm cmr
        else
          add_test cm cml ;

        if
          less2tests (cm,ci) !best_cost
        then begin
          if top then
            Printf.fprintf stderr "Get it: %d\n" i ;
          best := i ;
          best_cost := (cm,ci)
        end
      done ;
      !best, !best_cost in

    let ilow, ihigh, with_inter =
      if not !ok_inter then
        let rlow = ref (-1) and rhigh = ref (-1)
        and best_cost= ref (too_much,too_much) in
        for i=1 to lcases-2 do
          let _low_loc, low, high, inside, outside = slice_cases i i cases in
          if low=high then begin
            let _,(cmi,cinside) = opt_count false inside
            and _,(cmo,coutside) = opt_count false outside
            and cmij = {n=1 ; ni=0}
            and cij = {n=1 ; ni=0} in
            add_test cij cinside ;
            add_test cij coutside ;
            if less_tests cmi cmo then
              add_test cmij cmo
            else
              add_test cmij cmi ;
            if less2tests (cmij,cij) !best_cost then begin
              rlow := i ;
              rhigh := i ;
              best_cost := (cmij,cij)
            end
          end
        done ;
        !rlow, !rhigh, !best_cost
      else
        let rlow = ref (-1) and rhigh = ref (-1)
        and best_cost= ref (too_much,too_much) in
        for i=1 to lcases-2 do
          for j=i to lcases-2 do
            let _low_loc, low, high, inside, outside = slice_cases i j cases in
            let _,(cmi,cinside) = opt_count false inside
            and _,(cmo,coutside) = opt_count false outside
            and cmij = {n=1 ; ni=(if low=high then 0 else 1)}
            and cij = {n=1 ; ni=(if low=high then 0 else 1)} in
            add_test cij cinside ;
            add_test cij coutside ;
            if less_tests cmi cmo then
              add_test cmij cmo
            else
              add_test cmij cmi ;
            if less2tests (cmij,cij) !best_cost then begin
              rlow := i ;
              rhigh := j ;
              best_cost := (cmij,cij)
            end
          done
        done ;
        !rlow, !rhigh, !best_cost in
    let r = ref (Inter (ilow,ihigh)) and rc = ref with_inter in
    if less2tests with_sep !rc then begin
      r := Sep lim ; rc := with_sep
    end ;
    !r, !rc

  let make_if_test loc test arg i ifso ifnot =
    Arg.make_if loc
      (Arg.make_prim loc test [arg ; Arg.make_const loc i])
      ifso ifnot

  let make_if_lt loc arg i ifso ifnot = match i with
    | 1 ->
        make_if_test loc Arg.leint arg 0 ifso ifnot
    | _ ->
        make_if_test loc Arg.ltint arg i ifso ifnot

  and make_if_ge loc arg i ifso ifnot = match i with
    | 1 ->
        make_if_test loc Arg.gtint arg 0 ifso ifnot
    | _ ->
        make_if_test loc Arg.geint arg i ifso ifnot

  and make_if_eq loc arg i ifso ifnot =
    make_if_test loc Arg.eqint arg i ifso ifnot

  and make_if_ne loc arg i ifso ifnot =
    make_if_test loc Arg.neint arg i ifso ifnot

  let do_make_if_out loc h arg ifso ifnot =
    Arg.make_if loc (Arg.make_isout loc h arg) ifso ifnot

  let make_if_out loc ctx l d mk_ifso mk_ifno = match l with
    | 0 ->
        do_make_if_out loc
          (Arg.make_const loc d) ctx.arg (mk_ifso ctx) (mk_ifno ctx)
    | _ ->
        Arg.bind
          (Arg.make_offset loc ctx.arg (-l))
          (fun arg ->
             let ctx = {off= (-l+ctx.off) ; arg=arg; loc; } in
             do_make_if_out loc
               (Arg.make_const loc d) arg (mk_ifso ctx) (mk_ifno ctx))

  let do_make_if_in loc h arg ifso ifnot =
    Arg.make_if loc (Arg.make_isin loc h arg) ifso ifnot

  let make_if_in loc ctx l d mk_ifso mk_ifnot = match l with
    | 0 ->
        let ifso = mk_ifso ctx in
        let ifnot = mk_ifnot ctx in
        do_make_if_in loc (Arg.make_const loc d) ctx.arg ifso ifnot
    | _ ->
        Arg.bind
          (Arg.make_offset loc ctx.arg (-l))
          (fun arg ->
             let ctx = {off= (-l+ctx.off) ; arg=arg; loc; } in
             let ifso = mk_ifso ctx in
             let ifnot = mk_ifnot ctx in
             do_make_if_in loc (Arg.make_const loc d) arg ifso ifnot)

  let rec c_test ctx ({cases=cases ; actions=actions} as s) : Arg.act =
    let lcases = Array.length cases in
    assert(lcases > 0) ;
    if lcases = 1 then
      actions.(get_act cases 0) ctx
    else begin
      let w,_c = opt_count false cases in
(*
  Printf.fprintf stderr
  "off=%d tactic=%a for %a\n"
  ctx.off pret w pcases cases ;
  *)
      match w with
      | No -> actions.(get_act cases 0) ctx
      | Inter (i,j) ->
          let pivot_loc, low, high, inside, outside =
            slice_cases i j cases
          in
          let _,(cinside,_) = opt_count false inside
          and _,(coutside,_) = opt_count false outside in
          (* Costs are retrieved to put the code with more remaining tests
             in the privileged (positive) branch of ``if'' *)
          if low=high then begin
            if less_tests coutside cinside then
              let ifso = c_test ctx { s with cases = inside; } in
              let ifnot = c_test ctx { s with cases = outside; } in
              make_if_eq pivot_loc
                ctx.arg
                (low+ctx.off)
                ifso
                ifnot
            else
              let ifso = c_test ctx { s with cases = outside; } in
              let ifnot = c_test ctx { s with cases = inside; } in
              make_if_ne pivot_loc
                ctx.arg
                (low+ctx.off)
                ifso
                ifnot
          end else begin
            if less_tests coutside cinside then
              make_if_in pivot_loc
                ctx
                (low+ctx.off)
                (high-low)
                (fun ctx -> c_test ctx { s with cases=inside; })
                (fun ctx -> c_test ctx { s with cases=outside; })
            else
              make_if_out pivot_loc
                ctx
                (low+ctx.off)
                (high-low)
                (fun ctx -> c_test ctx { s with cases=outside; })
                (fun ctx -> c_test ctx { s with cases=inside; })
          end
      | Sep i ->
          let pivot_loc, lim, left, right = split_cases cases i in
          let _,(cleft,_) = opt_count false left
          and _,(cright,_) = opt_count false right in
          let left = {s with cases=left}
          and right = {s with cases=right} in
          let left = c_test ctx left in
          let right = c_test ctx right in
          if i=1 && (lim+ctx.off)=1 && get_low cases 0+ctx.off=0 then
            make_if_ne pivot_loc
              ctx.arg 0
              right
              left
          else if less_tests cright cleft then
            make_if_lt pivot_loc
              ctx.arg (lim+ctx.off)
              left
              right
          else
            make_if_ge pivot_loc
              ctx.arg (lim+ctx.off)
              right
              left
    end

  (* Minimal density of switches *)
  let theta = ref 0.33333

  (* Minimal number of tests to make a switch *)
  let switch_min = ref 3

  (* Particular case 0, 1, 2 *)
  let particular_case cases i j =
    j-i = 2 &&
    (let { low = l1; action_index = act1; _ } = cases.(i)
     and { low = l2; _ } = cases.(i+1)
     and { low = l3; high = h3; action_index = act3; _ } = cases.(i+2) in
     l1+1=l2 && l2+1=l3 && l3=h3 &&
     act1 <> act3)

  let approx_count cases i j =
    let l = j-i+1 in
    if l < !cut then
      let _,(_,{n=ntests}) = opt_count false (Array.sub cases i l) in
      ntests
    else
      l-1

  (* Sends back a boolean that says whether is switch is worth or not *)

  let dense {cases} i j =
    if i=j then true
    else
      let { low = l; _ } = cases.(i)
      and { high = h; _ } = cases.(j) in
      let ntests =  approx_count cases i j in
(*
  (ntests+1) >= theta * (h-l+1)
*)
      particular_case cases i j ||
      (ntests >= !switch_min &&
       float_of_int ntests +. 1.0 >=
       !theta *. (float_of_int h -. float_of_int l +. 1.0))

  (* Compute clusters by dynamic programming
     Adaptation of the correction to Bernstein
     ``Correction to `Producing Good Code for the Case Statement' ''
     S.K. Kannan and T.A. Proebsting
     Software Practice and Experience Vol. 24(2) 233 (Feb 1994)
  *)

  let comp_clusters s =
    let len = Array.length s.cases in
    let min_clusters = Array.make len max_int
    and k = Array.make len 0 in
    let get_min i = if i < 0 then 0 else min_clusters.(i) in

    for i = 0 to len-1 do
      for j = 0 to i do
        if
          dense s j i &&
          get_min (j-1) + 1 < min_clusters.(i)
        then begin
          k.(i) <- j ;
          min_clusters.(i) <- get_min (j-1) + 1
        end
      done ;
    done ;
    min_clusters.(len-1),k

  (* Assume j > i *)
  let make_switch loc {cases=cases ; actions=actions} i j =
    let { low = ll; _ } = cases.(i)
    and { high = hh; _ } = cases.(j) in
    let tbl = Array.make (hh-ll+1) 0
    and t = Hashtbl.create 17
    and index = ref 0 in
    let get_index act =
      try
        Hashtbl.find t act
      with
      | Not_found ->
          let i = !index in
          incr index ;
          Hashtbl.add t act i ;
          i in

    for k=i to j do
      let { low = l; high = h; action_index = act; _ } = cases.(k) in
      let index = get_index act in
      for kk=l-ll to h-ll do
        tbl.(kk) <- index
      done
    done ;
    let acts = Array.make !index actions.(0) in
    Hashtbl.iter
      (fun act i -> acts.(i) <- actions.(act))
      t ;
    (fun ctx ->
       match -ll-ctx.off with
       | 0 -> Arg.make_switch loc ctx.arg tbl acts
       | _ ->
           Arg.bind
             (Arg.make_offset loc ctx.arg (-ll-ctx.off))
             (fun arg -> Arg.make_switch loc arg tbl acts))


  let make_clusters ({cases=cases ; actions=actions} as s) n_clusters k =
    let len = Array.length cases in
    let r : cases =
      Array.make n_clusters
        { low_loc = Arg.no_location;
          low = 0;
          high_plus_one_loc = Arg.no_location;
          high = 0;
          action_index = 0;
        }
    and t = Hashtbl.create 17
    and index = ref 0
    and bidon = ref (Array.length actions) in
    let get_index act =
      try
        let i, _act = Hashtbl.find t act in
        i
      with
      | Not_found ->
          let action = actions.(act) in
          let i = !index in
          incr index ;
          Hashtbl.add
            t act
            (i, (fun _ctx -> action)) ;
          i
    and add_index act =
      let i = !index in
      incr index ;
      incr bidon ;
      Hashtbl.add t !bidon (i, act) ;
      i in

    let rec zyva j ir =
      let i = k.(j) in
      begin if i=j then
          let {
            low_loc;
            low;
            high_plus_one_loc;
            high;
            action_index;
          } = cases.(i)
          in
          r.(ir) <- {
            low_loc;
            low;
            high_plus_one_loc;
            high;
            action_index = get_index action_index;
          }
        else (* assert i < j *)
          let { low_loc; low; _ } = cases.(i)
          and { high; high_plus_one_loc; _ } = cases.(j) in
          r.(ir) <- {
            low_loc;
            low;
            high_plus_one_loc;
            high;
            action_index = add_index (make_switch low_loc s i j)
          }
      end ;
      if i > 0 then zyva (i-1) (ir-1) in

    zyva (len-1) (n_clusters-1) ;
    let acts = Array.make !index (fun _ -> assert false) in
    Hashtbl.iter (fun _ (i, act) -> acts.(i) <- act) t;
    {cases = r ; actions = acts}
  ;;


  let do_zyva loc (low,high) arg cases actions =
    let old_ok = !ok_inter in
    ok_inter := (abs low <= inter_limit && abs high <= inter_limit) ;
    if !ok_inter <> old_ok then Hashtbl.clear t ;

    let s = {cases=cases ; actions=actions} in

(*
  Printf.eprintf "ZYVA: %B [low=%i,high=%i]\n" !ok_inter low high ;
  pcases stderr cases ;
  prerr_endline "" ;
*)
    let n_clusters,k = comp_clusters s in
    let clusters = make_clusters s n_clusters k in
    c_test {arg=arg ; off=0; loc} clusters

  let abstract_shared actions =
    let handlers = ref (fun x -> x) in
    let actions =
      Array.map
        (fun act -> match act with
           | Single act -> act
           | Shared act ->
               let loc = Arg.location_of_action act in
               let i,h = Arg.make_catch loc act in
               let oh = !handlers in
               handlers := (fun act -> h (oh act)) ;
               Arg.make_exit loc i)
        actions in
    !handlers,actions

  let zyva loc lh arg cases actions =
    assert (Array.length cases > 0) ;
    let actions = actions.act_get_shared () in
    let hs,actions = abstract_shared actions in
    hs (do_zyva loc lh arg cases actions)

  and test_sequence loc arg cases actions =
    assert (Array.length cases > 0) ;
    let actions = actions.act_get_shared () in
    let hs,actions = abstract_shared actions in
    let old_ok = !ok_inter in
    ok_inter := false ;
    if !ok_inter <> old_ok then Hashtbl.clear t ;
    let s =
      {cases=cases ;
       actions=Array.map (fun act _ -> act) actions;
      }
    in
(*
  Printf.eprintf "SEQUENCE: %B\n" !ok_inter ;
  pcases stderr cases ;
  prerr_endline "" ;
*)
    hs (c_test {arg=arg ; off=0; loc;} s)
  ;;

end
