open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Lambda
open Parmatch
open Printf

(* Matching parameters *)

let match_string =
  try Sys.getenv "MATCH" with Not_found -> ""

let match_option c =
  try ignore (String.index match_string c) ; true
  with Not_found -> false

let share = match_option 's'
and tree = match_option 't'
and direction = match_option 'o'
and verbose =
  let r = ref 0 in
  for i = 0 to String.length match_string-1 do
    match  match_string.[i] with
    | 'v' -> incr r
    | 'V' -> r := !r + 2
    | _ -> ()
  done ;
  !r

(* Flatten one pattern *)
exception Cannot_flatten

let flatten_pattern size p = match p.pat_desc with
| Tpat_tuple args -> args
| Tpat_any -> omegas size  
| _ -> raise Cannot_flatten

(* Pattern free variables *)
let rec extract_vars r p = match p.pat_desc with
| Tpat_var id -> IdentSet.add id r
| Tpat_alias (p, id) ->
    extract_vars (IdentSet.add id r) p
| Tpat_tuple pats ->
    List.fold_left extract_vars r pats
| Tpat_record lpats ->
    List.fold_left
      (fun r (_,p) -> extract_vars r p)
      r lpats
| Tpat_construct (_,pats) ->
    List.fold_left extract_vars r pats
| Tpat_array pats ->
    List.fold_left extract_vars r pats
| Tpat_variant (_,Some p, _) -> extract_vars r p
| Tpat_or (p,_,_) -> extract_vars r p
| Tpat_constant _|Tpat_any|Tpat_variant (_,None,_) -> r

(*
   If there is a guard in a matching, then
   set exhaustiveness info to Partial.
   (because of side effects in guards, assume the worst)
*)

let check_partial pat_act_list partial =
  if
    List.exists
      (fun (_,lam) -> is_guarded lam)
       pat_act_list
  then begin
    Partial 
  end else
    partial
(* To find reasonable names for variables *)

let rec name_pattern default = function
    (pat :: patl, action) :: rem ->
      begin match pat.pat_desc with
        Tpat_var id -> id
      | Tpat_alias(p, id) -> id
      | _ -> name_pattern default rem
      end
  | _ -> Ident.create default

let arg_to_var arg cls = match arg with
| Lvar v -> v,arg
| _ ->
    let v = name_pattern "match" cls in
    v,Lvar v

(* Insertion of debugging events *)

let rec event_branch repr lam =
  begin match lam, repr with
    (_, None) ->
      lam
  | (Levent(lam', ev), Some r) ->
      incr r;
      Levent(lam', {lev_loc = ev.lev_loc;
                    lev_kind = ev.lev_kind;
                    lev_repr = repr;
                    lev_env = ev.lev_env})
  | (Llet(str, id, lam, body), _) ->
      Llet(str, id, lam, event_branch repr body)
  | Lstaticraise _,_ -> lam
  | (_, Some r) ->
      Printlambda.lambda Format.str_formatter lam ;
      fatal_error
        ("Matching.event_branch: "^Format.flush_str_formatter ())
  end




(* Once matchings are simplified one easily finds
   their nature *)

let rec what_is_cases cases = match cases with
| ({pat_desc=Tpat_any} :: _, _) :: rem -> what_is_cases rem
| (({pat_desc=(Tpat_var _|Tpat_or (_,_,_)|Tpat_alias (_,_))}::_),_)::_
  -> assert false (* applies to simplified matchings only *)
| (p::_,_)::_ -> p
| [] -> omega 
| _ -> assert false


(* Count how many variant labels can occur *)
let count_variants row = 
  let row = Btype.row_repr row in
  let num_constr = ref 0 in
  if row.row_closed then
    List.iter
      (fun (_, f) ->
        match Btype.row_field_repr f with
          Rabsent | Reither(true, _::_, _, _) -> ()
        | _ -> incr num_constr)
      row.row_fields
  else
    num_constr := max_int;
  !num_constr

(* A slight attempt to identify semantically equivalent lambda-expressions *)
exception Not_simple

let rec raw_rec env = function
  | Llet(StrictOpt,x,ex, body) -> raw_rec ((x,raw_rec env ex)::env) body
  | Lvar id as l ->
      begin try List.assoc id env with
      | Not_found -> l
      end
  | Lprim (Pfield i,args) ->
      Lprim (Pfield i, List.map (raw_rec env) args)
  | Lconst _ as l -> l
  | Lstaticraise (i,args) ->
        Lstaticraise (i, List.map (raw_rec env) args)
  | _ -> raise Not_simple

let raw_action l = try raw_rec [] l with Not_simple -> l

let same_actions = function
  | [] -> None
  | [_,act] -> Some act
  | (_,act0) :: rem ->
      try
        let raw_act0 = raw_rec [] act0 in
        let rec s_rec = function
          | [] -> Some act0
          | (_,act)::rem ->
              if raw_act0 = raw_rec [] act then
                s_rec rem
              else
                None in
        s_rec rem
      with
      | Not_simple -> None

let equal_action act1 act2 =
  try
    let raw1 = raw_rec [] act1
    and raw2 = raw_rec [] act2 in
    raw1 = raw2
  with
  | Not_simple -> false

(*********************************)
(* Switch or test sequence stuff *)
(*********************************)

let float_compare s1 s2 =
  let f1 = float_of_string s1 and f2 = float_of_string s2 in
  Pervasives.compare f1 f2

let sort_lambda_list l =
  List.sort
    (fun (x,_) (y,_) -> match x,y with
    | Const_float f1, Const_float f2 -> float_compare f1 f2
    | _, _ -> Pervasives.compare x y)
    l

let rec cut n l =
  if n = 0 then [],l
  else match l with
    [] -> raise (Invalid_argument "cut")
  | a::l -> let l1,l2 = cut (n-1) l in a::l1, l2

let rec do_tests_fail fail tst arg = function
  | [] -> fail
  | (c, act)::rem ->
      Lifthenelse
        (Lprim (tst, [arg ; Lconst (Const_base c)]),
         do_tests_fail fail tst arg rem,
         act)

let rec do_tests_nofail tst arg = function
  | [] -> fatal_error "Matching.do_tests_nofail"
  | [_,act] -> act
  | (c,act)::rem ->
      Lifthenelse
        (Lprim (tst, [arg ; Lconst (Const_base c)]),
         do_tests_nofail tst arg rem,
         act)

let make_test_sequence fail tst lt_tst arg const_lambda_list =
  let rec make_test_sequence const_lambda_list =
    if List.length const_lambda_list >= 4 && lt_tst <> Praise then
      split_sequence const_lambda_list
    else match fail with
    | None -> do_tests_nofail tst arg const_lambda_list
    | Some fail -> do_tests_fail fail tst arg const_lambda_list

  and split_sequence const_lambda_list =
    let list1, list2 =
      cut (List.length const_lambda_list / 2) const_lambda_list in
    Lifthenelse(Lprim(lt_tst,[arg; Lconst(Const_base (fst(List.hd list2)))]),
                make_test_sequence list1, make_test_sequence list2)
  in make_test_sequence (sort_lambda_list const_lambda_list)


let make_offset x arg = if x=0 then arg else Lprim(Poffsetint(x), [arg])

let prim_string_notequal =
  Pccall{prim_name = "caml_string_notequal";
          prim_arity = 2; prim_alloc = false;
          prim_native_name = ""; prim_native_float = false}

let rec explode_inter offset i j act k =
  if i <= j then
    explode_inter offset i (j-1) act ((j-offset,act)::k)
  else
    k

let max_vals cases acts =
  let vals = Array.create (Array.length acts) 0 in
  for i=Array.length cases-1 downto 0 do
    let l,h,act = cases.(i) in
    vals.(act) <- h - l + 1 + vals.(act)
  done ;
  let max = ref 0 in
  for i = Array.length vals-1 downto 0 do
    if vals.(i) >= vals.(!max) then
      max := i
  done ;
  if vals.(!max) > 1 then
    !max
  else
    -1

let as_int_list cases acts =
  let default = max_vals cases acts in
  let min_key,_,_ = cases.(0)
  and _,max_key,_ = cases.(Array.length cases-1) in

  let rec do_rec i k =
    if i >= 0 then
      let low, high, act =  cases.(i) in
      if act = default then
        do_rec (i-1) k
      else
        do_rec (i-1) (explode_inter min_key low high acts.(act) k)
    else
      k in
  min_key, max_key,do_rec (Array.length cases-1) [],
  (if default >= 0 then Some acts.(default) else None)


let make_switch_offset arg min_key max_key int_lambda_list default  =
  let numcases = max_key - min_key + 1 in
  let cases =
    List.map (fun (key, l) -> (key - min_key, l)) int_lambda_list in
  let offsetarg = make_offset (-min_key) arg in
  Lswitch(offsetarg,
          {sw_numconsts = numcases; sw_consts = cases;
            sw_numblocks = 0; sw_blocks = [];
            sw_failaction = default})

let make_switch_switcher arg cases acts =
  let l = ref [] in
  for i = Array.length cases-1 downto 0 do
    l := (i,acts.(cases.(i))) ::  !l
  done ;    
  Lswitch(arg,
          {sw_numconsts = Array.length cases ; sw_consts = !l ;
            sw_numblocks = 0 ; sw_blocks =  []  ;
            sw_failaction = None})
    
let full sw =
  List.length sw.sw_consts = sw.sw_numconsts &&
  List.length sw.sw_blocks = sw.sw_numblocks
    
let make_switch (arg,sw) = match sw.sw_failaction with
| None ->
    let t = Hashtbl.create 17 in
    let seen l = match l with
    | Lstaticraise (i,[]) ->
        let old = try Hashtbl.find t i with Not_found -> 0 in
        Hashtbl.replace t i (old+1)
    | _ -> () in
    List.iter (fun (_,lam) -> seen lam) sw.sw_consts ;
    List.iter (fun (_,lam) -> seen lam) sw.sw_blocks ;
    let i_max = ref (-1)
    and max = ref (-1) in
    Hashtbl.iter
      (fun i c ->
        if c > !max then begin
          i_max := i ;
          max := c
        end) t ;
    if !i_max >= 0 then
      let default = !i_max in
      let rec remove = function
        | [] -> []
        | (_,Lstaticraise (j,[]))::rem when j=default ->
            remove rem
        | x::rem -> x::remove rem in
      Lswitch
        (arg,
         {sw with
	  sw_consts = remove sw.sw_consts ;
	  sw_blocks = remove sw.sw_blocks ;
	  sw_failaction = Some (Lstaticraise (default,[]))})
    else
      Lswitch (arg,sw)
| _ -> Lswitch (arg,sw)
      


module SArg = struct
  type primitive = Lambda.primitive

  let eqint = Pintcomp Ceq
  let neint = Pintcomp Cneq
  let leint = Pintcomp Cle
  let ltint = Pintcomp Clt
  let geint = Pintcomp Cge
  let gtint = Pintcomp Cgt

  type act = Lambda.lambda

  let make_prim p args = Lprim (p,args)
  let make_offset arg n = match n with
  | 0 -> arg
  | _ -> Lprim (Poffsetint n,[arg])
  let bind arg body =
    let newvar,newarg = match arg with
    | Lvar v -> v,arg
    | _      ->
        let newvar = Ident.create "switcher" in
        newvar,Lvar newvar in
    bind StrictOpt newvar arg (body newarg)

  let make_isout h arg = Lprim (Pisout, [h ; arg])
  let make_isin h arg = Lprim (Pnot,[make_isout h arg])
  let make_if cond ifso ifnot = Lifthenelse (cond, ifso, ifnot)
  let make_switch = make_switch_switcher
end

module Switcher = Switch.Make(SArg)

open Switch

let lambda_of_int i =  Lconst (Const_base (Const_int i))

let rec last def = function
  | [] -> def
  | [x,_] -> x
  | _::rem -> last def rem

let get_edges low high l = match l with
| [] -> low, high
| (x,_)::_ -> x, last high l

      
let as_interval_canfail fail low high l =
  let store = mk_store equal_action in
  let rec nofail_rec cur_low cur_high cur_act = function
    | [] ->
        if cur_high = high then
          [cur_low,cur_high,cur_act]
        else
          [(cur_low,cur_high,cur_act) ; (cur_high+1,high, 0)]
    | ((i,act_i)::rem) as all ->
        let act_index = store.act_store act_i in
        if cur_high+1= i then
          if act_index=cur_act then
            nofail_rec cur_low i cur_act rem
          else if act_index=0 then
            (cur_low,i-1, cur_act)::fail_rec i i rem
          else
            (cur_low, i-1, cur_act)::nofail_rec i i act_index rem
        else
          (cur_low, cur_high, cur_act)::
          fail_rec ((cur_high+1)) (cur_high+1) all

  and fail_rec cur_low cur_high = function
    | [] -> [(cur_low, cur_high, 0)]
    | (i,act_i)::rem ->
        let index = store.act_store act_i in
        if index=0 then fail_rec cur_low i rem
        else
          (cur_low,i-1,0)::
          nofail_rec i i index rem in

  let rec init_rec = function
    | [] -> []
    | (i,act_i)::rem ->
        let index = store.act_store act_i in
        if index=0 then
          fail_rec low i rem
        else
          if low < i then
            (low,i-1,0)::nofail_rec i i index rem
          else
            nofail_rec i i index rem in

  ignore (store.act_store fail) ; (* fail has action index 0 *)
  let r = init_rec l in
  Array.of_list r,  store.act_get ()

let as_interval_nofail l =
  let store = mk_store equal_action in

  let rec i_rec cur_low cur_high cur_act = function
    | [] ->
        [cur_low, cur_high, cur_act]
    | (i,act)::rem ->
        let act_index = store.act_store act in
        if act_index = cur_act then
          i_rec cur_low i cur_act rem
        else
          (cur_low, cur_high, cur_act)::
          i_rec i i act_index rem in
  let inters = match l with
  | (i,act)::rem ->
      let act_index = store.act_store act in
      i_rec i i act_index rem
  | _ -> assert false in

  Array.of_list inters, store.act_get ()


let sort_int_lambda_list l =
  List.sort
    (fun (i1,_) (i2,_) ->
      if i1 < i2 then -1
      else if i2 < i1 then 1
      else 0)
    l

let as_interval fail low high l =
  let l = sort_int_lambda_list l in
  get_edges low high l,
  (match fail with
  | None -> as_interval_nofail l
  | Some act -> as_interval_canfail act low high l)

let call_switcher konst fail arg low high int_lambda_list =
  let edges, (cases, actions) =
    as_interval fail low high int_lambda_list in
  Switcher.zyva edges konst arg cases actions


(* Stubs for all cases of switch *)
let switch_constant cst arg const_lambda_list fail = match cst with
| Const_int _ ->
    let int_lambda_list =
      List.map (function Const_int n, l -> n,l | _ -> assert false)
        const_lambda_list in
    call_switcher
      lambda_of_int fail arg min_int max_int int_lambda_list
| Const_char _ ->
    let int_lambda_list =
      List.map (function Const_char c, l -> (Char.code c, l)
        | _ -> assert false)
        const_lambda_list in
    call_switcher
      (fun i -> Lconst (Const_base (Const_int i)))
      fail arg 0 255 int_lambda_list
| Const_string _ ->
    make_test_sequence
      fail prim_string_notequal Praise arg const_lambda_list
| Const_float _ ->
    make_test_sequence
      fail
      (Pfloatcomp Cneq) (Pfloatcomp Clt)
      arg const_lambda_list
| Const_int32 _ ->
    make_test_sequence
      fail
      (Pbintcomp(Pint32, Cneq)) (Pbintcomp(Pint32, Clt))
      arg const_lambda_list
| Const_int64 _ ->
    make_test_sequence
      fail
      (Pbintcomp(Pint64, Cneq)) (Pbintcomp(Pint64, Clt))
      arg const_lambda_list
| Const_nativeint _ ->
    make_test_sequence
      fail
      (Pbintcomp(Pnativeint, Cneq)) (Pbintcomp(Pnativeint, Clt))
      arg const_lambda_list

(* Exceptions *)
let switch_exn arg cls fail =
  let default, tests =
    match fail with
    | None ->
        begin match cls with
        | (_, act)::rem -> act,rem
        | _ -> assert false
        end
    | Some fail -> fail, cls in
  List.fold_right
    (fun (ex, act) rem ->
      match ex with
      | Cstr_exception path ->
          Lifthenelse(Lprim(Pintcomp Ceq,
                            [Lprim(Pfield 0, [arg]); transl_path path]),
                      act, rem)
      | _ -> assert false)
    tests default

(* Data types *)
let split_cases tag_lambda_list =
  let rec split_rec = function
      [] -> ([], [])
    | (cstr, act) :: rem ->
        let (consts, nonconsts) = split_rec rem in
        match cstr with
          Cstr_constant n -> ((n, act) :: consts, nonconsts)
        | Cstr_block n    -> (consts, (n, act) :: nonconsts)
        | _ -> assert false in
  let const, nonconst = split_rec tag_lambda_list in
  sort_int_lambda_list const,
  sort_int_lambda_list nonconst
    

let switch_constr cstr arg cls fail =
  let (consts, nonconsts) = split_cases cls in
  match same_actions cls,fail with
  | Some act,None -> act
  | _,_ ->
      begin match
        (cstr.cstr_consts, cstr.cstr_nonconsts, consts, nonconsts,fail)
      with
      | (1, 1, [0, act1], [0, act2],None)
      | (1, 1, [0,act1], [], Some act2)
      | (1, 1, [], [0,act2], Some act1) -> (* eg. list *)
          Lifthenelse(arg, act2, act1)
      | (n,0,_,[],_)|(n,_,_,[],None) -> (* cstr only *)
	  call_switcher
            (fun i -> Lconst (Const_base (Const_int i)))
            fail arg 0 (n-1) consts
      | (n,_,_,_,_) ->
	  begin match same_actions nonconsts,fail with
	  | Some act,None ->
	      Lifthenelse
		(Lprim (Pisint, [arg]),
		 call_switcher
		   (fun i -> Lconst (Const_base (Const_int i)))
		   fail arg 0 (n-1) consts,
		 act)
	  | _,_ ->
              make_switch(arg, {sw_numconsts = cstr.cstr_consts;
				sw_consts = consts;
				sw_numblocks = cstr.cstr_nonconsts;
				sw_blocks = nonconsts;
				sw_failaction = fail})
	  end
      end
      
(* Variants *)
let make_test_sequence_variant_constant fail arg int_lambda_list =
  let _, (cases, actions) =
    as_interval fail min_int max_int int_lambda_list in
  Switcher.test_sequence
    (fun i -> Lconst (Const_base (Const_int i))) arg cases actions

let call_switcher_variant_constant fail arg int_lambda_list =
  call_switcher
    (fun i -> Lconst (Const_base (Const_int i)))
    fail arg min_int max_int int_lambda_list

let call_switcher_variant_constr fail arg int_lambda_list =
  let v = Ident.create "variant" in
  Llet(StrictOpt, v, Lprim(Pfield 0, [arg]),
       call_switcher
         (fun i -> Lconst (Const_base (Const_int i)))
         fail (Lvar v) min_int max_int int_lambda_list)

let test_int_or_block arg if_int if_block =
  Lifthenelse(Lprim (Pisint, [arg]), if_int, if_block)

let switch_variant arg cls fail =
  let one_action = same_actions cls in
  let (consts, nonconsts) = split_cases cls in
  match one_action,fail with
  | Some act,None -> act
  | _,_ ->
      match (consts, nonconsts) with
      | ([n, act1], [m, act2]) when fail=None ->
          test_int_or_block arg act1 act2
      | (_, []) -> (* One can compare integers and pointers *)
          make_test_sequence_variant_constant fail arg consts
      | ([], _) ->
          let lam = call_switcher_variant_constr
              fail arg nonconsts in
          (* One must not dereference integers *)
          begin match fail with
          | None -> lam
          | Some fail -> test_int_or_block arg fail lam
          end
      | (_, _) ->
          let lam_const =
            call_switcher_variant_constant
              fail arg consts
          and lam_nonconst =
            call_switcher_variant_constr
              fail arg nonconsts in
          test_int_or_block arg lam_const lam_nonconst

(* Array *)

let switch_array kind arg cls fail =
  let newvar = Ident.create "len" in
  let switch =
    call_switcher
      lambda_of_int
      fail (Lvar newvar)
      0 max_int cls in
  bind StrictOpt newvar (Lprim(Parraylength kind, [arg])) switch


(******************)
(* Last utilities *)
(******************)

let partial_function loc () =
  (* [Location.get_pos_info] is too expensive *)
  let fname = match loc.Location.loc_start.Lexing.pos_fname with
              | "" -> !Location.input_name
              | x -> x
  in
  let pos = loc.Location.loc_start in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Lprim(Praise, [Lprim(Pmakeblock(0, Immutable),
          [transl_path Predef.path_match_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string fname);
               Const_base(Const_int line);
               Const_base(Const_int char)]))])])
