(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type iext = TooMuch | Int of int

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

   val default : act
   val bind : act -> (act -> act) -> act
   val make_offset : act -> int -> act
   val make_prim : primitive -> act list -> act
   val make_isout : act -> act -> act
   val make_if : act -> act -> act -> act
   val make_switch :
      act -> (int * int * int) array -> act array -> act
 end

module Make (Arg : S) =
  struct
    type l_status = Linter | Lsimple
    type t_status =
        Linear of l_status | Switch | ToCluster | Empty

    let string_of_status = function
      | Linear Linter -> "Linter"
      | Linear Lsimple -> "L"
      | Empty -> "E"
      | Switch -> "S"
      | ToCluster -> "?"


    type 'a inter =
        {low : iext ; high : iext ;
          icases : (int * int * int) array ;
          iacts : 'a array ;
          status : t_status}

    let prerr_icases t =
      prerr_string "{ " ;
      for i = 0 to Array.length t-1 do
        let l,h,act = t.(i) in
        Printf.fprintf stderr "(%d,%d,%d) " l h act
      done ;
      prerr_string "}"


    let string_of_iext = function
      | TooMuch -> "oo"
      | Int i   -> string_of_int i

    let prerr_inter i =
      Printf.fprintf stderr
        "status=%s, low=%s, high=%s, cases="
        (string_of_status i.status)
        (string_of_iext i.low) (string_of_iext i.high) ;
      prerr_icases i.icases

    let inter_default _ = function
      | 0 -> true
      | _ -> false

    let is_closed i = match i.low, i.high with
    | Int _, Int _ -> true
    | _,_          -> false

    type 'a t_ctx = 
      {ctx_low : iext ; ctx_high : iext ; off : int ;
        arg : 'a}

let find_staticfail _ = 0

(*
let as_checked i = match i.low, i.high with
| Int _, Int _ ->
    let cases = i.icases in
    let len = Array.length cases in
    let l0,h0,a0 = cases.(0)
    and ln,hn,an = cases.(len-1) in
    if inter_default i a0 && inter_default i an then
      {i with low=TooMuch ; high=TooMuch ;
       icases=Array.sub cases 1 (len-2)}
    else
      i
| TooMuch,Int _ ->
    let cases = i.icases in
    let len = Array.length cases in
    let ln,hn,an = cases.(len-1) in
    if inter_default i an then
      {i with high=TooMuch ; icases = Array.sub cases 0 (len-1)}
    else
      i
| Int _,TooMuch ->
    let cases = i.icases in
    let len = Array.length cases in
    let l0,h0,a0 = cases.(0) in
    if inter_default i a0 then
      {i with low=TooMuch ; icases = Array.sub cases 1 (len-1)}
    else
      i
| _,_ -> i
*)




let ninters {low=low ; high=high ; icases = cases} =
  Array.length cases +
    (match low,high with
    | Int _, Int _ -> 0
    | _,_          -> 1)

let min_key i = match i.low with
| TooMuch ->
    let low,_,_ = i.icases.(0) in
    low
| Int low -> low

and max_key i =  match i.high with
| TooMuch ->
    let _,high,_ = i.icases.(Array.length i.icases-1) in
    high
| Int high -> high

let nlabels i =  max_key i/4 - min_key i/4

let count_bornes i =  if is_closed i then 0 else 1

exception NoSuch

let single_values i =
  let singles = ref []
  and def = ref None
  and cases = i.icases in
  for i = 0 to Array.length cases-1 do
    let low,high,act = cases.(i) in
    if low=high then begin
      match !def with
      | Some def when def=act -> ()
      | _ ->
          singles := (low,act) :: !singles
    end else match !def with
    | None ->
        def := Some act ;
        singles :=
           List.filter (fun (_,act0) -> act0 <> act) !singles
    | Some def ->
        if def <> act then raise NoSuch
  done ;
  match i.low,i.high,!def,!singles with
  | Int _, Int _, None,(_,x)::r -> r,x
  | Int _, Int _, Some x,r  -> r,x
  | _,_,Some x,r when inter_default i x -> r,x
  | _,_,None,r              -> r,find_staticfail i
  | _,_,_,_                 -> raise NoSuch  

let count_by_action i =
  let low = i.low and high = i.high in
  let t = Array.create (Array.length i.iacts) (0,0,0) in
  let add l h act =
    let old_n,old_itests,old_ztests = t.(act) in    
    t.(act) <-
       (old_n+1,
        old_itests +
          (if l=h then 0
          else if Int l = low then 0
          else if Int h = high then 0
          else 1),
        old_ztests +
          (if l=h && Int l = low then 1 else 0)) in
  Array.iter (fun (l,h,act) -> add l h act) i.icases ;
  t

and group_by_action i =
  let t = Array.create (Array.length i.iacts) [] in
  let add l h act = t.(act) <- (l,h)::t.(act) in
  Array.iter (fun (l,h,act) -> add l h act) i.icases ;
  t

and low_action i =
  let _,_,act = i.icases.(0) in
  act

and high_action i =
  let cases = i.icases in
  let _,_,act = cases.(Array.length cases-1) in
  act

let array_iteri_rev f t =
  for i = Array.length t-1 downto 0 do
    f i t.(i)
  done

exception Found of int

let inter_values i =
  if is_closed i then begin

    let find_max t =
      let max = ref (-1) and max_itests = ref (-1) and max_ztests = ref (-1)
      and max_act = ref (-1) in
      array_iteri_rev
        (fun act (n,itests,ztests) ->
          if
            n > !max ||(* choose action with maximum number of intervals *)
                   (* then with maximal number of actual interval tests *)
            (n = !max &&  itests > !max_itests) ||
                   (* then with minimal number of tests against zero *)
            (n = !max && itests = !max_itests && ztests < !max_ztests)
          then begin
            max := n ;
            max_itests := itests ;
            max_ztests := ztests ;
            max_act := act
          end) t ;
      !max_act in
    
    let max_act = find_max (count_by_action i) in
    List.filter
      (fun (l,h,act) -> act <> max_act)
      (Array.to_list i.icases),
    max_act

  end else
    List.filter
      (fun (l,h,act) -> not (inter_default i act))
      (Array.to_list i.icases),
    find_staticfail i

  
let count_tests i = match i.icases with
| [| _ |] -> count_bornes i, Lsimple
| _ ->
  let count_inter =
    try
      let l,_ = inter_values i in
      List.length l
    with
    | NoSuch -> 1000

  and count_simple =
    let  cases,low,high = i.icases, i.low, i.high in
    let n = Array.length cases-1 in
    n + count_bornes i in

  if count_inter <=  count_simple then
    count_inter, Linter
  else
    count_simple, Lsimple
  

let make_if_test konst test arg i ifso ifnot =
  Arg.make_if
    (Arg.make_prim test [arg.arg ; konst (i+arg.off)])
    ifso ifnot

let inter_ctx off l h arg =
  {off=off ; ctx_low = Int l ; ctx_high = Int h ; arg = arg}

let make_if_inter konst arg l h mk_ifin ifout =
  if l=h then
    make_if_test konst Arg.neint arg l ifout
      (mk_ifin (inter_ctx arg.off l h arg.arg))
  else
    let new_off = arg.off-l in
    Arg.bind
      (Arg.make_offset arg.arg (-l))
      (fun arg ->
        Arg.make_if
          (Arg.make_isout (konst (h-l)) arg)
          ifout (mk_ifin (inter_ctx new_off l h arg)))

and make_if_inter_last  konst arg l h mk_ifin ifout =
  if l=h then
    make_if_test konst Arg.eqint arg l 
      (mk_ifin (inter_ctx arg.off l h arg.arg))
      ifout
  else
    let new_off = arg.off-l in
    Arg.bind
      (Arg.make_offset arg.arg (-l))
      (fun arg ->
        Arg.make_if
          (Arg.make_isout (konst (h-l)) arg)
          ifout (mk_ifin (inter_ctx new_off l h arg)))

let make_inters_ifs  konst arg ({iacts = acts} as i) =
  try
    let l,def = inter_values i in
    let rec if_rec arg = function
      | [] -> acts.(def) arg
      | (l1,h1,act1)::rem ->
          if Int l1 = arg.ctx_low then
            make_if_test konst (if l1=h1 then Arg.neint else Arg.gtint) arg  h1
              (if_rec  {arg with ctx_low=Int (h1+1)} rem)
              (acts.(act1) arg)
          else if Int h1 = arg.ctx_high then
            make_if_test konst (if l1=h1 then Arg.neint else Arg.ltint) arg  l1
              (if_rec {arg with ctx_high = Int (l1-1)} rem)
              (acts.(act1) arg)
          else
            make_if_inter konst arg  l1 h1 acts.(act1) (if_rec arg rem) in
    if_rec arg l
  with
  | NoSuch -> assert false

                     
    let make_linear_ifs l_status konst arg ({iacts = acts} as i) =
      match l_status with
      | Linter -> make_inters_ifs  konst arg i
      | Lsimple ->
          let  cases,low,high = i.icases,arg.ctx_low,arg.ctx_high in
          let n = Array.length cases-1 in
          let rec do_rec arg i =
            if i=n then
              let _,_,act = cases.(i) in
              acts.(act) arg
            else
              let _,high,act = cases.(i) in
              make_if_test konst
                Arg.leint arg high (acts.(act) arg)
                (do_rec arg (i+1)) in
          match low,high with
          | TooMuch, TooMuch ->
              let l = min_key i
              and h = max_key i in
              make_if_inter konst arg l h (fun arg -> do_rec arg 0) Arg.default
          | TooMuch,_ ->
              let l = min_key i in
              make_if_test konst Arg.ltint arg l Arg.default (do_rec arg 0)
          | _, TooMuch ->
              let h = max_key i in
              make_if_test konst Arg.gtint arg h Arg.default (do_rec arg 0)
          | _,_ -> do_rec arg 0 

let special_case i = match i.low, i.high with
| Int 0, Int 2 -> begin match i.icases with
  | [| (0,0,act1) ; (1,1,act2)  ; (2,2,act3) |] -> act1 <> act3
  | _ -> false
end
| _ -> false
  


exception Ends
exception NoCut of t_status
(*
let debug = ref false
*)

let cut_here i =
  let c_if, l_status = count_tests i in
(*
  if !debug then
    Printf.fprintf stderr "Attempt: %d as %s\n" c_if
      (string_of_status (Linear l_status)) ;
*)
  if c_if=0 then raise (NoCut Empty) ;
  if special_case i then raise (NoCut Switch) ;
  if c_if - count_bornes i <=  !Clflags.limit_switch then
    raise (NoCut (Linear l_status)) ;
  let icases = i.icases in
  let len = Array.length icases
  and c_switch = nlabels i + 1 in
  if c_switch <= c_if  then raise (NoCut Switch) ;

  let r = ref (-1) and max = ref (-1) in
  for j = 0 to len-1 do
    let low,high,_ = icases.(j) in
    if high-low+1 > !max then begin
      max := high-low ;
      r := j
    end
  done ;
  if len > 2 then begin
    let l0,h0,act0 = icases.(0)
    and ln,hn,actn = icases.(len-1) in
    if
      act0 = actn &&
      (h0-l0+hn-ln+2 > !max)
    then
      raise Ends
  end ;
  !r

let sub_cases from_here len cases =
  if len <= 0 then [||]
  else
    Array.sub cases from_here len
   
let present act i len cases =
  let rec do_rec i =
    if i < len then
      let _,_,act0 = cases.(i) in
      act0=act || do_rec (i+1)
    else
      false in
  do_rec i



let explode_linear i k =
  let acts = i.iacts
  and cases = i.icases in
  let last = Array.length cases-1 in

  let rec explode_rec j = match last-j with
    | 0 ->
        let (l,_,_) as x = cases.(j) in
        {i with low = Int l ; icases = [| x |] ; status = Empty}::k
    | _ ->
        let (l,h,_) as x = cases.(j) in
        {i with low = Int l ; high = Int h ;
           icases = [| x |] ; status = Empty}::
        explode_rec (j+1) in
        
  
  match cases with
  | [| |] | [| _ |] -> {i with status=Empty}::k
  | _ ->
      let (_,h0,_) as x = cases.(0) in
      {i with high = Int h0 ; icases = [| x |] ; status = Empty}::
      explode_rec 1

let rec do_cluster i k =
(*
  if !debug then begin
    prerr_string "++++++++++++++++\nCluster " ; prerr_inter i ;
    prerr_endline ""
  end ;
*)
  let cases = i.icases in
  if i.high = TooMuch && inter_default i (low_action i) then
    let l0,h0,act0 = cases.(0) in
    let rest = sub_cases  1 (Array.length cases-1) cases in
    {i with high=Int h0 ; icases = [| cases.(0) |] ; status=Empty}::
    do_cluster
      {i with low=Int (h0+1) ; icases = rest}
      k
  else
  try
    match cases with
    | [| _,_,act |] ->
        if is_closed i || inter_default i act then
          {i with status=Empty}::k
        else
          let _,status = count_tests i in
          raise (NoCut (Linear status))
    | _ ->
    let j = cut_here i in

    let c_low,c_high,c_act = cases.(j) in
    if false (* c_low=c_high *) then begin
      let left,right =
        if j=0 || present c_act 0 j cases then
          sub_cases 0 (j+1) cases,
          sub_cases (j+1) (Array.length cases-j-1) cases
        else
          sub_cases 0 j cases,
          sub_cases j (Array.length cases-j) cases in
(*
      if !debug then begin
        prerr_string "Left = " ; prerr_icases left ; prerr_endline "" ;
        prerr_string "Right = " ; prerr_icases right ; prerr_endline ""
      end ;
*)
      do_cluster
        {i with high = Int (c_low-1) ; icases=left}
        (do_cluster
           {i with low = Int c_low ; icases=right} k)
    end else begin
      let left = sub_cases 0 j cases
      and center = [| cases.(j) |]
      and right = sub_cases (j+1) (Array.length cases-j-1) cases in
(*
      if !debug then begin
        prerr_string "Left = " ; prerr_icases left ; prerr_endline "" ;
        prerr_string "Center = " ; prerr_icases center ; prerr_endline "" ;
        prerr_string "Right = " ; prerr_icases right ; prerr_endline ""
      end ;
*)
      if j=0 then
        {i with low=i.low ; high = Int c_high ;
         icases = center ; status=Empty}::
        do_cluster
          {i with low = Int (c_high+1) ; high=i.high ; icases = right} k
      else if j = Array.length cases-1 then
        do_cluster
          {i with low = i.low ; high= Int (c_low-1) ; icases = left}
          ({i with low = Int c_low ; high = i.high ;
            icases=center ; status=Empty}::k)
      else
        do_cluster
          {i with low = i.low ; high= Int (c_low-1) ; icases = left}
          ({i with low = Int c_low ; high = Int c_high ;
            icases=center ; status=Empty}::
           do_cluster
             {i with low = Int (c_high+1) ; high=i.high ; icases = right}
             k)
    end
with
| NoCut status ->
(*
    if !debug then
      Printf.fprintf stderr "%s\n" (string_of_status status) ;
*)
    begin match status with
    | Linear _ -> explode_linear i k
    | _ -> {i with status=status}::k
    end
| Ends ->
    let cases = i.icases in
    let len = Array.length cases in
    let _,h0,act0 = cases.(0)
    and center = sub_cases 1 (len-2) cases
    and ln,_,actn = cases.(len-1) in

(*
    if !debug then begin
      prerr_string "Left = " ; prerr_icases [| cases.(0) |] ;
      prerr_endline "" ;
      prerr_string "Center = " ; prerr_icases center ; prerr_endline "" ;
      prerr_string "Right = " ; prerr_icases [| cases.(len-1) |] ;
      prerr_endline ""
    end ;
*)

    {i with high = Int h0 ; status = Empty ; icases = [| cases.(0) |]}::
    do_cluster
      {i with low = Int (h0+1) ; high = Int (ln-1) ; icases = center}
      ({i with low = Int ln ; status = Empty ; icases = [| cases.(len-1) |]}::k)


let do_merge_clusters i1 i2 =
  {low=i1.low ; high = i2.high ;
    icases = Array.append i1.icases i2.icases ;
    iacts= i1.iacts ;
    status = ToCluster}


exception NoMerge

let merge_clusters i1 i2 = match i1.status, i2.status with
| Linear _, Linear _  -> do_merge_clusters  i1 i2
| _,_                -> raise NoMerge

let simpl_clusters l =
  match l with
  | [] -> l
  | [_] -> l
  | _ ->
(*
      if !debug then begin
        prerr_endline "------------------- Clusters --------------" ;
        List.iter
          (fun i -> prerr_inter i ; prerr_endline "") l
      end ;
*)
      l

let cluster i =
  
  simpl_clusters (do_cluster i [])



let fail_out inter =
  let t = inter.icases in
  let j = ref 1
  and len = Array.length t in
  let new_low = 
    let _,high,act0 as all0 = t.(0) in
    if inter_default inter act0 then begin
      t.(0) <- t.(1) ;
      Int (high+1)
    end else  begin
      inter.low 
    end in

  for i = 1 to Array.length t-1 do
    let (_,high,act as all) = t.(i)
    and low0,_,act0 = t.(!j-1) in
    if inter_default inter act ||  act0=act then
      t.(!j-1) <- low0, high, act0
    else begin
      t.(!j) <- all ;
      incr j
    end 
  done ;
  let new_t =
    if !j <> len then
      Array.sub t 0 !j
    else
      t in
  let _,new_high,_ = new_t.(!j-1) in
  {inter with low = new_low ; high = Int new_high ; icases = new_t}
      

let as_int_int_acts i =
  let acts = i.iacts in
  Array.map
    (fun (l,h,act) -> (l,h,acts.(act)))
    i.icases

let comp_leaf konst  arg i =  match i.status with
  | Linear l_status -> make_linear_ifs l_status konst arg i
  | Empty ->
      let _,_,act = i.icases.(0) in
      i.iacts.(act) arg
  | Switch ->
    let min_key = min_key i in
    let mk_switch arg =
      let acts = Array.map (fun act -> act arg) i.iacts in
      Arg.make_switch arg.arg i.icases acts in
    mk_switch {arg with arg = Arg.make_offset arg.arg (-arg.off-min_key)}

  | ToCluster -> Misc.fatal_error  "Matching.comp_leaf"


type 'a action =  | Unique of 'a | Shared of int * 'a


let same_cluster_action c1 c2 = match c1.status, c2 with
| Empty, Shared (i2,_) ->  low_action c1=i2
| _,_ -> false

let cluster_clusters konst arg cls =
  let actions = ref [Shared (0, cls.(0).iacts.(0))]
  and n_actions = ref 1 in
  let rec store_rec  act i = function
    | [] -> begin match act.status with 
      | Empty ->
          let index = low_action act in
          [Shared (index, act.iacts.(index))]
      | _ -> [Unique (fun arg -> comp_leaf konst arg act)]
    end
    | act0::rem ->
        if same_cluster_action act act0 then
          raise (Found i)
        else
          act0::store_rec act (i+1) rem in
  let store act =
    try
      actions := store_rec act 0 !actions ;
      let r = !n_actions in
      incr n_actions ;
      r
    with
    | Found i -> i in
  let cases =
    Array.map
      (fun c -> min_key c, max_key c,store c) cls in
  let low = cls.(0).low
  and high = cls.(Array.length cls-1).high in
  {high = high ; low = low ;
    icases = cases ;
    iacts = Array.map
      (function
        | Unique act -> act
        | Shared (_,act) -> act)
      (Array.of_list !actions) ;
    status = ToCluster}
      
  
let final_tests konst arg cl =

  let rec comp_tree cl =
  let n,status = count_tests cl in

(*
  if !debug then begin
    prerr_inter cl  ;
    Printf.fprintf stderr "\nFinally : %d tests as %s\n" n
      (string_of_status (Linear status)) ;
    flush stderr
  end ;
*)

  if n <= !Clflags.limit_tree then
    comp_leaf konst
      {arg with ctx_low = cl.low ; ctx_high = cl.high}
      {cl with status = Linear status}
  else
    let cases = cl.icases in
    let len = Array.length cases in
    let half = match cl.low, cl.high with
    | TooMuch,Int _ -> (len-1)/2
    | Int _, TooMuch -> (len+1)/2
    | _,_ -> len/2 in
    let left = sub_cases 0 half cases
    and right = sub_cases half (len-half) cases in
    let _,key,_ = left.(half-1) in
      make_if_test konst
        Arg.leint arg key
          (comp_tree {cl with high=Int key ; icases = left})
          (comp_tree {cl with low=Int (key+1) ; icases=right}) in

  comp_tree cl 

  
  
let comp_clusters konst arg l =
  let cls = Array.of_list l in
  let cl = cluster_clusters konst arg cls in
  final_tests konst arg cl

let comp_inter konst arg i = comp_clusters konst arg (cluster i)


let zyva konst arg low high cases acts =
    let cl = 
      {low = low ; high = high ;
        icases = cases  ;
        iacts=Array.map (fun act -> (fun _ -> act)) acts ;
        status = ToCluster} in
(*
    let old_debug = !debug in
    if fst (count_tests cl) > 2 then debug := true ;
    if !debug then begin
      prerr_endline "******** zyva **********" ;
      prerr_inter cl ;
      prerr_endline ""
    end ;
*)
    let r = comp_inter konst
          {ctx_low=low ; ctx_high=high ; off=0 ; arg=arg} cl in
(*
    if !debug then prerr_endline "************************" ;
    debug := old_debug ;
*)
    r

  end
