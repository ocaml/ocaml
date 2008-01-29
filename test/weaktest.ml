(* $Id$ *)

let ( =%= ) = ( = );;
let ( = ) = ();;

type expr =
  | LL of private_info * string list * expr
  | VV of private_info * string
  | AA of private_info * string * expr list

and private_info = {
  hash : int;
  skel_hash : int;
  vars_hash : (string * int) list;
  size : int;
};;


(************************)
(* small sets of formulas (represented as lists) *)

let rec diff l1 l2 =
  match l1 with
  | [] -> []
  | e::t -> if List.exists ((==) e) l2
            then diff t l2
            else e :: (diff t l2)
;;

let union l1 l2 = List.rev_append (diff l1 l2) l2;;

let rec disjoint l1 l2 =
  match l1 with
  | [] -> true
  | h::t -> if List.exists ((==) h) l2
            then false
            else disjoint t l2
;;

(*******************)

let k0 = 0x798764da;;
let k1 = 0x6f75849b;;
let k2 = 0x4c14862c;;
let k3 = 0x72a7baf3;;
let k4 = 0x7542122c;;
let k5 = 0x74a318d5;;
let k6 = 0x7eb1b2dc;;
let k7 = 0x4bea2543;;
let k8 = 0x297e236d;;
let k9 = 0x47c6ad26;;

let combine k h1 h2 =
  let x = k lxor h1 in
  x + (x lsl 1) + (x lsl 8) + (x lsr 16) + h2
;;

let mkhash skel vars =
  let cmb accu (v, h) = combine k0 accu (combine k0 (Hashtbl.hash v) h) in
  List.fold_left cmb skel vars
;;

let mkpriv skel vars sz = {
  hash = mkhash skel vars;
  skel_hash = skel;
  vars_hash = vars;
  size = sz;
};;

let get_priv = function
  | LL (h, _, _) -> h
  | VV (h, _) -> h
  | AA (h, _, _) -> h
;;

let get_hash e = (get_priv e).hash;;
let get_skel e = (get_priv e).skel_hash;;
let get_vars e = (get_priv e).vars_hash;;
let get_size e = (get_priv e).size;;

let rec str_union l1 l2 =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | h::t, _ when List.exists ((=%=) h) l2 -> str_union t l2
  | h::t, _ -> str_union t (h :: l2)
;;

let rec str_remove x l =
  match x, l with
  | _, [] -> []
  | v, h::t when v =%= h -> t
  | _, h::t -> h :: (str_remove x t)
;;

let is_const s =
  match s.[0] with
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> false
  | _ -> true
;;

let priv_var s =
  if is_const s
  then mkpriv (Hashtbl.hash s) [] 1
  else mkpriv k1 [(s, k2)] 1
;;

let rec comb_vars accu (cur, v1) v2 =
  match v1, v2 with
  | [], [] -> (combine k3 cur 0, List.rev accu)
  | (x1, h1) :: t1, [] ->
      comb_vars ((x1, combine k3 h1 0) :: accu) (cur, t1) []
  | (x1, h1) :: t1, (x2, h2) :: t2 when x1 < x2 ->
      comb_vars ((x1, combine k3 h1 0) :: accu) (cur, t1) v2
  | [], (x2, h2) :: t2 ->
      comb_vars ((x2, combine k3 cur h2) :: accu) (cur, []) t2
  | (x1, h1) :: t1, (x2, h2) :: t2 when x1 > x2 ->
      comb_vars ((x2, combine k3 cur h2) :: accu) (cur, v1) t2
  | (x1, h1) :: t1, (x2, h2) :: t2 ->
      assert (x1 =%= x2);
      comb_vars ((x1, combine k3 h1 h2) :: accu) (cur, t1) t2
;;

let priv_app s args =
  let (skel_base, vars_base) =
    if is_const s
    then (Hashtbl.hash s, [])
    else (0, [(s, k5)])
  in
  let comb_skel accu e = combine k4 accu (get_skel e) in
  let skel = List.fold_left comb_skel skel_base args in
  let arg_vars = List.map get_vars args in
  let (_, vars) = List.fold_left (comb_vars []) (0, vars_base) arg_vars in
  let sz = List.fold_left (fun a e -> a + get_size e) 1 args in
  mkpriv skel vars sz
;;

let rec remove accu vs evars =
  match vs, evars with
  | [], l -> List.rev_append accu l
  | l, [] -> List.rev accu
  | v1 :: t1, (v2, h2) :: t2 when v1 < v2 -> remove accu t1 evars
  | v1 :: t1, ((v2, h2) as vh) :: t2 when v1 > v2 -> remove (vh :: accu) vs t2
  | v1 :: t1, (v2, h2) :: t2 -> remove accu t1 t2
;;

let rec assoc0 key l =
  match l with
  | [] -> 0
  | (s, h) :: t when s =%= key -> h
  | _ :: t -> assoc0 key t
;;

let priv_lam vs e =
  assert (not (List.exists is_const vs));
  let evars = get_vars e in
  let vars = remove [] (List.sort compare vs) evars in
  let cmb accu v = combine k6 accu (assoc0 v evars) in
  let skel = List.fold_left cmb (get_skel e) vs in
  mkpriv skel vars (1 + get_size e)
;;


module HashedExpr = struct
  type t = expr;;

  let hash = get_hash;;

  type binding = Bound of int | Free of string;;

  let get_binding env v =
    let rec index i v env =
      match env with
      | x :: _ when x =%= v -> Bound i
      | _ :: t -> index (i+1) v t
      | [] -> Free v
    in
    index 0 v env
  ;;

  let same_binding env1 v1 env2 v2 =
    match (get_binding env1 v1), (get_binding env2 v2) with
    | Bound i1, Bound i2 -> i1 =%= i2
    | Free w1, Free w2 -> w1 =%= w2
    | _, _ -> false
  ;;

  let var_name v =
    match v with
    | VV (name, _) -> name
    | _ -> assert false
  ;;

  let rec equal_in_env env1 env2 e1 e2 =
    match e1, e2 with
    | VV (_, s1), VV (_, s2) -> same_binding env1 s1 env2 s2
    | AA (_, f1, args1), AA (_, f2, args2) ->
        f1 =%= f2 && List.length args1 =%= List.length args2
        && List.for_all2 (equal_in_env env1 env2) args1 args2
    | LL (_, vs1, b1), LL (_, vs2, b2) ->
        List.length vs1 =%= List.length vs2
        && equal_in_env (vs1 @ env1) (vs2 @ env2) b1 b2
    | _, _ -> false
  ;;

  let equal e1 e2 =
    match e1, e2 with
    | VV (_, v1), VV (_, v2) -> v1 =%= v2
    | AA (_, f1, args1), AA (_, f2, args2) ->
        f1 =%= f2 && List.length args1 =%= List.length args2
        && List.for_all2 (==) args1 args2
    | LL (_, v1, b1), LL (_, v2, b2)
      -> v1 =%= v2 && b1 == b2
         || List.length v1 =%= List.length v2 && equal_in_env v1 v2 b1 b2
    | _, _ -> false
  ;;
end;;

module HE = Weak.Make (HashedExpr);;
let tbl = HE.create 7;;

let generated = ref 0;;
let he_merge tbl x =
  begin try HE.find tbl x
  with Not_found ->
    HE.add tbl x;
    incr generated;
    x
  end

let vv (s) = he_merge tbl (VV (priv_var s, s));;
let aa (f, args) = he_merge tbl (AA (priv_app f args, f, args));;
let ll (v, e) = he_merge tbl (LL (priv_lam v e, v, e));;

module Expr = struct
  type t = expr;;
  let hash = get_hash;;
  let equal = (==);;
  let compare x y =
    match compare (hash x) (hash y) with
    | 0 -> if equal x y then 0 else Pervasives.compare x y
    | x when x < 0 -> -1
    | _ -> 1
  ;;
end;;

(************************)

let buflen = 20;;
let small = 5;;
let iter1 = 1000;;
let iter2 = 1000;;

let buffer = Array.make buflen (vv "x");;
let live = ref [];;

let rnd_small () =
  let rec log x = if x =%= 0 then 0 else 1 + log (x / 2) in
  small - log (Random.int (1 lsl (small-1)))
;;

let rnd_list gen =
  let rec loop i = if i =%= 0 then [] else gen () :: loop (i - 1) in
  loop (rnd_small ())
;;

let rnd_var () = String.make 1 (Char.chr (Char.code 'a' + Random.int 26));;

let rec rm_dup l =
  match l with
  | [] | [ _ ] -> l
  | x :: t -> if List.mem x t then rm_dup t else x :: (rm_dup t)
;;

let rnd_expr () =
  let result =
    match Random.int 3 with
    | 0 ->
        let vars = rm_dup (rnd_list rnd_var) in
        let body = buffer.(Random.int buflen) in
        ll (vars, body)
    | 1 -> vv (rnd_var ())
    | _ ->
        let f = rnd_var () in
        let args = rnd_list (fun () -> buffer.(Random.int buflen)) in
        aa (f, args)
  in
  buffer.(Random.int buflen) <- result;
  result
;;

module H = Hashtbl.Make (Expr);;

let seen = H.create 7;;

let rec count e =
  if H.mem seen e then begin
    (*Printf.eprintf "shared\n"; flush stderr;*)
  end else begin
    H.add seen e ();
    match e with
    | LL (_, _, b) -> count b;
    | VV (_, _) -> ()
    | AA (_, _, args) -> List.iter count args;
  end;
;;

let rec check e =
  HE.mem tbl e && (
    match e with
    | LL (_, _, b) -> check b
    | VV (_, _) -> true
    | AA (_, _, args) -> List.for_all check args
  )
;;

let gen2 = ref 0;;

let main seed =

  Random.init seed;

  let at_gc () =
    gen2 := !generated;
    generated := 0;
    (*Printf.eprintf "gen2 = %d\n" !gen2; flush stderr;*)
  in
  ignore (Gc.create_alarm at_gc);

  for i = 1 to iter1 do
    for j = 1 to iter2 do
      ignore (rnd_expr ());
    done;
    live := rnd_expr () :: !live;
  done;

  H.clear seen;
  List.iter count !live;
  Array.iter count buffer;
  let max_live = H.length seen + !generated + !gen2 in

  let table_size = HE.count tbl in

  let chk x e = x && check e in
  let l1 = List.fold_left chk true !live in
  let l2 = Array.fold_left chk true buffer in
  let all_alive = l1 && l2 in

  if not all_alive then begin
    Printf.printf "fail: not all alive\n";
    raise Exit;
  end else if table_size > max_live then begin
    Printf.printf "fail: too many alive\n";
    raise Exit;
  end else begin
    Printf.printf "pass\n";
  end;
;;

let seed =
  if Array.length Sys.argv < 2
  then (Random.self_init (); Random.bits ())
  else int_of_string Sys.argv.(1)
;;

try main seed
with e ->
  Printf.printf "TEST FAILED [%d]\n" seed;
  raise e;
;;
