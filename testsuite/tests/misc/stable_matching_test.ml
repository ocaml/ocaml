(* TEST
include ocamlcommon;
 native; *)


let default_size = (10, 11)

let trace ~on =
  if on then Format.printf else Format.ifprintf Format.std_formatter

(** Implementation of Gale-Shapley algorithm to use as a reference implementation *)

module Gale_Shapley = struct
  open Stable_matching
  type right = {
    index:right_index;
    candidate: int;
    preferences:(left_index * rank) array;
  }

  let next_candidate r = { r with candidate = r.candidate + 1 }

  type matched_left = { matched_to:right; rank:rank }

  type state = {
    active: right list;
    inactive: right_index list;
    left: matched_left option array;
  }

  let try_match ~compatible state (r:right) =
    if r.candidate >= Array.length r.preferences then
      { state with inactive = r.index :: state.inactive }
    else
      let candidate, rank = r.preferences.(r.candidate) in
      if not (compatible candidate r.index) then
        { state with active = next_candidate r :: state.active }
      else match state.left.(candidate) with
      | None ->
          state.left.(candidate) <- Some { matched_to=r; rank };
          state
      | Some r' ->
          if r'.rank > rank then
            let () = state.left.(candidate) <- Some { matched_to=r; rank } in
            { state with active = next_candidate r'.matched_to :: state.active }
          else
            { state with active = next_candidate r :: state.active }

  let rec fix ~compatible state =
    let state =
      List.fold_left (try_match ~compatible) { state with active = [] }
        state.active
    in
    match state.active with
    | [] -> state
    | _ ->  fix ~compatible state

  let matches ~compatible ~preferences ~size:(n_left, n_right) =
    let init i = { index=i; candidate = 0; preferences = preferences i } in
    let active = Array.to_list @@ Array.init n_right init in
    let left = Array.make n_left None in
    let state = fix ~compatible { active; inactive = []; left } in
    let left_and_pairs = state.left |> Array.to_seqi |>
      Seq.partition_map (function
          | i,None -> Left i
          | i, Some m -> Right (i, m.matched_to.index)
        )
    in
    let left, pairs = Pair.map List.of_seq List.of_seq left_and_pairs in
    { left; pairs; right = state.inactive }

end


module Full = struct
  let random ~size:(left_size, right_size) ~debug hdist r =
    let trace x = trace ~on:debug x in
    let a = Array.init left_size Fun.id in
    trace "%2d|" r;
    let () = Array.shuffle ~rand:Random.int a in
    let a = Array.mapi (fun d l ->
        (* avoid ties on the left side *)
        let rank = d * right_size + r in
        trace " %d" l;
        Hashtbl.replace hdist (l,r) rank;
        (l,rank))
        a
    in
    trace "@,";
    a
end

module Partial = struct
  let random ~size:(left_size, right_size) ~debug hdist r =
    let trace x = trace ~on:debug x in
    let len = Random.int left_size in
    let a = Array.init len Fun.id in
    trace "%2d|" r;
    let () = Array.shuffle ~rand:Random.int a in
    let a = Array.mapi (fun d l ->
        trace " %d" l;
        let rank = d * right_size + r in
        Hashtbl.replace hdist (l,r) rank;
        (l,rank))
        a
    in
    trace "@,";
    a
end

module Partial_and_ties = struct
  let random ~size:(left_size, right_size) ~debug hdist r =
    let trace x = trace ~on:debug x in
    let len = Random.int left_size in
    let a = Array.init len Fun.id in
    trace "%2d|" r;
    let () = Array.shuffle ~rand:Random.int a in
    let new_tie_size () = Random.int 2 in
    let tie_counter = ref (new_tie_size (), 0) in
    let a = Array.mapi (fun i l ->
        let tie_size, rank = !tie_counter in
        let rank =
          if tie_size < 0 then begin
            tie_counter := new_tie_size (), rank + 1;
            trace " | %d" l;
            rank + 1
          end
          else begin
            if i = 0 then trace " %d" l else trace " ┆ %d" l;
            tie_counter := tie_size - 1 , rank;
            rank
          end
        in
        Hashtbl.replace hdist (l,r) rank;
        (l,rank))
        a
    in
    trace "@,";
    a
end

let random_matrice ~debug ~size:(left_size, right_size as size) line =
  let h = Hashtbl.create (left_size * right_size) in
  Array.init right_size (fun r -> line ~size ~debug h r),
  h

let pp_pair ppf (x,y) = Format.fprintf ppf "%2d ⇐ %2d" x y

let pp_pair_opt ppf = function
  | None -> Format.fprintf ppf "       "
  | Some p -> pp_pair ppf p

let compare_pair (x,y) (x',y') =
    let c = compare y y' in
    if c = 0 then compare x x' else c

let pp_matching ppf x =
  let x = List.sort compare_pair x.Stable_matching.pairs in
  Format.fprintf ppf "@,@[<v>";
  List.iter (Format.fprintf ppf "%a@," pp_pair) x;
  Format.fprintf ppf "@]"

let compare_matching ppf (x,y) =
  let rec equalize x y = match x, y with
    | [], [] -> []
    | a :: x, b :: y -> (Some a, Some b) :: equalize x y
    | a :: x, [] -> (Some a, None) :: equalize x []
    | [], b :: y -> (None, Some b) :: equalize [] y
  in
  let x = List.sort compare_pair x.Stable_matching.pairs in
  let y = List.sort compare_pair y.Stable_matching.pairs in
  let l = equalize x y in
  Format.fprintf ppf "@,@[<v>";
  List.iter (fun (l,r) ->
      Format.fprintf ppf "%a | %a@," pp_pair_opt l pp_pair_opt r
      ) l;
  Format.fprintf ppf "@]"

let test ~debug ~size line =
  let trace x = trace ~on:debug x in
  let m, h = random_matrice ~size ~debug line in
  let compatible _ _ = true in
  let preferences r = m.(r) in
  let distance i j = match Hashtbl.find h (i,j) with
    | exception Not_found -> Int.max_int
    | x -> x
  in
  let gs_matches = Gale_Shapley.matches ~compatible ~preferences ~size in
  let opt_matches = Stable_matching.matches ~compatible ~preferences ~size in
  let eq x y =
    let sort x =  List.sort Stdlib.compare x in
    let open Stable_matching in
    let r = sort x.pairs = sort y.pairs in
    if r then trace "%a@," pp_matching x
     else trace "%a@," compare_matching (x,y);
    r
  in
  let result matches =
    List.length matches.Stable_matching.pairs,
    Stable_matching.stable_matches ~distance matches = Ok (),
    Stable_matching.strong_stable_matches ~distance matches
  in
  result gs_matches, result opt_matches, eq gs_matches opt_matches

let pp_single ppf (size,stable,strongly_stable) =
  let pp_pair ppf ((l,r), (l',r')) = Format.fprintf ppf "(%d⇐%d, %d⇐%d)" l r l' r' in
  let other ((l,r), (l',r')) = ((l', r), (l,r')) in
  let pp_rank ppf (l,r) = Format.fprintf ppf "(%d,%d)" l r in
  if not stable then Format.fprintf ppf "FAIL" else
  match strongly_stable with
  | Ok () -> Format.fprintf ppf "%d matched" size
  | Error (e:Stable_matching.unstable_matching) ->
      let pair = e.first, e.second in
      Format.fprintf ppf "UNSTABLE %a %a vs %a %a"
        pp_pair pair pp_rank e.current_rank
        pp_pair (other pair) pp_rank e.optimal

let pp_test ppf (gs,opt,same) =
  if same then
    Format.fprintf ppf "SAME %a" pp_single gs
  else
    Format.fprintf ppf "@[%a,@ %a@]"
      pp_single gs
      pp_single opt

let printed_test r line =
  Format.printf "@[<v>@,----------------@,test %d:@," r;
  let r = test ~debug:true ~size:default_size line in
  Format.printf "%a@]@." pp_test r

let same_statistic ~size n =
  let same = ref 0 in
  for _ = 1 to n do
      let _, _, similar = test ~size ~debug:false Full.random in
      if similar then incr same
    done;
  if !same = n then
    Format.printf "OK: same matching as Gale-Shapley [%d tests]@." n
  else
    Format.printf "FAIL:%d/%d different matching@." !same n


let partial_statistic ~size n =
  let len (s,_,_) = s in
  let aggregate (size,stable,strong) (size',stable',strong') =
    let to_int b = if b then 1 else 0 in
    (size + size', stable + to_int stable', strong + to_int (strong'=Ok()))
  in
  let aggregated_gs = ref (0,0,0) in
  let aggregated_opt = ref (0,0,0) in
  let balance = ref (0,0,0) in
  for _ = 1 to n do
      let gs, opt, _  = test ~size ~debug:false Partial_and_ties.random in
      aggregated_gs := aggregate !aggregated_gs gs;
      aggregated_opt := aggregate !aggregated_opt opt;
      let cmp = compare (len gs) (len opt) in
      let gs_better, eq, opt_better = !balance in
      if cmp = 0 then
        balance := gs_better, eq + 1, opt_better
      else if cmp > 0 then
        balance := gs_better + 1, eq, opt_better
      else
        balance := gs_better, eq, opt_better + 1
    done;
  let pp_aggregate ppf (size,stable,strong) =
    let ratio x = float_of_int x /. float_of_int n in
    let percent x = ratio (100 * x) in
    Format.fprintf ppf "average size=%g, weakly stable=%g%%, strongly stable=%g%%"
      (ratio size) (percent stable) (percent strong)
  in
  let pp_balance ppf (gs_better,eq,opt_better) =
    Format.fprintf ppf "Advantages: Gale-Shapley=%d, equality=%d, 3/2=%d"
      gs_better eq opt_better
  in
  Format.printf "@[<v>Gale-Shapley:%a@,3/2 optimal:%a@,%a@]@."
     pp_aggregate !aggregated_gs
      pp_aggregate !aggregated_opt
      pp_balance !balance

let () = Random.init 123
let n_examples = 3
let n_tests = 5
let () =
  for n = 1 to n_examples do printed_test n Full.random done;
  for n = 1 to n_examples do printed_test n Partial.random done
let () =
  for n = 1 to n_examples do printed_test n Partial_and_ties.random done
let () = same_statistic ~size:(30,31) n_tests
let () = partial_statistic ~size:(29,30) n_tests
