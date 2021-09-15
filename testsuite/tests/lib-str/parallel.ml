(* TEST
* hasstr
include str
** bytecode
** native
*)

let total = Atomic.make 0

let run str () =
  let re = Str.regexp str in
  let input = "The quick brown fox jumped over the lazy_t" in
  match Str.search_forward re input 0 with
  | exception Not_found -> Atomic.decr total
  | _ ->
      let s = Str.matched_group 0 input in
      if not (String.equal s str) then
        Atomic.decr total
      else
        Atomic.incr total

let _ =
  (* generate a set of cases matching the reference input or not. *)
  let domain_params = List.init 7
    (fun i -> if i mod 2 == 0 then "the lazy_t" else "the lazy dog")
  in
  for i = 0 to 3 do
    let domains =
      List.map (fun param -> Domain.spawn (run param)) domain_params
    in
    (* domain 0 is an "odd" case, required to achieve a neutral total by the end *)
    run "the lazy dog" ();
    List.iter Domain.join domains
  done;
  let total' = Atomic.get total in
  if total' != 0 then
    Printf.eprintf "NOK: total is not 0: %d\n" total'
  else
    print_endline "OK"
