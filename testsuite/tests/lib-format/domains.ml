(* TEST *)

(** Test that domains stdout and stderr are flushed at domain exit *)

type pf = { pf: 'a. ('a, Format.formatter, unit) format -> 'a }

let n_domain = 1 (* to preserve Domain.at_exit ordering *)

let test start {pf} =
  let rec print d () = pf "%d" d in
  let () = pf "@[" in
  let ds =
    List.init n_domain (fun d -> Domain.spawn (print (start+d)))
  in
  List.iter Domain.join ds;
  pf "@]"

let () =
  test 0 {pf=Format.eprintf};
  test n_domain {pf=Format.printf};
  test (2*n_domain) {pf=Format.printf};
  test (3*n_domain) {pf=Format.eprintf};
  Format.printf "\n"
