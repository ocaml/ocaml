(* TEST
 modules = "recommended_domain_count_cstubs.c";
*)

external get_max_domains : unit -> int = "caml_get_max_domains"

let _ =
  assert (Domain.recommended_domain_count () > 0);
  assert (Domain.recommended_domain_count () <= (get_max_domains ()));
  (* Domain.max_domains should agree with the C-level value *)
  assert (Domain.max_domains () = get_max_domains ());
  assert (Domain.max_domains () >= 1);
  assert (Domain.recommended_domain_count () <= Domain.max_domains ());
  print_string "passed\n"
