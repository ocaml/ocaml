(* TEST
* hasunix
include unix
** bytecode
** native
*)

let try_ext cmd =
  let ic, oc, iec = Unix.open_process_full cmd (Unix.environment ()) in
  let n = try Some (input_line ic |> int_of_string) with _ -> None in
  match Unix.close_process_full (ic, oc, iec) with
  | Unix.WEXITED 0 -> n
  | _ -> None

(* Try to guess the number of cpus via an external method. If we
   can't, assume it passed since we have nothing to compare against *)
let _ =
  List.iter (fun cmd ->
      match try_ext cmd with
      | None -> ()
      | Some n ->
          let recommended_domain_count =
            Domain.recommended_domain_count () in
          if recommended_domain_count > n then
            failwith
              (Printf.sprintf
                 "external tool %s says n=%d, recommended domain says=%d\n"
                 cmd n recommended_domain_count))
    ["nproc"; "sysctl -n hw.ncpu"];
  print_string "passed\n"
