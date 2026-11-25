(* TEST
 include unix;
 hasunix;
 { bytecode; }{ native; }
*)

(* #15335: on Windows, SO_RCVTIMEO and SO_SNDTIMEO did not round-trip *)

let () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  List.iter (fun (name, opt) ->
      List.iter (fun t ->
          Unix.setsockopt_float s opt t;
          Printf.printf "%s %g -> %g\n" name t (Unix.getsockopt_float s opt))
        [2.5; 0.25; 0.0])
    [ ("SO_RCVTIMEO", Unix.SO_RCVTIMEO); ("SO_SNDTIMEO", Unix.SO_SNDTIMEO) ];
  Unix.close s
