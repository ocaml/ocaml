(** Parse command line options *)

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 0

let (default_avail, default_size, default_nruns) =
  if test_size >= 3 then (8, 5000, 20)
  else if test_size = 2 then (4, 5000, 20)
  else (2, 1000, 10)

let verbose = ref false
and size = ref 5000
and nruns = ref 20
and navail = ref default_avail

module type Config = sig
  val verbose : bool
  val size : int
  val nruns : int
  val navail : int
end

module Make (N : sig val pgm : string end) =
  struct
    let pgm =
      if Array.length Sys.argv > 0 then Sys.argv.(0) else N.pgm

    let () =
      let doc_def msg r = Printf.sprintf "<n> %s, default %d" msg !r in
      let open Arg in
      Arg.parse
        [
          "-v",Set verbose,"be verbose";
          "-q",Clear verbose,"be silent";
          "-s",Int (fun i -> size :=i), doc_def "size of arrays" size ;
          "-r",Int (fun i -> nruns :=i),doc_def "number of iterations" nruns ;
          "-a",Int (fun i -> navail :=i),
          doc_def "number of cores available for tests" navail ;
        ]
        (fun _ -> raise (Bad "No argument allowed"))
        (Printf.sprintf "%s: (options)*, where options are:" pgm)

    module Config = struct
      let verbose = !verbose
      let size = !size
      let nruns = !nruns
      let navail = !navail
    end

  end
