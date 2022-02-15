(** Parse command line options *)
let verbose = ref false
and size = ref 5000
and nruns = ref 20
and ninstances = ref 1

module type Config = sig
  val verbose : bool
  val size : int
  val nruns : int
  val ninstances : int
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
          "-r",Int (fun i -> nruns :=i),doc_def "number of interations" nruns ;
          "-n",Int (fun i -> ninstances :=i),
          doc_def "number of concurrent test instances" ninstances ;
        ]
        (fun _ -> raise (Bad "No argument allowed"))
        (Printf.sprintf "%s: (options)*, where options are:" pgm)

    module Config = struct
      let verbose = !verbose
      let size = !size
      let nruns = !nruns
      let ninstances = !ninstances
    end

  end
