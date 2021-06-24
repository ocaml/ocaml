(* TEST *)

let () =
  let fmt_key = Format.synchronized_formatter_of_out_channel stdout in
  let domains = Array.init 7 (fun i ->
    Domain.spawn (fun () ->
          let fmt = Domain.DLS.get fmt_key in
          for j = 1 to 10000000 do () done;
          for j = 1 to 100 do
            Format.fprintf fmt "21. +. 21. is %f@." (21. +. 21.);
          done
      )
    ) in
  Array.iter Domain.join domains
