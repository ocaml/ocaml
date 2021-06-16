(* TEST *)

let () =
  let domains = Array.init 7 (fun i ->
    Domain.spawn (fun () ->
          for j = 1 to 10000000 do () done;
          for j = 1 to 100 do
            Format.printf "The quick brown fox jumped over the lazy dog\n";
            flush stdout
          done
      )
    ) in
  Array.iter Domain.join domains
