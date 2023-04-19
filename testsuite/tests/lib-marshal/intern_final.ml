(* TEST *)

let t : int array = Array.make 200 42
let c = open_out_bin "data42"
let () = Marshal.to_channel c t []
let () = close_out c

let t : int array = Array.make 200 0
let c = open_out_bin "data0"
let () = Marshal.to_channel c t []
let () = close_out c

let rec fill_minor accu = function
  | 0 -> accu
  | n -> fill_minor (n::accu) (n-1)

let rec callback c0 () =
  seek_in c0 0;
  let a = Marshal.from_channel c0 in
  Array.iter (fun v -> assert (v = 0)) a

let () =
  let c0 = open_in_bin "data0" in
  let c42 = open_in_bin "data42" in

  ignore (Gc.create_alarm (callback c0));

  for i = 0 to 100000 do
    seek_in c42 0;
    let res : int array = Marshal.from_channel c42 in
    Array.iter (fun n -> assert (n = 42)) res
  done;
  Printf.printf "OK!\n"
