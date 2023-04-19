(* TEST *)

let len = 15000
let rounds = 10

let () =
  let oc = open_out "data.txt" in
  for i = 1 to rounds do
    Printf.fprintf oc "%s\n%!" (String.make len 'x');
  done;
  close_out oc;
  let ic = open_in "data.txt" in
  let l1 = in_channel_length ic in
  for i = 1 to rounds do
    let s = input_line ic in
    assert (String.length s = len);
    let l = in_channel_length ic in
    assert (l = l1)
  done;
  close_in ic
