(* TEST *)

let () =
  let o1 = object val x = 123 method get () = x end in
  let o2 = object method get () = 456 end in
  let r = ref o1 in
  let n = (!r)#get (r := o2) in
  (* Order of evaluation here is not really specified or consistent,
     but we want to get either 123 or 456 *)
  assert (n = 123 || n = 456)
