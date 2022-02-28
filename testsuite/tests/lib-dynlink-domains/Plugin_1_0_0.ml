let () = Store.add "[0_0_1]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.dd626133b25p+1
let add x = Store.add x
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.bf37a3f737fb2p+1
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 927." ^ "That's all"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 779." ^ "That's all"
let () = Store.add "[0_0_1]->[]"
let () = Store.add "[0_0_1]->[]"
let d0_0_0_1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_0_0_0.cmo")
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 627." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.9ea414f4c6b77p+1
let () = Store.add "[0_0_1]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.1d54fc1e95cfcp+2
let () = Domain.join d0_0_0_1
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 355." ^ "That's all"
