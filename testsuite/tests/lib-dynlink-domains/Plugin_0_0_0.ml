let () = Store.add "[0_0_0]->[]"
let () = Store.add "[0_0_0]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 938." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.e9cbba20257cfp+0
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 856." ^ "That's all"
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_0_0_0_0.cmo"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.d982d97cf43d2p+1
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.0bc6f17c375f9p+1
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 146." ^ "That's all"
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_0_0_0_1.cmo"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 289." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.19976d38c42ddp-3
let d2_0_0_0 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_0_0_0_2.cmo")
let add x = Store.add x
let () = Store.add "[0_0_0]->[]"
let () = Domain.join d2_0_0_0
let () = Store.add "[0_0_0]->[]"
