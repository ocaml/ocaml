let () = Store.add "[0_1]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.8fa5b0cc7a28dp-3
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 849." ^ "That's all"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 174." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.5e64c52caa4cp+0
let () = Store.add "[0_1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 582." ^ "That's all"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 158." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.2a31c66bbd6fep-2
let () = Store.add "[0_1]->[]"
let add x = Store.add x
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_0_0.cmo"
let () = Store.add "[0_1]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.21e3742bdd57ep+2
