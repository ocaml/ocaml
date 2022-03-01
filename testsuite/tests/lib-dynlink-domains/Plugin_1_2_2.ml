let add x = Plugin_1_2_0.add x
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_2_0.cmo"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 324." ^ Plugin_1_2_0.wordy
let () = Store.add "[2_2_1]->[]"
let () = Store.add "[2_2_1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 883." ^ Plugin_1_2_0.wordy
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 779." ^ Plugin_1_2_0.wordy
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_1_2_0.sqrt2
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 380." ^ Plugin_1_2_0.wordy
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.5f0333a0a2732p+1
let () = Store.add "[2_2_1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 46." ^ Plugin_1_2_0.wordy
let () = Plugin_1_2_0_0.add "[2_2_1]->[0_0_2_1]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_1_2_0.sqrt2
