let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.f57fc99a6fa18p+0
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_0_2_0.sqrt2
let add x = Store.add x
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_0_2_0.sqrt2
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_0_2_0.sqrt2
let () = Plugin_0_2_0_0.add "[3_2_0]->[0_0_2_0]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 599." ^ Plugin_0_2_0.wordy
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_0_2_0.sqrt2
let () = Plugin_0_2_0_0.add "[3_2_0]->[0_0_2_0]"
let d0_3_2_0 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_0_2_3_0.cmo")
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 653." ^ Plugin_0_2_0.wordy
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 895." ^ Plugin_0_2_0.wordy
let () = Domain.join d0_3_2_0
