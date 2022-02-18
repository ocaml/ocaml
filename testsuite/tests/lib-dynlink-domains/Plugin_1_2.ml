let d0_2_1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_0.cmo")
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 115." ^ Plugin_1_0_0.wordy
let () = Plugin_1_0.add "[2_1]->[0_1]"
let () = Domain.join d0_2_1
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 273." ^ Plugin_1_0_0.wordy
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 935." ^ Plugin_1_0_0.wordy
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 217." ^ Plugin_1_0_0.wordy
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 488." ^ Plugin_1_0_0.wordy
let () = Store.add "[2_1]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_1_0.sqrt2
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 34." ^ Plugin_1_0_0.wordy
let add x = Plugin_1_0.add x
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_1_0.sqrt2
