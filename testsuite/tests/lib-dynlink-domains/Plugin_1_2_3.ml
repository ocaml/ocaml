let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 249." ^ Plugin_1_2_0.wordy
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 757." ^ Plugin_1_2_0.wordy
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_1_2_0.sqrt2
let add x = Plugin_1_2_0.add x
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 882." ^ Plugin_1_2_0.wordy
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 236." ^ "That's all"
let () = Plugin_1_2_0.add "[3_2_1]->[0_2_1]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 179." ^ Plugin_1_2_0.wordy
let d0_3_2_1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_3_0.cmo")
let () = Domain.join d0_3_2_1
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_1_2_0.sqrt2
let () = Plugin_1_2_0.add "[3_2_1]->[0_2_1]"
let () = Store.add "[3_2_1]->[]"
let () = Plugin_1_2_0.add "[3_2_1]->[0_2_1]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 438." ^ Plugin_1_2_0.wordy
