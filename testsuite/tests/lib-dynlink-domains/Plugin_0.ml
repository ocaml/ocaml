let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.2e7809321f87cp-1
let () = Store.add "[0]->[]"
let add x = Store.add x
let () = Store.add "[0]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 344." ^ "That's all"
let () = Store.add "[0]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 107." ^ "That's all"
let d0_0 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_0_0.cmo")
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 886." ^ "That's all"
let () = Store.add "[0]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.a9103c0b19caap-1
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.a336dfe6f5e0fp+0
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.509d123be5b22p+0
let () = Domain.join d0_0
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.f63331a1668d8p+1
