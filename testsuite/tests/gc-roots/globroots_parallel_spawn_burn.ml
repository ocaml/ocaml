(* TEST
   flags += " -w a "
   modules = "globrootsprim.c globroots.ml"
*)

open Globroots

module TestClassic = Test(Classic)
module TestGenerational = Test(Generational)

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 0

let _ =
  if test_size <= 1 then begin print_string "ok\n"; exit 0 end

let n = 10

let _ =
  for _ = 1 to 20 do
    let burn = fun () ->
      let module TestClassic = Test(Classic) () in
      let module TestGenerational = Test(Generational) () in
      TestClassic.test n;
      TestGenerational.test n in
    let d = Array.init 4 (fun _ -> Domain.spawn burn) in
    Array.iter Domain.join d
  done;
  let n = 128 in

  let arr_classic =
    Array.init n (fun i -> Classic.register (Int.to_string i)) in
  let d_classic_set = Array.init 4 (fun i -> Domain.spawn(fun () ->
    for j = i * (n / 4) to ((i + 1) * (n / 4) - 1) do
      Classic.set arr_classic.(j) (Int.to_string (j * 4))
    done)) in
  Array.iter Domain.join d_classic_set;
  let d_classic_remove = Array.init 4 (fun i -> Domain.spawn(fun () ->
    for j = i * (n / 4) to ((i + 1) * (n / 4) - 1) do
      Classic.remove arr_classic.(j)
    done)) in
  Array.iter Domain.join d_classic_remove;

  let arr_generational =
    Array.init 128 (fun i -> Generational.register (Int.to_string (i+1))) in
  let d_generational_set = Array.init 4 (fun i -> Domain.spawn(fun () ->
    for j = i * (n / 4) to ((i + 1) * (n / 4) - 1) do
      Generational.set arr_generational.(j) (Int.to_string (j * 4))
    done)) in
  Array.iter Domain.join d_generational_set;
  let d_generational_remove = Array.init 4 (fun i -> Domain.spawn(fun () ->
    for j = i * (n / 4) to ((i + 1) * (n / 4) - 1) do
      Generational.remove arr_generational.(j)
    done)) in
  Array.iter Domain.join d_generational_remove;

  print_string "ok\n";
