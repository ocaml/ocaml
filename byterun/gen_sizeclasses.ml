let overhead block slot obj = 1. -. float_of_int((block / slot) * obj) /. float_of_int block

let max_overhead = 0.10

let rec blocksizes block slot = function
  | 0 -> []
  | obj ->
    if overhead block slot obj > max_overhead 
    then obj :: blocksizes block obj (obj - 1)
    else blocksizes block slot (obj - 1)

let rec findi_acc i p = function
  | [] -> raise Not_found
  | x :: xs -> if p x then i else findi_acc (i + 1) p xs
let findi = findi_acc 0

let arena = 4096
let max_slot = 128
let sizes = List.rev (blocksizes arena max_int max_slot)

let rec size_slots n =
  if n > max_slot then [] else findi (fun x -> n <= x) sizes :: size_slots (n + 1)

open Format

let rec print_list ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "%d" x
  | x :: xs -> fprintf ppf "%d,@ %a" x print_list xs

let _ =
  printf "#define ARENA_REGION_WSIZE %d\n" arena;
  printf "#define MAX_SLOT_WSIZE %d\n" max_slot;
  printf "#define NUM_SIZECLASSES %d\n" (List.length sizes);
  printf "static const unsigned int slot_sizes[NUM_SIZECLASSES] = @[<2>{ %a };@]\n" print_list sizes;
  printf "static const unsigned char size_slots[MAX_SLOT_WSIZE + 1] = @[<2>{ %a };@]\n" print_list (255 :: size_slots 1);
