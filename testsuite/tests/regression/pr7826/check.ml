(* Used by nested.ml: reads the profile that [ocamlopt -dprofile] wrote, and
   fails if the generate phase -- the one whose allocation is dominated by
   closure conversion -- exceeded the threshold below.

   Allocation is deterministic for a given compiler and input, so unlike a
   wall-clock bound this does not depend on how fast the machine is.  The chain
   nested.ml compiles allocates about 1 GB here; before the closure pass
   memoised the free variables of each function group that phase allocated about
   200 GB (see #7826), so a 10 GB threshold separates the two by a wide margin. *)

let threshold_mb = 10_000.

let to_mb field =
  let n = String.length field in
  if n < 2 then failwith ("unexpected allocation field: " ^ field);
  let value = float_of_string (String.sub field 0 (n - 2)) in
  match String.sub field (n - 2) 2 with
  | "GB" -> value *. 1024.
  | "MB" -> value
  | "kB" -> value /. 1024.
  | "B" -> value /. (1024. *. 1024.)
  | _ -> failwith ("unexpected allocation unit: " ^ field)

(* The phase table is indented, and `generate` is the name of exactly one line
   in it -- its children are named after their own phases. *)
let closure_allocation path =
  let ic = open_in path in
  let rec scan () =
    match input_line ic with
    | line ->
        let fields =
          String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
        if fields <> [] && List.nth fields (List.length fields - 1) = "generate"
        then List.nth fields 1
        else scan ()
    | exception End_of_file -> failwith ("no closure phase in " ^ path)
  in
  let field = scan () in
  close_in ic;
  field

let () =
  let path = Sys.argv.(1) in
  let allocated = to_mb (closure_allocation path) in
  Printf.printf "generate phase allocated %.0f MB (limit %.0f MB)\n"
    allocated threshold_mb;
  if allocated > threshold_mb then begin
    Printf.eprintf
      "the generate phase allocated %.0f MB, over the %.0f MB limit: closure \
       conversion is recomputing the free variables of function groups per \
       nesting level again\n"
      allocated threshold_mb;
    exit 1
  end
