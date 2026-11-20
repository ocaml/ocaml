(* TEST *)

(* Ephemerons with old data and freshly-set young keys, keys kept
   alive: ephemeron marking must preserve the data even when it reads
   a key that is still young (is_unmarked is not meaningful for young
   blocks: in one of three cycles live young blocks read as unmarked).
   Failure modes: debug-runtime assertion in caml_ephe_clean at
   ephemeron sweeping, or corrupted ephemeron data. *)

let n = 200
let ephes = Array.init n (fun _ -> Obj.Ephemeron.create 1)
let stop = Atomic.make false

let setter () =
  Gc.set { (Gc.get ()) with minor_heap_size = 128 * 1024 };
  for i = 0 to n - 1 do
    Obj.Ephemeron.set_data ephes.(i) (Obj.repr (Array.make 10 i))
  done;
  Gc.minor ();  (* promote the data; it ages from here *)
  let prev = ref [] in
  while not (Atomic.get stop) do
    let keep = ref [] in
    for i = 0 to n - 1 do
      let r = ref i in
      Obj.Ephemeron.set_key ephes.(i) 0 (Obj.repr r);
      keep := Obj.repr r :: !keep
    done;
    prev := !keep;
    ignore (Sys.opaque_identity !prev)
  done;
  (* The keys were alive throughout, so the data must be intact.
     (No reads during the run: ephemeron getters darken their result,
     which would mask the bug.) *)
  for i = 0 to n - 1 do
    match Obj.Ephemeron.get_data ephes.(i) with
    | Some d ->
      let a : int array = Obj.obj d in
      if not (Array.length a = 10 && a.(0) = i) then begin
        print_endline "CORRUPT DATA"; exit 42
      end
    | None -> ()
  done

let () =
  let d = Domain.spawn setter in
  let slots = Array.make 64 [||] in
  for i = 1 to 600_000 do
    slots.(i land 63) <- Array.make 100 i;
    if i mod 1_000 = 0 then ignore (Sys.opaque_identity (Gc.major_slice 500))
  done;
  ignore (Sys.opaque_identity slots);
  Atomic.set stop true;
  Domain.join d;
  print_endline "ok"
