(* TEST *)

(* A key store during marking on an ephemeron that has already been
   scanned with a dead key must not leave the data unmarked (#15288). *)

module E = Obj.Ephemeron

let go = Atomic.make 0
let ack = Atomic.make 0
let stop = Atomic.make false
let sink = Array.make 4096 [||]
let key = ref None
let live = Bytes.make 64 'l'

(* A second domain that takes a major slice when asked. *)
let other () =
  while not (Atomic.get stop) do
    if Atomic.get go > Atomic.get ack then begin
      ignore (Gc.major_slice 0);
      Atomic.incr ack
    end else Domain.cpu_relax ()
  done

let unset_key e = E.unset_key e 0

let set_key e = E.set_key e 0 (Obj.repr live)

let blit_key e =
  let src = E.create 1 in
  E.set_key src 0 (Obj.repr live);
  E.blit_key src 0 e 0 1

let attempt store =
  let e = E.create 1 in
  key := Some (Bytes.make 64 'k');
  E.set_key e 0 (Obj.repr (Option.get !key));
  E.set_data e (Obj.repr (Array.init 64 (fun i -> ref i)));
  Gc.full_major ();
  key := None;                                  (* the key is now dead *)
  for i = 1 to 100_000 do                       (* get marking started *)
    sink.(i land 4095) <- Array.make 8 0
  done;
  for _ = 1 to 60 do ignore (Gc.major_slice 0) done;
  store e;                                      (* during marking *)
  Atomic.incr go;
  while Atomic.get ack < Atomic.get go do Domain.cpu_relax () done;
  Gc.full_major ();
  ignore (Sys.opaque_identity
            (Array.init 3000 (fun i -> Array.make 64 (ref i))));
  match E.get_data e with
  | None -> ()
  | Some d ->
    let a : int ref array = Obj.obj d in
    assert (Array.length a = 64 && !(a.(5)) = 5)

let () =
  let d = Domain.spawn other in
  List.iter (fun store -> for _ = 1 to 30 do attempt store done)
    [unset_key; set_key; blit_key];
  Atomic.set stop true;
  Domain.join d
