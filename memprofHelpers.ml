open Memprof
open Printexc

(* Data structures *)

let min_buf_size = 1024
let empty_ephe = Ephemeron.K1.create ()
let samples = ref (Array.make min_buf_size empty_ephe)
let n_samples = ref 0

(* Data structure managment functions. They are not reentrant, so they should
   not be called when the sampling is active. *)

let reset () =
  samples := Array.make min_buf_size empty_ephe;
  n_samples := 0

let clean () =
  let s = !samples and sz = !n_samples in
  let rec aux i j =
    if i >= sz then j
    else if Ephemeron.K1.check_key s.(i) then (s.(j) <- s.(i); aux (i+1) (j+1))
    else aux (i+1) j
  in
  n_samples := aux 0 0;
  Array.fill s !n_samples (sz - !n_samples) empty_ephe;
  if 8 * !n_samples <= Array.length s && Array.length s > min_buf_size then
    samples := Array.sub s 0 (max min_buf_size (2 * !n_samples))
  else if 2 * !n_samples > Array.length s then begin
    let s_new = Array.make (2 * !n_samples) empty_ephe in
    Array.blit !samples 0 s_new 0 !n_samples;
    samples := s_new
  end

let push e =
  if !n_samples = Array.length !samples then clean ();
  !samples.(!n_samples) <- e;
  incr n_samples

(* The callback we use. *)

let callback : sample_info Memprof.callback = fun info ->
  let ephe = Ephemeron.K1.create () in
  Ephemeron.K1.set_data ephe info;
  push ephe;
  Some ephe

(* Reading and printing the set of samples. *)

type sampleTree =
    STC of sample_info list * int * (raw_backtrace_slot, sampleTree) Hashtbl.t

let add_sampleTree (s:sample_info) (t:sampleTree) : sampleTree =
  let rec aux idx (STC (sl, n, sth)) =
    if idx >= Printexc.raw_backtrace_length s.callstack then
      STC(s::sl, n+s.n_samples, sth)
    else
      let li = Printexc.get_raw_backtrace_slot s.callstack idx in
      let child =
        try Hashtbl.find sth li
        with Not_found -> STC ([], 0, Hashtbl.create 3)
      in
      Hashtbl.replace sth li (aux (idx+1) child);
      STC(sl, n+s.n_samples, sth)
  in
  aux 0 t

type sortedSampleTree =
    SSTC of sample_info list * int * (raw_backtrace_slot * sortedSampleTree) list

let rec sort_sampleTree (t:sampleTree) : sortedSampleTree =
  let STC (sl, n, sth) = t in
  SSTC (sl, n,
        List.sort (fun (_, SSTC (_, n1, _)) (_, SSTC (_, n2, _)) -> n2 - n1)
         (Hashtbl.fold (fun li st lst -> (li, sort_sampleTree st)::lst) sth []))

let print chan min_samples (SSTC (_, n, tl)) =
  let rec aux indent =
    List.iter (fun (li, SSTC (sl, n, tl)) ->
      if min_samples <= n then
        begin
          begin match Printexc.Slot.location (convert_raw_backtrace_slot li) with
                | Some { filename; line_number; start_char; end_char } ->
                   Printf.fprintf chan "%7d | %s%s:%d %d-%d" n indent
                                  filename line_number start_char end_char
                | None ->
                   Printf.fprintf chan "%7d | %s?" n indent
          end;
          Printf.fprintf chan "\n";
          aux (indent^"  ") tl
        end)
  in
  Printf.fprintf chan "%7d | Total samples\n" n;
  aux "" tl;
  Printf.fprintf chan "-----------------------------------------------\n"

let dump () =
  let s, sz = !samples, !n_samples in
  let rec aux st i =
    if i >= sz then st
    else match Ephemeron.K1.get_data s.(i) with
         | None -> aux st (i+1)
         | Some s -> aux (add_sampleTree s st) (i+1)
  in
  sort_sampleTree (aux (STC ([], 0, Hashtbl.create 3)) 0)

let start sampling_rate callstack_size min_samples_print =
  Memprof.start { sampling_rate; callstack_size; callback };
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle
    (fun _ ->
     stop ();
     let chan =
       open_out_gen [Open_wronly; Open_creat; Open_text; Open_append]
                    0o666  "memory_profile"
     in
     print chan min_samples_print (dump ());
     close_out chan;
     Memprof.start { sampling_rate; callstack_size; callback }));
  Memprof.start { sampling_rate; callstack_size; callback }
