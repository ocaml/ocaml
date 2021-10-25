(* TEST
   modules = "stubs.c"
*)

external init_skiplist : unit -> unit = "init_skiplist"
external hammer_skiplist : int -> unit = "hammer_skiplist"

let () =
   init_skiplist ();
   let domains_list = List.init 4 (fun i -> Domain.spawn (fun () -> hammer_skiplist i)) in
      ignore(List.iter Domain.join domains_list)

(* Concurrent versions of the memory test in tests.ml, see there first *)
external insert_skiplist : int -> int -> int -> unit = "insert_skiplist"
external find_skiplist : int -> int -> int -> bool = "find_skiplist"
external clean_skiplist : int -> unit = "clean_skiplist"
external cardinal_skiplist : unit -> int = "cardinal_skiplist"

let () =
  (* Clean garbage list *)
  clean_skiplist (-1);
  (* Check cleaning *)
  clean_skiplist 0;
  init_skiplist ();
  assert (cardinal_skiplist() = 0) ;
  let nturns = 128 and npar = 4 and nseq = 4 in
  assert (nturns < 1024); (* See calc_key in stubs.c *)
  (* Fill skip list and then empty it *)
  for k = 1 to nseq do
    let d_list =
      List.init npar
        (fun i ->
          Domain.spawn
            (fun () ->
              for k = 1 to nturns do insert_skiplist k npar i done)) in
    ignore (List.iter Domain.join d_list) ;
    assert (cardinal_skiplist() = npar*nturns) ;
    let d_list =
      List.init npar
        (fun i ->
          Domain.spawn
            (fun () ->
              for k = 1 to nturns do assert(find_skiplist k npar i) done)) in
    ignore (List.iter Domain.join d_list) ;
    assert (cardinal_skiplist() = 0) ;
    clean_skiplist (npar*nturns) ;
  done ;
  (* Fill and empty skiplist concurrently *)
  for k = 1 to nseq do
    let d_list =
      List.init (npar*2)
        (fun i ->
          Domain.spawn
            (fun () ->
              let j = i/2 in
              if i mod 2 = 0 then
                for k = 1 to nturns do insert_skiplist k npar j done
              else
                for k = 1 to nturns do
                  while not (find_skiplist k npar j) do
                    Domain.cpu_relax ()
                  done
                done)) in
    ignore (List.iter Domain.join d_list) ;
    assert (cardinal_skiplist() = 0) ;
    clean_skiplist (npar*nturns) ;
  done ;
  (* Fill and empty skiplist concurrently, checking list consistency *)
  for k = 1 to nseq do
    let d_list =
      List.init (npar*2)
        (fun i ->
          Domain.spawn
            (fun () ->
              let j = i/2 in
              if i mod 2 = 0 then
                for k = 1 to nturns do insert_skiplist k npar j done
              else if j mod 2 = 0 then
                for k = 1 to nturns do
                  while not (find_skiplist k npar j) do
                    Domain.cpu_relax ()
                  done
                done)) in
    ignore (List.iter Domain.join d_list) ;
    assert (cardinal_skiplist() = nturns*(npar-(npar+1)/2)) ;
    clean_skiplist (nturns*((npar+1)/2)) ;
    let d_list =
      List.init npar
        (fun i ->
          Domain.spawn
            (fun () ->
              if i mod 2 = 1 then
                for k = 1 to nturns do
                  while not (find_skiplist k npar i) do
                    Domain.cpu_relax ()
                  done
                done)) in
    ignore (List.iter Domain.join d_list) ;
    assert (cardinal_skiplist() = 0);
    clean_skiplist (nturns*(npar/2)) ;
  done ;
  ()
