 let verbose =
  try
    int_of_string (Sys.getenv "COMVERBOSE")
  with _ -> 0


open Printf

let debug tag =
  if verbose > 0 then Join.debug tag
  else  ksprintf (fun _ -> ())

let safe_close_in chan =
  debug "COM" "close_in" ;
  try close_in chan
  with Sys_error msg ->
   debug "COM" "Sys_error in close_in: \"%s\"" msg ;
    ()

  let safe_close_out chan =
  debug "COM" "close_out" ;
    try close_out chan
    with Sys_error msg ->
      debug "COM" "Sys_error in close_out: \"%s\"" msg ;
      ()

  let read_line chan =
    try
      let x = Pervasives.input_line chan in
      debug "COM" "read_line: '%s'" x ;
      Some x
    with End_of_file -> None

  let output_line chan line =
    try
      debug "COM" "output_line: '%s'" line ;
      output_string chan line ;
      output_char chan '\n' ;
      flush chan ;
      true
    with Sys_error _msg -> false


(** Producers *)
module P = struct
  type 'a t = {
      get : 'a option Join.chan Join.chan;
      kill : unit Join.chan;
    }

  let empty () =
    def get(k) = k(None)
    and kill() = 0 in
    {get; kill;}

  let map f prod =
    def get(k) = kont(k) & prod.get(reader)
    and kont(k) & reader(x) =
      let y = match x with
      | Some x -> Some (f x)
      | None -> None in
      k(y) in
    {get=get;kill=prod.kill}
                

         
  let when_none k_none prod =
    def get(k) = kont(k) & prod.get(reader)
    and kont(k) & reader(x) =
      begin match x with
      | Some _ -> 0
      | None -> k_none()
      end &        k(x) in
    {get=get;kill=prod.kill}


        
  let of_list xs =
    def st([]) & get(k) = k(None) & st([])
    or  st(x::xs) & get(k) = k(Some x) & st(xs)
    or  st(_) & kill() = st([]) in
    spawn st(xs) ;
    {get; kill;}

  def to_list(prod,k) =
    def st(xs) & read(x) = match x with
    | Some x -> st(x::xs) & prod.get(read)
    | None -> k(List.rev xs) in
    st([]) & prod.get(read)


  let of_text chan =
    def get(k) & alive() =
      alive() &
      let x =
        try  read_line chan
        with Sys_error msg ->
          debug "PROD" "Sys_error in read_line: \"%s\"" msg ;
          None in
      begin match x with None -> safe_close_in chan | Some _ -> () end ;
      k(x)
    or get(k) & dead() = k(None) & dead()
    or kill() & alive() = safe_close_in chan ; dead()
    or kill() & dead() = dead() in
    spawn alive() ;
    { get=get ; kill=kill ; }

  def to_text(prod,chan,k) =
    def writer(line) & lock() = match line with
    | Some line ->
        let ok = output_line chan line in
        lock() & 
        if ok then prod.get(writer)
        else begin safe_close_out chan ; prod.kill() end
  | None -> lock() & k() in
  prod.get(writer) & lock()

  def to_text_close(prod,chan) =
    def k() = safe_close_out chan ; 0 in
    to_text(prod,chan,k)
end


module C = struct
  type 'a t = {
      put : ('a * bool Join.chan) Join.chan;
      close : unit -> unit;
    }

(*
  let of_text (chan) =
    def  put(line,k) & alive() =
      let ok = output_line chan line in
      k(ok) &
      if ok then alive() else dead()
    or put(_,k) & dead() = k(false)

    or close() & alive() = safe_close_out chan ; (dead() & reply to close)
    or close() & dead() = dead() & reply to close in

    spawn alive() ;
    {put;close;}
*)
end


def connect(prod,cons,k) =
  def reader(line) =  match line with
  | Some line -> cons.C.put(line,pk)
  | None -> cons.C.close() ; k()
  and pk(b) =
    begin if not b then prod.P.kill() end &
    prod.P.get(reader) in
  prod.P.get(reader)
 
