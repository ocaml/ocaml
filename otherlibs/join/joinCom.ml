
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


  let read_line chan =
    try
      let x = Pervasives.input_line chan in
      Some x
    with End_of_file -> None

  let safe_close_in chan =
    try close_in chan
    with Sys_error msg ->
      Join.debug "PROD" "Sys_error in close_in: \"%s\"" msg ;
      ()

  let of_text chan =
    def get(k) & alive() =
      alive() &
      let x =
        try  read_line chan
        with Sys_error msg ->
          Join.debug "PROD" "Sys_error in read_line: \"%s\"" msg ;
          None in
      k(x)
    or get(k) & dead() = k(None) & dead()
    or kill() & alive() = safe_close_in chan ; dead()
    or kill() & dead() = dead() in
    spawn alive() ;
    { get=get ; kill=kill ; }

  let output_line chan line =
    output_string chan line ;
    output_char chan '\n' ;
    flush chan ;
    ()

  def to_text(prod,chan,k) =
    def writer(line) = match line with
    | Some line ->
        let ok =
          try output_line chan line ;
            true
          with Sys_error _msg -> false in
        if ok then prod.get(writer)
        else prod.kill()
  | None -> k() in
  prod.get(writer)

end


module C = struct
  type 'a t = {
      put : ('a * bool Join.chan) Join.chan;
      close : unit -> unit;
    }
end


def connect(prod,cons,k) =
  def reader(line) =  match line with
  | Some line -> cons.C.put(line,pk)
  | None -> cons.C.close() ; k()
  and pk(b) =
    if b then prod.P.get(reader)
    else prod.P.kill() in
  prod.P.get(reader)
 
