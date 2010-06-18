let verbose =
  try
    int_of_string (Sys.getenv "TEXTVERBOSE")
  with _ -> 0


open Printf

let debug tag =
  if verbose > 0 then Join.debug tag
  else  ksprintf (fun _ -> ())


let pp_status = function
  | Unix.WEXITED i -> sprintf "EXIT %i" i
  | Unix.WSIGNALED i -> sprintf "SIGNAL %i" i
  | _ -> assert false

let safe_kill pid s =
  debug "TEXT" "KILL@%i" pid ;
  try Unix.kill pid s
  with Unix.Unix_error _ -> ()

let rec safe_wait pid =
  try
    let _,st = Unix.waitpid [] pid in
    debug "TEXT" "WAIT@%i: %s" pid (pp_status st);
    st
  with
  | Unix.Unix_error _ -> safe_wait pid
  | _ -> assert false


module Async = struct

  type producer = string JoinCom.P.t

  type t =
      { out : producer ;
        err : producer ;
        waitpid : Unix.process_status Join.chan Join.chan ;
        kill : int -> unit;
        pid : int ;
      }


  let et =
    let ep = JoinCom.P.empty() in
    { out=ep; err=ep;
      waitpid=(def k(_) = 0 in k);
      kill=(fun _ -> ());
      pid=(-1)}

      
  def producer_to_chan (prod,chan) =
    def k() = close_out chan ; 0 in        
    JoinCom.P.to_text (prod,chan,k)

   let async_waitpid pid =
    def waitpid(k) & result(st) = k(st) in
    spawn result(safe_wait pid) ;
    waitpid

  let async_kill pid =
    def kill(sid) = safe_kill pid sid ; reply to kill in
    kill

  let add_kill_wait pid r =
    { r with
      pid=pid; kill=async_kill pid;
      waitpid=async_waitpid pid; }      

  let command cmd argv =
    let pid = JoinProc.command cmd argv in
    add_kill_wait pid et

  let open_in cmd argv =
    let pid,in_chan = JoinProc.open_in cmd argv in
    let out = JoinCom.P.of_text in_chan in
    add_kill_wait pid { et with out; }

  let open_out cmd argv input =
    let pid,out_chan = JoinProc.open_out cmd argv in
    debug "TEXT" "open_out pid=%i" pid ;
    spawn producer_to_chan (input,out_chan) ;
    add_kill_wait pid et

  let open_in_out cmd argv input =
    let pid,(in_chan,out_chan) = JoinProc.open_in_out cmd argv in
    let out = JoinCom.P.of_text in_chan in
    spawn producer_to_chan (input,out_chan) ;
    add_kill_wait pid { et with out; }

  let open_full cmd argv input =
    let pid,(in_chan, out_chan,err_chan) = JoinProc.open_full cmd argv in
    let out = JoinCom.P.of_text in_chan
    and err = JoinCom.P.of_text err_chan in
    spawn producer_to_chan (input,out_chan) ;
    add_kill_wait pid { et with out; err;}

end

module Sync = struct

  type text = string list

  type result =
      { st : Unix.process_status ;
        out : text ;
        err : text ; }

  type t =
      { wait : unit  -> result;
        kill : int -> unit; }


  module P = JoinCom.P

  let list_to_producer = P.of_list

  def consume (prod,k) =
    P.to_list(prod,def kk(xs) = prod.P.kill() & k(xs) in kk) 

  let fst xs = match xs with
  | x::_ -> x
  | [] -> ""

  let create cmd argv =
    let f = Async.open_full cmd argv in
    def fork(input) =
      let ext = f(list_to_producer(input)) in
      let tagpid = sprintf "SYNC@%i" ext.Async.pid in
      def wait_ter() & waitpid(st) & out(os) & err(es) =
        debug tagpid "%s" (fst os) ;
        reply { st=st; out=os; err=es } to wait_ter in
      let verb tag k =
        def v(r) =
          debug tagpid "%s: %s" tag (fst r) ; k(r) in v in
      consume(ext.Async.out,verb "OUT" out) &
      consume(ext.Async.err,verb "ERR" err) &
      ext.Async.waitpid(waitpid) &
      reply { wait=wait_ter; kill=ext.Async.kill; } to fork in
    fork
end
