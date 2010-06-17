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


  let create cmd argv =
    def fork(input) =
      let pid,(in_chan, out_chan,err_chan) = JoinProc.open_full cmd argv in
      debug "TEXT" "ZYVA@%i" pid ;
      def waitpid(k) & result(st) = k(st) in
      let out = JoinCom.P.of_text in_chan
      and err = JoinCom.P.of_text err_chan in
      def k() = close_out out_chan ; 0 in        
      JoinCom.P.to_text (input,out_chan,k) &
      begin
        let r = safe_wait pid in
        result(r)
      end &
      def kill(sid) = safe_kill pid sid ; reply to kill in
      reply
        { out = out;
          err = err;
          waitpid=waitpid;
          kill=kill; pid;}
       to fork in
    fork
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
    let f = Async.create cmd argv in
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
