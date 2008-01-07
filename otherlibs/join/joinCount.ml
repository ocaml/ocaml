module Collector = struct
  type ('a,'b) t =
      { collect : 'a Join.chan ;
      wait : unit -> 'b ; }
	
  let create f y0 n =
    def count(y,n) & collect(x) = count(f x y,n-1)
    or  count(y,0) & wait() = reply y to wait in
    spawn count(y0,n) ;
    { collect=collect ; wait=wait ; }
end

module Down = struct
  type ('a,'b) t =
      { tick : unit Join.chan ;
      wait : unit -> unit ; }

  let create n =
    let col = Collector.create (fun () () -> ()) () n in
    { tick = col.Collector.collect ; wait = col.Collector.wait; }
end


module Dynamic = struct
  type ('a, 'b) t =
      { enter: unit -> unit; leave: 'a Join.chan;
        wait: unit -> 'b; finished: unit Join.chan; }

  let create combine init =
    def state(n,r) & enter() = state(n+1,r) & reply () to enter
    or state(n,r) & leave(v) = state(n-1,combine v r)
    or state(0,r) & wait() & finished() = reply r to wait in
    spawn state(0,init) ;
    { enter=enter ; leave=leave ; wait=wait; finished=finished ; }
end  

