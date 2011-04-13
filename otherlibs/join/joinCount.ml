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
  type t =
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

module Monitor = struct
  type key = int

  type ('a,'b,'c) t =
      { enter : 'a -> key;
        leave : key * 'b -> unit;
        is_pending : key -> bool;
        get_pendings : unit -> (key * 'a) list;
        wait : unit -> 'c;
        finished : unit Join.chan; }

  let create gather default =
    def state(new_id, pendings, result) & enter(x) =
      state(new_id+1, (new_id,x)::pendings, result) &
      reply new_id to enter

    or state(new_id, pendings, result) & leave(id,v) =
      reply to leave &
      if List.mem_assoc id pendings then
        let result' = gather v result in
        let pendings'= List.remove_assoc id pendings in
        state(new_id, pendings', result')
      else
        state(new_id, pendings, result)

    or state(new_id, pendings, result) & is_pending(id) =
      state(new_id, pendings, result) &
      let b = List.mem_assoc id pendings in
      reply b to is_pending

    or state(new_id, pendings, result) & get_pendings() =
      state(new_id, pendings, result) &
      reply pendings to get_pendings

    or state(new_id, [], result) & wait() & finished() =
      state(new_id, [], result) & reply result to wait

    in spawn state(0, [], default) ;

    {  enter=enter ; leave=leave ;
       is_pending=is_pending ; get_pendings=get_pendings ;
       wait=wait; finished=finished ; }
end
