module type Symantics = sig
  type ('c,'sv,'dv) repr
  type ('c, 'sv, 'dv) pack = {
   _lam : 'sa 'sb 'da 'db . (('c,'sa,'da) repr -> ('c,'sb,'db) repr)
    -> ('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr),'da->'db) repr;
   }
end

(* concrete *)
module R = struct
  type ('c,'sv,'dv) repr = 'dv
  type ('c, 'sv, 'dv) pack = {
   _lam : 'sa 'sb 'da 'db . (('c,'sa,'da) repr -> ('c,'sb,'db) repr)
    -> ('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr),'da->'db) repr;
   }
end;;

module XX(S:Symantics) = struct end;;
module XXR = XX(R);;
