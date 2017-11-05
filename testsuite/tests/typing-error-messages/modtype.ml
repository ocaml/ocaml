module type s = sig
        type arma
        type virumque
        type cano
        type troiae
        type qui
        type primus
        type ab
        type oris
        type italiam
        type fato
        type profugus
        type laviniaque
        type venit
        type litora
end;;

module F(X: s) = struct end;;

module M=F(struct
        type multum
        type ille
        type et
        type terris
        type jactatus
        let et=()
        type alto
        type ui
        type superum
        type saevae
        type memorem
        type junonis
        type ob
        type iram 
end);;
[%%expect {|
module type s =
  sig
    type arma
    type virumque
    type cano
    type troiae
    type qui
    type primus
    type ab
    type oris
    type italiam
    type fato
    type profugus
    type laviniaque
    type venit
    type litora
  end
module F : functor (X : s) -> sig  end
Line _, characters 11-287:
Error: Signature mismatch:
       Modules do not match:
         !(sig ... * 2 type et ... * 3 type alto ... * 7 end)
       is not included in
         !(s)
       The type `litora' is required but not provided
       The type `venit' is required but not provided
       The type `laviniaque' is required but not provided
       The type `profugus' is required but not provided
       The type `fato' is required but not provided
       The type `italiam' is required but not provided
       The type `oris' is required but not provided
       The type `ab' is required but not provided
       The type `primus' is required but not provided
       The type `qui' is required but not provided
       The type `troiae' is required but not provided
       The type `cano' is required but not provided
       The type `virumque' is required but not provided
       The type `arma' is required but not provided
|}]
