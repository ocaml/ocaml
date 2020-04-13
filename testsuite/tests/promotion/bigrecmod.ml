(* TEST
 *)

module type Big = sig
  type a

  val f000 : a val f001 : a val f002 : a val f003 : a val f004 : a val f005 : a val f006 : a
  val f007 : a val f008 : a val f009 : a val f010 : a val f011 : a val f012 : a val f013 : a
  val f014 : a val f015 : a val f016 : a val f017 : a val f018 : a val f019 : a val f020 : a
  val f021 : a val f022 : a val f023 : a val f024 : a val f025 : a val f026 : a val f027 : a
  val f028 : a val f029 : a val f030 : a val f031 : a val f032 : a val f033 : a val f034 : a
  val f035 : a val f036 : a val f037 : a val f038 : a val f039 : a val f040 : a val f041 : a
  val f042 : a val f043 : a val f044 : a val f045 : a val f046 : a val f047 : a val f048 : a
  val f049 : a val f050 : a val f051 : a val f052 : a val f053 : a val f054 : a val f055 : a
  val f056 : a val f057 : a val f058 : a val f059 : a val f060 : a val f061 : a val f062 : a
  val f063 : a val f064 : a val f065 : a val f066 : a val f067 : a val f068 : a val f069 : a
  val f070 : a val f071 : a val f072 : a val f073 : a val f074 : a val f075 : a val f076 : a
  val f077 : a val f078 : a val f079 : a val f080 : a val f081 : a val f082 : a val f083 : a
  val f084 : a val f085 : a val f086 : a val f087 : a val f088 : a val f089 : a val f090 : a
  val f091 : a val f092 : a val f093 : a val f094 : a val f095 : a val f096 : a val f097 : a
  val f098 : a val f099 : a val f100 : a val f101 : a val f102 : a val f103 : a val f104 : a
  val f105 : a val f106 : a val f107 : a val f108 : a val f109 : a val f110 : a val f111 : a
  val f112 : a val f113 : a val f114 : a val f115 : a val f116 : a val f117 : a val f118 : a
  val f119 : a val f120 : a val f121 : a val f122 : a val f123 : a val f124 : a val f125 : a
  val f126 : a val f127 : a val f128 : a val f129 : a val f130 : a val f131 : a val f132 : a
  val f133 : a val f134 : a val f135 : a val f136 : a val f137 : a val f138 : a val f139 : a
  val f140 : a val f141 : a val f142 : a val f143 : a val f144 : a val f145 : a val f146 : a
  val f147 : a val f148 : a val f149 : a val f150 : a val f151 : a val f152 : a val f153 : a
  val f154 : a val f155 : a val f156 : a val f157 : a val f158 : a val f159 : a val f160 : a
  val f161 : a val f162 : a val f163 : a val f164 : a val f165 : a val f166 : a val f167 : a
  val f168 : a val f169 : a val f170 : a val f171 : a val f172 : a val f173 : a val f174 : a
  val f175 : a val f176 : a val f177 : a val f178 : a val f179 : a val f180 : a val f181 : a
  val f182 : a val f183 : a val f184 : a val f185 : a val f186 : a val f187 : a val f188 : a
  val f189 : a val f190 : a val f191 : a val f192 : a val f193 : a val f194 : a val f195 : a
  val f196 : a val f197 : a val f198 : a val f199 : a val f200 : a val f201 : a val f202 : a
  val f203 : a val f204 : a val f205 : a val f206 : a val f207 : a val f208 : a val f209 : a
  val f210 : a val f211 : a val f212 : a val f213 : a val f214 : a val f215 : a val f216 : a
  val f217 : a val f218 : a val f219 : a val f220 : a val f221 : a val f222 : a val f223 : a
  val f224 : a val f225 : a val f226 : a val f227 : a val f228 : a val f229 : a val f230 : a
  val f231 : a val f232 : a val f233 : a val f234 : a val f235 : a val f236 : a val f237 : a
  val f238 : a val f239 : a val f240 : a val f241 : a val f242 : a val f243 : a val f244 : a
  val f245 : a val f246 : a val f247 : a val f248 : a val f249 : a val f250 : a val f251 : a
  val f252 : a val f253 : a val f254 : a val f255 : a val f256 : a val f257 : a val f258 : a
  val f259 : a val f260 : a val f261 : a val f262 : a val f263 : a val f264 : a val f265 : a
  val f266 : a val f267 : a val f268 : a val f269 : a val f270 : a val f271 : a val f272 : a
  val f273 : a val f274 : a val f275 : a val f276 : a val f277 : a val f278 : a val f279 : a
  val f280 : a val f281 : a val f282 : a val f283 : a val f284 : a val f285 : a val f286 : a
  val f287 : a val f288 : a val f289 : a val f290 : a val f291 : a val f292 : a val f293 : a
  val f294 : a val f295 : a val f296 : a val f297 : a val f298 : a val f299 : a val f300 : a
end

let p = ref 42
module Big = struct
  type a = [`Foo of int ref]
  let a : a = `Foo p

  let f000 = a let f001 = a let f002 = a let f003 = a let f004 = a let f005 = a let f006 = a
  let f007 = a let f008 = a let f009 = a let f010 = a let f011 = a let f012 = a let f013 = a
  let f014 = a let f015 = a let f016 = a let f017 = a let f018 = a let f019 = a let f020 = a
  let f021 = a let f022 = a let f023 = a let f024 = a let f025 = a let f026 = a let f027 = a
  let f028 = a let f029 = a let f030 = a let f031 = a let f032 = a let f033 = a let f034 = a
  let f035 = a let f036 = a let f037 = a let f038 = a let f039 = a let f040 = a let f041 = a
  let f042 = a let f043 = a let f044 = a let f045 = a let f046 = a let f047 = a let f048 = a
  let f049 = a let f050 = a let f051 = a let f052 = a let f053 = a let f054 = a let f055 = a
  let f056 = a let f057 = a let f058 = a let f059 = a let f060 = a let f061 = a let f062 = a
  let f063 = a let f064 = a let f065 = a let f066 = a let f067 = a let f068 = a let f069 = a
  let f070 = a let f071 = a let f072 = a let f073 = a let f074 = a let f075 = a let f076 = a
  let f077 = a let f078 = a let f079 = a let f080 = a let f081 = a let f082 = a let f083 = a
  let f084 = a let f085 = a let f086 = a let f087 = a let f088 = a let f089 = a let f090 = a
  let f091 = a let f092 = a let f093 = a let f094 = a let f095 = a let f096 = a let f097 = a
  let f098 = a let f099 = a let f100 = a let f101 = a let f102 = a let f103 = a let f104 = a
  let f105 = a let f106 = a let f107 = a let f108 = a let f109 = a let f110 = a let f111 = a
  let f112 = a let f113 = a let f114 = a let f115 = a let f116 = a let f117 = a let f118 = a
  let f119 = a let f120 = a let f121 = a let f122 = a let f123 = a let f124 = a let f125 = a
  let f126 = a let f127 = a let f128 = a let f129 = a let f130 = a let f131 = a let f132 = a
  let f133 = a let f134 = a let f135 = a let f136 = a let f137 = a let f138 = a let f139 = a
  let f140 = a let f141 = a let f142 = a let f143 = a let f144 = a let f145 = a let f146 = a
  let f147 = a let f148 = a let f149 = a let f150 = a let f151 = a let f152 = a let f153 = a
  let f154 = a let f155 = a let f156 = a let f157 = a let f158 = a let f159 = a let f160 = a
  let f161 = a let f162 = a let f163 = a let f164 = a let f165 = a let f166 = a let f167 = a
  let f168 = a let f169 = a let f170 = a let f171 = a let f172 = a let f173 = a let f174 = a
  let f175 = a let f176 = a let f177 = a let f178 = a let f179 = a let f180 = a let f181 = a
  let f182 = a let f183 = a let f184 = a let f185 = a let f186 = a let f187 = a let f188 = a
  let f189 = a let f190 = a let f191 = a let f192 = a let f193 = a let f194 = a let f195 = a
  let f196 = a let f197 = a let f198 = a let f199 = a let f200 = a let f201 = a let f202 = a
  let f203 = a let f204 = a let f205 = a let f206 = a let f207 = a let f208 = a let f209 = a
  let f210 = a let f211 = a let f212 = a let f213 = a let f214 = a let f215 = a let f216 = a
  let f217 = a let f218 = a let f219 = a let f220 = a let f221 = a let f222 = a let f223 = a
  let f224 = a let f225 = a let f226 = a let f227 = a let f228 = a let f229 = a let f230 = a
  let f231 = a let f232 = a let f233 = a let f234 = a let f235 = a let f236 = a let f237 = a
  let f238 = a let f239 = a let f240 = a let f241 = a let f242 = a let f243 = a let f244 = a
  let f245 = a let f246 = a let f247 = a let f248 = a let f249 = a let f250 = a let f251 = a
  let f252 = a let f253 = a let f254 = a let f255 = a let f256 = a let f257 = a let f258 = a
  let f259 = a let f260 = a let f261 = a let f262 = a let f263 = a let f264 = a let f265 = a
  let f266 = a let f267 = a let f268 = a let f269 = a let f270 = a let f271 = a let f272 = a
  let f273 = a let f274 = a let f275 = a let f276 = a let f277 = a let f278 = a let f279 = a
  let f280 = a let f281 = a let f282 = a let f283 = a let f284 = a let f285 = a let f286 = a
  let f287 = a let f288 = a let f289 = a let f290 = a let f291 = a let f292 = a let f293 = a
  let f294 = a let f295 = a let f296 = a let f297 = a let f298 = a let f299 = a let f300 = a

end

let () =
  let p' = match Big.f132 with `Foo p -> p in
  Printf.printf "%b %b %b %b\n%!"
    (Obj.is_shared (Obj.repr (module Big : Big)))
    (Obj.is_shared (Obj.repr Big.f000))
    (Obj.is_shared (Obj.repr p))
    (p == p')


let () = Gc.full_major ()

module type T = sig
  val f : unit -> int ref
  val g : unit -> int ref
end
module type Big' = Big with type a = unit -> int ref

module F (X : sig val p : int ref end) = struct
  module rec M : T = struct
    let f () = Big'.f000 ()
    let g () = N.g ()
  end
  and Big' : Big' = struct
    type a = unit -> int ref
    let a () = N.f ()

    let f000 () = M.g () let f001 () = a () let f002 () = a () let f003 () = a () let f004 () = a () let f005 () = a () let f006 () = a ()
    let f007 () = a () let f008 () = a () let f009 () = a () let f010 () = a () let f011 () = a () let f012 () = a () let f013 () = a ()
    let f014 () = a () let f015 () = a () let f016 () = a () let f017 () = a () let f018 () = a () let f019 () = a () let f020 () = a ()
    let f021 () = a () let f022 () = a () let f023 () = a () let f024 () = a () let f025 () = a () let f026 () = a () let f027 () = a ()
    let f028 () = a () let f029 () = a () let f030 () = a () let f031 () = a () let f032 () = a () let f033 () = a () let f034 () = a ()
    let f035 () = a () let f036 () = a () let f037 () = a () let f038 () = a () let f039 () = a () let f040 () = a () let f041 () = a ()
    let f042 () = a () let f043 () = a () let f044 () = a () let f045 () = a () let f046 () = a () let f047 () = a () let f048 () = a ()
    let f049 () = a () let f050 () = a () let f051 () = a () let f052 () = a () let f053 () = a () let f054 () = a () let f055 () = a ()
    let f056 () = a () let f057 () = a () let f058 () = a () let f059 () = a () let f060 () = a () let f061 () = a () let f062 () = a ()
    let f063 () = a () let f064 () = a () let f065 () = a () let f066 () = a () let f067 () = a () let f068 () = a () let f069 () = a ()
    let f070 () = a () let f071 () = a () let f072 () = a () let f073 () = a () let f074 () = a () let f075 () = a () let f076 () = a ()
    let f077 () = a () let f078 () = a () let f079 () = a () let f080 () = a () let f081 () = a () let f082 () = a () let f083 () = a ()
    let f084 () = a () let f085 () = a () let f086 () = a () let f087 () = a () let f088 () = a () let f089 () = a () let f090 () = a ()
    let f091 () = a () let f092 () = a () let f093 () = a () let f094 () = a () let f095 () = a () let f096 () = a () let f097 () = a ()
    let f098 () = a () let f099 () = a () let f100 () = a () let f101 () = a () let f102 () = a () let f103 () = a () let f104 () = a ()
    let f105 () = a () let f106 () = a () let f107 () = a () let f108 () = a () let f109 () = a () let f110 () = a () let f111 () = a ()
    let f112 () = a () let f113 () = a () let f114 () = a () let f115 () = a () let f116 () = a () let f117 () = a () let f118 () = a ()
    let f119 () = a () let f120 () = a () let f121 () = a () let f122 () = a () let f123 () = a () let f124 () = a () let f125 () = a ()
    let f126 () = a () let f127 () = a () let f128 () = a () let f129 () = a () let f130 () = a () let f131 () = a () let f132 () = a ()
    let f133 () = a () let f134 () = a () let f135 () = a () let f136 () = a () let f137 () = a () let f138 () = a () let f139 () = a ()
    let f140 () = a () let f141 () = a () let f142 () = a () let f143 () = a () let f144 () = a () let f145 () = a () let f146 () = a ()
    let f147 () = a () let f148 () = a () let f149 () = a () let f150 () = a () let f151 () = a () let f152 () = a () let f153 () = a ()
    let f154 () = a () let f155 () = a () let f156 () = a () let f157 () = a () let f158 () = a () let f159 () = a () let f160 () = a ()
    let f161 () = a () let f162 () = a () let f163 () = a () let f164 () = a () let f165 () = a () let f166 () = a () let f167 () = a ()
    let f168 () = a () let f169 () = a () let f170 () = a () let f171 () = a () let f172 () = a () let f173 () = a () let f174 () = a ()
    let f175 () = a () let f176 () = a () let f177 () = a () let f178 () = a () let f179 () = a () let f180 () = a () let f181 () = a ()
    let f182 () = a () let f183 () = a () let f184 () = a () let f185 () = a () let f186 () = a () let f187 () = a () let f188 () = a ()
    let f189 () = a () let f190 () = a () let f191 () = a () let f192 () = a () let f193 () = a () let f194 () = a () let f195 () = a ()
    let f196 () = a () let f197 () = a () let f198 () = a () let f199 () = a () let f200 () = a () let f201 () = a () let f202 () = a ()
    let f203 () = a () let f204 () = a () let f205 () = a () let f206 () = a () let f207 () = a () let f208 () = a () let f209 () = a ()
    let f210 () = a () let f211 () = a () let f212 () = a () let f213 () = a () let f214 () = a () let f215 () = a () let f216 () = a ()
    let f217 () = a () let f218 () = a () let f219 () = a () let f220 () = a () let f221 () = a () let f222 () = a () let f223 () = a ()
    let f224 () = a () let f225 () = a () let f226 () = a () let f227 () = a () let f228 () = a () let f229 () = a () let f230 () = a ()
    let f231 () = a () let f232 () = a () let f233 () = a () let f234 () = a () let f235 () = a () let f236 () = a () let f237 () = a ()
    let f238 () = a () let f239 () = a () let f240 () = a () let f241 () = a () let f242 () = a () let f243 () = a () let f244 () = a ()
    let f245 () = a () let f246 () = a () let f247 () = a () let f248 () = a () let f249 () = a () let f250 () = a () let f251 () = a ()
    let f252 () = a () let f253 () = a () let f254 () = a () let f255 () = a () let f256 () = a () let f257 () = a () let f258 () = a ()
    let f259 () = a () let f260 () = a () let f261 () = a () let f262 () = a () let f263 () = a () let f264 () = a () let f265 () = a ()
    let f266 () = a () let f267 () = a () let f268 () = a () let f269 () = a () let f270 () = a () let f271 () = a () let f272 () = a ()
    let f273 () = a () let f274 () = a () let f275 () = a () let f276 () = a () let f277 () = a () let f278 () = a () let f279 () = a ()
    let f280 () = a () let f281 () = a () let f282 () = a () let f283 () = a () let f284 () = a () let f285 () = a () let f286 () = a ()
    let f287 () = a () let f288 () = a () let f289 () = a () let f290 () = a () let f291 () = a () let f292 () = a () let f293 () = a ()
    let f294 () = a () let f295 () = a () let f296 () = a () let f297 () = a () let f298 () = a () let f299 () = a () let f300 () = a ()

  end
  and N : T = struct
    let f () = Big'.f000 ()
    let g () = X.p
  end
end

let () =
  let module FX = F (struct let p = p end) in
  let open FX in
  let p' = M.f () in
  Printf.printf "%b %b %b %b %b %b %b %b %b\n%!"
    (Obj.is_shared (Obj.repr (module Big' : Big')))
    (Obj.is_shared (Obj.repr Big.f123))
    (Obj.is_shared (Obj.repr (module M : T)))
    (Obj.is_shared (Obj.repr M.f))
    (Obj.is_shared (Obj.repr M.g))
    (Obj.is_shared (Obj.repr (module N : T)))
    (Obj.is_shared (Obj.repr N.f))
    (Obj.is_shared (Obj.repr N.g))
    (p == p')
