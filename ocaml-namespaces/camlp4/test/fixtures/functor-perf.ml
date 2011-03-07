module type S = sig
(* <%- for i in 0 .. 1000 do -%>
  val f<%= i %> : int -> int -> int
<%- end -%>                            *)
  val f0 : int -> int -> int
  val f1 : int -> int -> int
  val f2 : int -> int -> int
  val f3 : int -> int -> int
  val f4 : int -> int -> int
  val f5 : int -> int -> int
  val f6 : int -> int -> int
  val f7 : int -> int -> int
  val f8 : int -> int -> int
  val f9 : int -> int -> int
  val f10 : int -> int -> int
  val f11 : int -> int -> int
  val f12 : int -> int -> int
  val f13 : int -> int -> int
  val f14 : int -> int -> int
  val f15 : int -> int -> int
  val f16 : int -> int -> int
  val f17 : int -> int -> int
  val f18 : int -> int -> int
  val f19 : int -> int -> int
  val f20 : int -> int -> int
  val f21 : int -> int -> int
  val f22 : int -> int -> int
  val f23 : int -> int -> int
  val f24 : int -> int -> int
  val f25 : int -> int -> int
  val f26 : int -> int -> int
  val f27 : int -> int -> int
  val f28 : int -> int -> int
  val f29 : int -> int -> int
  val f30 : int -> int -> int
  val f31 : int -> int -> int
  val f32 : int -> int -> int
  val f33 : int -> int -> int
  val f34 : int -> int -> int
  val f35 : int -> int -> int
  val f36 : int -> int -> int
  val f37 : int -> int -> int
  val f38 : int -> int -> int
  val f39 : int -> int -> int
  val f40 : int -> int -> int
  val f41 : int -> int -> int
  val f42 : int -> int -> int
  val f43 : int -> int -> int
  val f44 : int -> int -> int
  val f45 : int -> int -> int
  val f46 : int -> int -> int
  val f47 : int -> int -> int
  val f48 : int -> int -> int
  val f49 : int -> int -> int
  val f50 : int -> int -> int
  val f51 : int -> int -> int
  val f52 : int -> int -> int
  val f53 : int -> int -> int
  val f54 : int -> int -> int
  val f55 : int -> int -> int
  val f56 : int -> int -> int
  val f57 : int -> int -> int
  val f58 : int -> int -> int
  val f59 : int -> int -> int
  val f60 : int -> int -> int
  val f61 : int -> int -> int
  val f62 : int -> int -> int
  val f63 : int -> int -> int
  val f64 : int -> int -> int
  val f65 : int -> int -> int
  val f66 : int -> int -> int
  val f67 : int -> int -> int
  val f68 : int -> int -> int
  val f69 : int -> int -> int
  val f70 : int -> int -> int
  val f71 : int -> int -> int
  val f72 : int -> int -> int
  val f73 : int -> int -> int
  val f74 : int -> int -> int
  val f75 : int -> int -> int
  val f76 : int -> int -> int
  val f77 : int -> int -> int
  val f78 : int -> int -> int
  val f79 : int -> int -> int
  val f80 : int -> int -> int
  val f81 : int -> int -> int
  val f82 : int -> int -> int
  val f83 : int -> int -> int
  val f84 : int -> int -> int
  val f85 : int -> int -> int
  val f86 : int -> int -> int
  val f87 : int -> int -> int
  val f88 : int -> int -> int
  val f89 : int -> int -> int
  val f90 : int -> int -> int
  val f91 : int -> int -> int
  val f92 : int -> int -> int
  val f93 : int -> int -> int
  val f94 : int -> int -> int
  val f95 : int -> int -> int
  val f96 : int -> int -> int
  val f97 : int -> int -> int
  val f98 : int -> int -> int
  val f99 : int -> int -> int
  val f100 : int -> int -> int
  val f101 : int -> int -> int
  val f102 : int -> int -> int
  val f103 : int -> int -> int
  val f104 : int -> int -> int
  val f105 : int -> int -> int
  val f106 : int -> int -> int
  val f107 : int -> int -> int
  val f108 : int -> int -> int
  val f109 : int -> int -> int
  val f110 : int -> int -> int
  val f111 : int -> int -> int
  val f112 : int -> int -> int
  val f113 : int -> int -> int
  val f114 : int -> int -> int
  val f115 : int -> int -> int
  val f116 : int -> int -> int
  val f117 : int -> int -> int
  val f118 : int -> int -> int
  val f119 : int -> int -> int
  val f120 : int -> int -> int
  val f121 : int -> int -> int
  val f122 : int -> int -> int
  val f123 : int -> int -> int
  val f124 : int -> int -> int
  val f125 : int -> int -> int
  val f126 : int -> int -> int
  val f127 : int -> int -> int
  val f128 : int -> int -> int
  val f129 : int -> int -> int
  val f130 : int -> int -> int
  val f131 : int -> int -> int
  val f132 : int -> int -> int
  val f133 : int -> int -> int
  val f134 : int -> int -> int
  val f135 : int -> int -> int
  val f136 : int -> int -> int
  val f137 : int -> int -> int
  val f138 : int -> int -> int
  val f139 : int -> int -> int
  val f140 : int -> int -> int
  val f141 : int -> int -> int
  val f142 : int -> int -> int
  val f143 : int -> int -> int
  val f144 : int -> int -> int
  val f145 : int -> int -> int
  val f146 : int -> int -> int
  val f147 : int -> int -> int
  val f148 : int -> int -> int
  val f149 : int -> int -> int
  val f150 : int -> int -> int
  val f151 : int -> int -> int
  val f152 : int -> int -> int
  val f153 : int -> int -> int
  val f154 : int -> int -> int
  val f155 : int -> int -> int
  val f156 : int -> int -> int
  val f157 : int -> int -> int
  val f158 : int -> int -> int
  val f159 : int -> int -> int
  val f160 : int -> int -> int
  val f161 : int -> int -> int
  val f162 : int -> int -> int
  val f163 : int -> int -> int
  val f164 : int -> int -> int
  val f165 : int -> int -> int
  val f166 : int -> int -> int
  val f167 : int -> int -> int
  val f168 : int -> int -> int
  val f169 : int -> int -> int
  val f170 : int -> int -> int
  val f171 : int -> int -> int
  val f172 : int -> int -> int
  val f173 : int -> int -> int
  val f174 : int -> int -> int
  val f175 : int -> int -> int
  val f176 : int -> int -> int
  val f177 : int -> int -> int
  val f178 : int -> int -> int
  val f179 : int -> int -> int
  val f180 : int -> int -> int
  val f181 : int -> int -> int
  val f182 : int -> int -> int
  val f183 : int -> int -> int
  val f184 : int -> int -> int
  val f185 : int -> int -> int
  val f186 : int -> int -> int
  val f187 : int -> int -> int
  val f188 : int -> int -> int
  val f189 : int -> int -> int
  val f190 : int -> int -> int
  val f191 : int -> int -> int
  val f192 : int -> int -> int
  val f193 : int -> int -> int
  val f194 : int -> int -> int
  val f195 : int -> int -> int
  val f196 : int -> int -> int
  val f197 : int -> int -> int
  val f198 : int -> int -> int
  val f199 : int -> int -> int
  val f200 : int -> int -> int
  val f201 : int -> int -> int
  val f202 : int -> int -> int
  val f203 : int -> int -> int
  val f204 : int -> int -> int
  val f205 : int -> int -> int
  val f206 : int -> int -> int
  val f207 : int -> int -> int
  val f208 : int -> int -> int
  val f209 : int -> int -> int
  val f210 : int -> int -> int
  val f211 : int -> int -> int
  val f212 : int -> int -> int
  val f213 : int -> int -> int
  val f214 : int -> int -> int
  val f215 : int -> int -> int
  val f216 : int -> int -> int
  val f217 : int -> int -> int
  val f218 : int -> int -> int
  val f219 : int -> int -> int
  val f220 : int -> int -> int
  val f221 : int -> int -> int
  val f222 : int -> int -> int
  val f223 : int -> int -> int
  val f224 : int -> int -> int
  val f225 : int -> int -> int
  val f226 : int -> int -> int
  val f227 : int -> int -> int
  val f228 : int -> int -> int
  val f229 : int -> int -> int
  val f230 : int -> int -> int
  val f231 : int -> int -> int
  val f232 : int -> int -> int
  val f233 : int -> int -> int
  val f234 : int -> int -> int
  val f235 : int -> int -> int
  val f236 : int -> int -> int
  val f237 : int -> int -> int
  val f238 : int -> int -> int
  val f239 : int -> int -> int
  val f240 : int -> int -> int
  val f241 : int -> int -> int
  val f242 : int -> int -> int
  val f243 : int -> int -> int
  val f244 : int -> int -> int
  val f245 : int -> int -> int
  val f246 : int -> int -> int
  val f247 : int -> int -> int
  val f248 : int -> int -> int
  val f249 : int -> int -> int
  val f250 : int -> int -> int
  val f251 : int -> int -> int
  val f252 : int -> int -> int
  val f253 : int -> int -> int
  val f254 : int -> int -> int
  val f255 : int -> int -> int
  val f256 : int -> int -> int
  val f257 : int -> int -> int
  val f258 : int -> int -> int
  val f259 : int -> int -> int
  val f260 : int -> int -> int
  val f261 : int -> int -> int
  val f262 : int -> int -> int
  val f263 : int -> int -> int
  val f264 : int -> int -> int
  val f265 : int -> int -> int
  val f266 : int -> int -> int
  val f267 : int -> int -> int
  val f268 : int -> int -> int
  val f269 : int -> int -> int
  val f270 : int -> int -> int
  val f271 : int -> int -> int
  val f272 : int -> int -> int
  val f273 : int -> int -> int
  val f274 : int -> int -> int
  val f275 : int -> int -> int
  val f276 : int -> int -> int
  val f277 : int -> int -> int
  val f278 : int -> int -> int
  val f279 : int -> int -> int
  val f280 : int -> int -> int
  val f281 : int -> int -> int
  val f282 : int -> int -> int
  val f283 : int -> int -> int
  val f284 : int -> int -> int
  val f285 : int -> int -> int
  val f286 : int -> int -> int
  val f287 : int -> int -> int
  val f288 : int -> int -> int
  val f289 : int -> int -> int
  val f290 : int -> int -> int
  val f291 : int -> int -> int
  val f292 : int -> int -> int
  val f293 : int -> int -> int
  val f294 : int -> int -> int
  val f295 : int -> int -> int
  val f296 : int -> int -> int
  val f297 : int -> int -> int
  val f298 : int -> int -> int
  val f299 : int -> int -> int
  val f300 : int -> int -> int
  val f301 : int -> int -> int
  val f302 : int -> int -> int
  val f303 : int -> int -> int
  val f304 : int -> int -> int
  val f305 : int -> int -> int
  val f306 : int -> int -> int
  val f307 : int -> int -> int
  val f308 : int -> int -> int
  val f309 : int -> int -> int
  val f310 : int -> int -> int
  val f311 : int -> int -> int
  val f312 : int -> int -> int
  val f313 : int -> int -> int
  val f314 : int -> int -> int
  val f315 : int -> int -> int
  val f316 : int -> int -> int
  val f317 : int -> int -> int
  val f318 : int -> int -> int
  val f319 : int -> int -> int
  val f320 : int -> int -> int
  val f321 : int -> int -> int
  val f322 : int -> int -> int
  val f323 : int -> int -> int
  val f324 : int -> int -> int
  val f325 : int -> int -> int
  val f326 : int -> int -> int
  val f327 : int -> int -> int
  val f328 : int -> int -> int
  val f329 : int -> int -> int
  val f330 : int -> int -> int
  val f331 : int -> int -> int
  val f332 : int -> int -> int
  val f333 : int -> int -> int
  val f334 : int -> int -> int
  val f335 : int -> int -> int
  val f336 : int -> int -> int
  val f337 : int -> int -> int
  val f338 : int -> int -> int
  val f339 : int -> int -> int
  val f340 : int -> int -> int
  val f341 : int -> int -> int
  val f342 : int -> int -> int
  val f343 : int -> int -> int
  val f344 : int -> int -> int
  val f345 : int -> int -> int
  val f346 : int -> int -> int
  val f347 : int -> int -> int
  val f348 : int -> int -> int
  val f349 : int -> int -> int
  val f350 : int -> int -> int
  val f351 : int -> int -> int
  val f352 : int -> int -> int
  val f353 : int -> int -> int
  val f354 : int -> int -> int
  val f355 : int -> int -> int
  val f356 : int -> int -> int
  val f357 : int -> int -> int
  val f358 : int -> int -> int
  val f359 : int -> int -> int
  val f360 : int -> int -> int
  val f361 : int -> int -> int
  val f362 : int -> int -> int
  val f363 : int -> int -> int
  val f364 : int -> int -> int
  val f365 : int -> int -> int
  val f366 : int -> int -> int
  val f367 : int -> int -> int
  val f368 : int -> int -> int
  val f369 : int -> int -> int
  val f370 : int -> int -> int
  val f371 : int -> int -> int
  val f372 : int -> int -> int
  val f373 : int -> int -> int
  val f374 : int -> int -> int
  val f375 : int -> int -> int
  val f376 : int -> int -> int
  val f377 : int -> int -> int
  val f378 : int -> int -> int
  val f379 : int -> int -> int
  val f380 : int -> int -> int
  val f381 : int -> int -> int
  val f382 : int -> int -> int
  val f383 : int -> int -> int
  val f384 : int -> int -> int
  val f385 : int -> int -> int
  val f386 : int -> int -> int
  val f387 : int -> int -> int
  val f388 : int -> int -> int
  val f389 : int -> int -> int
  val f390 : int -> int -> int
  val f391 : int -> int -> int
  val f392 : int -> int -> int
  val f393 : int -> int -> int
  val f394 : int -> int -> int
  val f395 : int -> int -> int
  val f396 : int -> int -> int
  val f397 : int -> int -> int
  val f398 : int -> int -> int
  val f399 : int -> int -> int
  val f400 : int -> int -> int
  val f401 : int -> int -> int
  val f402 : int -> int -> int
  val f403 : int -> int -> int
  val f404 : int -> int -> int
  val f405 : int -> int -> int
  val f406 : int -> int -> int
  val f407 : int -> int -> int
  val f408 : int -> int -> int
  val f409 : int -> int -> int
  val f410 : int -> int -> int
  val f411 : int -> int -> int
  val f412 : int -> int -> int
  val f413 : int -> int -> int
  val f414 : int -> int -> int
  val f415 : int -> int -> int
  val f416 : int -> int -> int
  val f417 : int -> int -> int
  val f418 : int -> int -> int
  val f419 : int -> int -> int
  val f420 : int -> int -> int
  val f421 : int -> int -> int
  val f422 : int -> int -> int
  val f423 : int -> int -> int
  val f424 : int -> int -> int
  val f425 : int -> int -> int
  val f426 : int -> int -> int
  val f427 : int -> int -> int
  val f428 : int -> int -> int
  val f429 : int -> int -> int
  val f430 : int -> int -> int
  val f431 : int -> int -> int
  val f432 : int -> int -> int
  val f433 : int -> int -> int
  val f434 : int -> int -> int
  val f435 : int -> int -> int
  val f436 : int -> int -> int
  val f437 : int -> int -> int
  val f438 : int -> int -> int
  val f439 : int -> int -> int
  val f440 : int -> int -> int
  val f441 : int -> int -> int
  val f442 : int -> int -> int
  val f443 : int -> int -> int
  val f444 : int -> int -> int
  val f445 : int -> int -> int
  val f446 : int -> int -> int
  val f447 : int -> int -> int
  val f448 : int -> int -> int
  val f449 : int -> int -> int
  val f450 : int -> int -> int
  val f451 : int -> int -> int
  val f452 : int -> int -> int
  val f453 : int -> int -> int
  val f454 : int -> int -> int
  val f455 : int -> int -> int
  val f456 : int -> int -> int
  val f457 : int -> int -> int
  val f458 : int -> int -> int
  val f459 : int -> int -> int
  val f460 : int -> int -> int
  val f461 : int -> int -> int
  val f462 : int -> int -> int
  val f463 : int -> int -> int
  val f464 : int -> int -> int
  val f465 : int -> int -> int
  val f466 : int -> int -> int
  val f467 : int -> int -> int
  val f468 : int -> int -> int
  val f469 : int -> int -> int
  val f470 : int -> int -> int
  val f471 : int -> int -> int
  val f472 : int -> int -> int
  val f473 : int -> int -> int
  val f474 : int -> int -> int
  val f475 : int -> int -> int
  val f476 : int -> int -> int
  val f477 : int -> int -> int
  val f478 : int -> int -> int
  val f479 : int -> int -> int
  val f480 : int -> int -> int
  val f481 : int -> int -> int
  val f482 : int -> int -> int
  val f483 : int -> int -> int
  val f484 : int -> int -> int
  val f485 : int -> int -> int
  val f486 : int -> int -> int
  val f487 : int -> int -> int
  val f488 : int -> int -> int
  val f489 : int -> int -> int
  val f490 : int -> int -> int
  val f491 : int -> int -> int
  val f492 : int -> int -> int
  val f493 : int -> int -> int
  val f494 : int -> int -> int
  val f495 : int -> int -> int
  val f496 : int -> int -> int
  val f497 : int -> int -> int
  val f498 : int -> int -> int
  val f499 : int -> int -> int
  val f500 : int -> int -> int
  val f501 : int -> int -> int
  val f502 : int -> int -> int
  val f503 : int -> int -> int
  val f504 : int -> int -> int
  val f505 : int -> int -> int
  val f506 : int -> int -> int
  val f507 : int -> int -> int
  val f508 : int -> int -> int
  val f509 : int -> int -> int
  val f510 : int -> int -> int
  val f511 : int -> int -> int
  val f512 : int -> int -> int
  val f513 : int -> int -> int
  val f514 : int -> int -> int
  val f515 : int -> int -> int
  val f516 : int -> int -> int
  val f517 : int -> int -> int
  val f518 : int -> int -> int
  val f519 : int -> int -> int
  val f520 : int -> int -> int
  val f521 : int -> int -> int
  val f522 : int -> int -> int
  val f523 : int -> int -> int
  val f524 : int -> int -> int
  val f525 : int -> int -> int
  val f526 : int -> int -> int
  val f527 : int -> int -> int
  val f528 : int -> int -> int
  val f529 : int -> int -> int
  val f530 : int -> int -> int
  val f531 : int -> int -> int
  val f532 : int -> int -> int
  val f533 : int -> int -> int
  val f534 : int -> int -> int
  val f535 : int -> int -> int
  val f536 : int -> int -> int
  val f537 : int -> int -> int
  val f538 : int -> int -> int
  val f539 : int -> int -> int
  val f540 : int -> int -> int
  val f541 : int -> int -> int
  val f542 : int -> int -> int
  val f543 : int -> int -> int
  val f544 : int -> int -> int
  val f545 : int -> int -> int
  val f546 : int -> int -> int
  val f547 : int -> int -> int
  val f548 : int -> int -> int
  val f549 : int -> int -> int
  val f550 : int -> int -> int
  val f551 : int -> int -> int
  val f552 : int -> int -> int
  val f553 : int -> int -> int
  val f554 : int -> int -> int
  val f555 : int -> int -> int
  val f556 : int -> int -> int
  val f557 : int -> int -> int
  val f558 : int -> int -> int
  val f559 : int -> int -> int
  val f560 : int -> int -> int
  val f561 : int -> int -> int
  val f562 : int -> int -> int
  val f563 : int -> int -> int
  val f564 : int -> int -> int
  val f565 : int -> int -> int
  val f566 : int -> int -> int
  val f567 : int -> int -> int
  val f568 : int -> int -> int
  val f569 : int -> int -> int
  val f570 : int -> int -> int
  val f571 : int -> int -> int
  val f572 : int -> int -> int
  val f573 : int -> int -> int
  val f574 : int -> int -> int
  val f575 : int -> int -> int
  val f576 : int -> int -> int
  val f577 : int -> int -> int
  val f578 : int -> int -> int
  val f579 : int -> int -> int
  val f580 : int -> int -> int
  val f581 : int -> int -> int
  val f582 : int -> int -> int
  val f583 : int -> int -> int
  val f584 : int -> int -> int
  val f585 : int -> int -> int
  val f586 : int -> int -> int
  val f587 : int -> int -> int
  val f588 : int -> int -> int
  val f589 : int -> int -> int
  val f590 : int -> int -> int
  val f591 : int -> int -> int
  val f592 : int -> int -> int
  val f593 : int -> int -> int
  val f594 : int -> int -> int
  val f595 : int -> int -> int
  val f596 : int -> int -> int
  val f597 : int -> int -> int
  val f598 : int -> int -> int
  val f599 : int -> int -> int
  val f600 : int -> int -> int
  val f601 : int -> int -> int
  val f602 : int -> int -> int
  val f603 : int -> int -> int
  val f604 : int -> int -> int
  val f605 : int -> int -> int
  val f606 : int -> int -> int
  val f607 : int -> int -> int
  val f608 : int -> int -> int
  val f609 : int -> int -> int
  val f610 : int -> int -> int
  val f611 : int -> int -> int
  val f612 : int -> int -> int
  val f613 : int -> int -> int
  val f614 : int -> int -> int
  val f615 : int -> int -> int
  val f616 : int -> int -> int
  val f617 : int -> int -> int
  val f618 : int -> int -> int
  val f619 : int -> int -> int
  val f620 : int -> int -> int
  val f621 : int -> int -> int
  val f622 : int -> int -> int
  val f623 : int -> int -> int
  val f624 : int -> int -> int
  val f625 : int -> int -> int
  val f626 : int -> int -> int
  val f627 : int -> int -> int
  val f628 : int -> int -> int
  val f629 : int -> int -> int
  val f630 : int -> int -> int
  val f631 : int -> int -> int
  val f632 : int -> int -> int
  val f633 : int -> int -> int
  val f634 : int -> int -> int
  val f635 : int -> int -> int
  val f636 : int -> int -> int
  val f637 : int -> int -> int
  val f638 : int -> int -> int
  val f639 : int -> int -> int
  val f640 : int -> int -> int
  val f641 : int -> int -> int
  val f642 : int -> int -> int
  val f643 : int -> int -> int
  val f644 : int -> int -> int
  val f645 : int -> int -> int
  val f646 : int -> int -> int
  val f647 : int -> int -> int
  val f648 : int -> int -> int
  val f649 : int -> int -> int
  val f650 : int -> int -> int
  val f651 : int -> int -> int
  val f652 : int -> int -> int
  val f653 : int -> int -> int
  val f654 : int -> int -> int
  val f655 : int -> int -> int
  val f656 : int -> int -> int
  val f657 : int -> int -> int
  val f658 : int -> int -> int
  val f659 : int -> int -> int
  val f660 : int -> int -> int
  val f661 : int -> int -> int
  val f662 : int -> int -> int
  val f663 : int -> int -> int
  val f664 : int -> int -> int
  val f665 : int -> int -> int
  val f666 : int -> int -> int
  val f667 : int -> int -> int
  val f668 : int -> int -> int
  val f669 : int -> int -> int
  val f670 : int -> int -> int
  val f671 : int -> int -> int
  val f672 : int -> int -> int
  val f673 : int -> int -> int
  val f674 : int -> int -> int
  val f675 : int -> int -> int
  val f676 : int -> int -> int
  val f677 : int -> int -> int
  val f678 : int -> int -> int
  val f679 : int -> int -> int
  val f680 : int -> int -> int
  val f681 : int -> int -> int
  val f682 : int -> int -> int
  val f683 : int -> int -> int
  val f684 : int -> int -> int
  val f685 : int -> int -> int
  val f686 : int -> int -> int
  val f687 : int -> int -> int
  val f688 : int -> int -> int
  val f689 : int -> int -> int
  val f690 : int -> int -> int
  val f691 : int -> int -> int
  val f692 : int -> int -> int
  val f693 : int -> int -> int
  val f694 : int -> int -> int
  val f695 : int -> int -> int
  val f696 : int -> int -> int
  val f697 : int -> int -> int
  val f698 : int -> int -> int
  val f699 : int -> int -> int
  val f700 : int -> int -> int
  val f701 : int -> int -> int
  val f702 : int -> int -> int
  val f703 : int -> int -> int
  val f704 : int -> int -> int
  val f705 : int -> int -> int
  val f706 : int -> int -> int
  val f707 : int -> int -> int
  val f708 : int -> int -> int
  val f709 : int -> int -> int
  val f710 : int -> int -> int
  val f711 : int -> int -> int
  val f712 : int -> int -> int
  val f713 : int -> int -> int
  val f714 : int -> int -> int
  val f715 : int -> int -> int
  val f716 : int -> int -> int
  val f717 : int -> int -> int
  val f718 : int -> int -> int
  val f719 : int -> int -> int
  val f720 : int -> int -> int
  val f721 : int -> int -> int
  val f722 : int -> int -> int
  val f723 : int -> int -> int
  val f724 : int -> int -> int
  val f725 : int -> int -> int
  val f726 : int -> int -> int
  val f727 : int -> int -> int
  val f728 : int -> int -> int
  val f729 : int -> int -> int
  val f730 : int -> int -> int
  val f731 : int -> int -> int
  val f732 : int -> int -> int
  val f733 : int -> int -> int
  val f734 : int -> int -> int
  val f735 : int -> int -> int
  val f736 : int -> int -> int
  val f737 : int -> int -> int
  val f738 : int -> int -> int
  val f739 : int -> int -> int
  val f740 : int -> int -> int
  val f741 : int -> int -> int
  val f742 : int -> int -> int
  val f743 : int -> int -> int
  val f744 : int -> int -> int
  val f745 : int -> int -> int
  val f746 : int -> int -> int
  val f747 : int -> int -> int
  val f748 : int -> int -> int
  val f749 : int -> int -> int
  val f750 : int -> int -> int
  val f751 : int -> int -> int
  val f752 : int -> int -> int
  val f753 : int -> int -> int
  val f754 : int -> int -> int
  val f755 : int -> int -> int
  val f756 : int -> int -> int
  val f757 : int -> int -> int
  val f758 : int -> int -> int
  val f759 : int -> int -> int
  val f760 : int -> int -> int
  val f761 : int -> int -> int
  val f762 : int -> int -> int
  val f763 : int -> int -> int
  val f764 : int -> int -> int
  val f765 : int -> int -> int
  val f766 : int -> int -> int
  val f767 : int -> int -> int
  val f768 : int -> int -> int
  val f769 : int -> int -> int
  val f770 : int -> int -> int
  val f771 : int -> int -> int
  val f772 : int -> int -> int
  val f773 : int -> int -> int
  val f774 : int -> int -> int
  val f775 : int -> int -> int
  val f776 : int -> int -> int
  val f777 : int -> int -> int
  val f778 : int -> int -> int
  val f779 : int -> int -> int
  val f780 : int -> int -> int
  val f781 : int -> int -> int
  val f782 : int -> int -> int
  val f783 : int -> int -> int
  val f784 : int -> int -> int
  val f785 : int -> int -> int
  val f786 : int -> int -> int
  val f787 : int -> int -> int
  val f788 : int -> int -> int
  val f789 : int -> int -> int
  val f790 : int -> int -> int
  val f791 : int -> int -> int
  val f792 : int -> int -> int
  val f793 : int -> int -> int
  val f794 : int -> int -> int
  val f795 : int -> int -> int
  val f796 : int -> int -> int
  val f797 : int -> int -> int
  val f798 : int -> int -> int
  val f799 : int -> int -> int
  val f800 : int -> int -> int
  val f801 : int -> int -> int
  val f802 : int -> int -> int
  val f803 : int -> int -> int
  val f804 : int -> int -> int
  val f805 : int -> int -> int
  val f806 : int -> int -> int
  val f807 : int -> int -> int
  val f808 : int -> int -> int
  val f809 : int -> int -> int
  val f810 : int -> int -> int
  val f811 : int -> int -> int
  val f812 : int -> int -> int
  val f813 : int -> int -> int
  val f814 : int -> int -> int
  val f815 : int -> int -> int
  val f816 : int -> int -> int
  val f817 : int -> int -> int
  val f818 : int -> int -> int
  val f819 : int -> int -> int
  val f820 : int -> int -> int
  val f821 : int -> int -> int
  val f822 : int -> int -> int
  val f823 : int -> int -> int
  val f824 : int -> int -> int
  val f825 : int -> int -> int
  val f826 : int -> int -> int
  val f827 : int -> int -> int
  val f828 : int -> int -> int
  val f829 : int -> int -> int
  val f830 : int -> int -> int
  val f831 : int -> int -> int
  val f832 : int -> int -> int
  val f833 : int -> int -> int
  val f834 : int -> int -> int
  val f835 : int -> int -> int
  val f836 : int -> int -> int
  val f837 : int -> int -> int
  val f838 : int -> int -> int
  val f839 : int -> int -> int
  val f840 : int -> int -> int
  val f841 : int -> int -> int
  val f842 : int -> int -> int
  val f843 : int -> int -> int
  val f844 : int -> int -> int
  val f845 : int -> int -> int
  val f846 : int -> int -> int
  val f847 : int -> int -> int
  val f848 : int -> int -> int
  val f849 : int -> int -> int
  val f850 : int -> int -> int
  val f851 : int -> int -> int
  val f852 : int -> int -> int
  val f853 : int -> int -> int
  val f854 : int -> int -> int
  val f855 : int -> int -> int
  val f856 : int -> int -> int
  val f857 : int -> int -> int
  val f858 : int -> int -> int
  val f859 : int -> int -> int
  val f860 : int -> int -> int
  val f861 : int -> int -> int
  val f862 : int -> int -> int
  val f863 : int -> int -> int
  val f864 : int -> int -> int
  val f865 : int -> int -> int
  val f866 : int -> int -> int
  val f867 : int -> int -> int
  val f868 : int -> int -> int
  val f869 : int -> int -> int
  val f870 : int -> int -> int
  val f871 : int -> int -> int
  val f872 : int -> int -> int
  val f873 : int -> int -> int
  val f874 : int -> int -> int
  val f875 : int -> int -> int
  val f876 : int -> int -> int
  val f877 : int -> int -> int
  val f878 : int -> int -> int
  val f879 : int -> int -> int
  val f880 : int -> int -> int
  val f881 : int -> int -> int
  val f882 : int -> int -> int
  val f883 : int -> int -> int
  val f884 : int -> int -> int
  val f885 : int -> int -> int
  val f886 : int -> int -> int
  val f887 : int -> int -> int
  val f888 : int -> int -> int
  val f889 : int -> int -> int
  val f890 : int -> int -> int
  val f891 : int -> int -> int
  val f892 : int -> int -> int
  val f893 : int -> int -> int
  val f894 : int -> int -> int
  val f895 : int -> int -> int
  val f896 : int -> int -> int
  val f897 : int -> int -> int
  val f898 : int -> int -> int
  val f899 : int -> int -> int
  val f900 : int -> int -> int
  val f901 : int -> int -> int
  val f902 : int -> int -> int
  val f903 : int -> int -> int
  val f904 : int -> int -> int
  val f905 : int -> int -> int
  val f906 : int -> int -> int
  val f907 : int -> int -> int
  val f908 : int -> int -> int
  val f909 : int -> int -> int
  val f910 : int -> int -> int
  val f911 : int -> int -> int
  val f912 : int -> int -> int
  val f913 : int -> int -> int
  val f914 : int -> int -> int
  val f915 : int -> int -> int
  val f916 : int -> int -> int
  val f917 : int -> int -> int
  val f918 : int -> int -> int
  val f919 : int -> int -> int
  val f920 : int -> int -> int
  val f921 : int -> int -> int
  val f922 : int -> int -> int
  val f923 : int -> int -> int
  val f924 : int -> int -> int
  val f925 : int -> int -> int
  val f926 : int -> int -> int
  val f927 : int -> int -> int
  val f928 : int -> int -> int
  val f929 : int -> int -> int
  val f930 : int -> int -> int
  val f931 : int -> int -> int
  val f932 : int -> int -> int
  val f933 : int -> int -> int
  val f934 : int -> int -> int
  val f935 : int -> int -> int
  val f936 : int -> int -> int
  val f937 : int -> int -> int
  val f938 : int -> int -> int
  val f939 : int -> int -> int
  val f940 : int -> int -> int
  val f941 : int -> int -> int
  val f942 : int -> int -> int
  val f943 : int -> int -> int
  val f944 : int -> int -> int
  val f945 : int -> int -> int
  val f946 : int -> int -> int
  val f947 : int -> int -> int
  val f948 : int -> int -> int
  val f949 : int -> int -> int
  val f950 : int -> int -> int
  val f951 : int -> int -> int
  val f952 : int -> int -> int
  val f953 : int -> int -> int
  val f954 : int -> int -> int
  val f955 : int -> int -> int
  val f956 : int -> int -> int
  val f957 : int -> int -> int
  val f958 : int -> int -> int
  val f959 : int -> int -> int
  val f960 : int -> int -> int
  val f961 : int -> int -> int
  val f962 : int -> int -> int
  val f963 : int -> int -> int
  val f964 : int -> int -> int
  val f965 : int -> int -> int
  val f966 : int -> int -> int
  val f967 : int -> int -> int
  val f968 : int -> int -> int
  val f969 : int -> int -> int
  val f970 : int -> int -> int
  val f971 : int -> int -> int
  val f972 : int -> int -> int
  val f973 : int -> int -> int
  val f974 : int -> int -> int
  val f975 : int -> int -> int
  val f976 : int -> int -> int
  val f977 : int -> int -> int
  val f978 : int -> int -> int
  val f979 : int -> int -> int
  val f980 : int -> int -> int
  val f981 : int -> int -> int
  val f982 : int -> int -> int
  val f983 : int -> int -> int
  val f984 : int -> int -> int
  val f985 : int -> int -> int
  val f986 : int -> int -> int
  val f987 : int -> int -> int
  val f988 : int -> int -> int
  val f989 : int -> int -> int
  val f990 : int -> int -> int
  val f991 : int -> int -> int
  val f992 : int -> int -> int
  val f993 : int -> int -> int
  val f994 : int -> int -> int
  val f995 : int -> int -> int
  val f996 : int -> int -> int
  val f997 : int -> int -> int
  val f998 : int -> int -> int
  val f999 : int -> int -> int
  val f1000 : int -> int -> int
end

module Make (M : S) = struct
  include M
end

module M = struct
(* <%- for i in 0 .. 1000 do -%>
  let f<%= i %> = ( + )
<%- end -%>                      *)
  let f0 = ( + )
  let f1 = ( + )
  let f2 = ( + )
  let f3 = ( + )
  let f4 = ( + )
  let f5 = ( + )
  let f6 = ( + )
  let f7 = ( + )
  let f8 = ( + )
  let f9 = ( + )
  let f10 = ( + )
  let f11 = ( + )
  let f12 = ( + )
  let f13 = ( + )
  let f14 = ( + )
  let f15 = ( + )
  let f16 = ( + )
  let f17 = ( + )
  let f18 = ( + )
  let f19 = ( + )
  let f20 = ( + )
  let f21 = ( + )
  let f22 = ( + )
  let f23 = ( + )
  let f24 = ( + )
  let f25 = ( + )
  let f26 = ( + )
  let f27 = ( + )
  let f28 = ( + )
  let f29 = ( + )
  let f30 = ( + )
  let f31 = ( + )
  let f32 = ( + )
  let f33 = ( + )
  let f34 = ( + )
  let f35 = ( + )
  let f36 = ( + )
  let f37 = ( + )
  let f38 = ( + )
  let f39 = ( + )
  let f40 = ( + )
  let f41 = ( + )
  let f42 = ( + )
  let f43 = ( + )
  let f44 = ( + )
  let f45 = ( + )
  let f46 = ( + )
  let f47 = ( + )
  let f48 = ( + )
  let f49 = ( + )
  let f50 = ( + )
  let f51 = ( + )
  let f52 = ( + )
  let f53 = ( + )
  let f54 = ( + )
  let f55 = ( + )
  let f56 = ( + )
  let f57 = ( + )
  let f58 = ( + )
  let f59 = ( + )
  let f60 = ( + )
  let f61 = ( + )
  let f62 = ( + )
  let f63 = ( + )
  let f64 = ( + )
  let f65 = ( + )
  let f66 = ( + )
  let f67 = ( + )
  let f68 = ( + )
  let f69 = ( + )
  let f70 = ( + )
  let f71 = ( + )
  let f72 = ( + )
  let f73 = ( + )
  let f74 = ( + )
  let f75 = ( + )
  let f76 = ( + )
  let f77 = ( + )
  let f78 = ( + )
  let f79 = ( + )
  let f80 = ( + )
  let f81 = ( + )
  let f82 = ( + )
  let f83 = ( + )
  let f84 = ( + )
  let f85 = ( + )
  let f86 = ( + )
  let f87 = ( + )
  let f88 = ( + )
  let f89 = ( + )
  let f90 = ( + )
  let f91 = ( + )
  let f92 = ( + )
  let f93 = ( + )
  let f94 = ( + )
  let f95 = ( + )
  let f96 = ( + )
  let f97 = ( + )
  let f98 = ( + )
  let f99 = ( + )
  let f100 = ( + )
  let f101 = ( + )
  let f102 = ( + )
  let f103 = ( + )
  let f104 = ( + )
  let f105 = ( + )
  let f106 = ( + )
  let f107 = ( + )
  let f108 = ( + )
  let f109 = ( + )
  let f110 = ( + )
  let f111 = ( + )
  let f112 = ( + )
  let f113 = ( + )
  let f114 = ( + )
  let f115 = ( + )
  let f116 = ( + )
  let f117 = ( + )
  let f118 = ( + )
  let f119 = ( + )
  let f120 = ( + )
  let f121 = ( + )
  let f122 = ( + )
  let f123 = ( + )
  let f124 = ( + )
  let f125 = ( + )
  let f126 = ( + )
  let f127 = ( + )
  let f128 = ( + )
  let f129 = ( + )
  let f130 = ( + )
  let f131 = ( + )
  let f132 = ( + )
  let f133 = ( + )
  let f134 = ( + )
  let f135 = ( + )
  let f136 = ( + )
  let f137 = ( + )
  let f138 = ( + )
  let f139 = ( + )
  let f140 = ( + )
  let f141 = ( + )
  let f142 = ( + )
  let f143 = ( + )
  let f144 = ( + )
  let f145 = ( + )
  let f146 = ( + )
  let f147 = ( + )
  let f148 = ( + )
  let f149 = ( + )
  let f150 = ( + )
  let f151 = ( + )
  let f152 = ( + )
  let f153 = ( + )
  let f154 = ( + )
  let f155 = ( + )
  let f156 = ( + )
  let f157 = ( + )
  let f158 = ( + )
  let f159 = ( + )
  let f160 = ( + )
  let f161 = ( + )
  let f162 = ( + )
  let f163 = ( + )
  let f164 = ( + )
  let f165 = ( + )
  let f166 = ( + )
  let f167 = ( + )
  let f168 = ( + )
  let f169 = ( + )
  let f170 = ( + )
  let f171 = ( + )
  let f172 = ( + )
  let f173 = ( + )
  let f174 = ( + )
  let f175 = ( + )
  let f176 = ( + )
  let f177 = ( + )
  let f178 = ( + )
  let f179 = ( + )
  let f180 = ( + )
  let f181 = ( + )
  let f182 = ( + )
  let f183 = ( + )
  let f184 = ( + )
  let f185 = ( + )
  let f186 = ( + )
  let f187 = ( + )
  let f188 = ( + )
  let f189 = ( + )
  let f190 = ( + )
  let f191 = ( + )
  let f192 = ( + )
  let f193 = ( + )
  let f194 = ( + )
  let f195 = ( + )
  let f196 = ( + )
  let f197 = ( + )
  let f198 = ( + )
  let f199 = ( + )
  let f200 = ( + )
  let f201 = ( + )
  let f202 = ( + )
  let f203 = ( + )
  let f204 = ( + )
  let f205 = ( + )
  let f206 = ( + )
  let f207 = ( + )
  let f208 = ( + )
  let f209 = ( + )
  let f210 = ( + )
  let f211 = ( + )
  let f212 = ( + )
  let f213 = ( + )
  let f214 = ( + )
  let f215 = ( + )
  let f216 = ( + )
  let f217 = ( + )
  let f218 = ( + )
  let f219 = ( + )
  let f220 = ( + )
  let f221 = ( + )
  let f222 = ( + )
  let f223 = ( + )
  let f224 = ( + )
  let f225 = ( + )
  let f226 = ( + )
  let f227 = ( + )
  let f228 = ( + )
  let f229 = ( + )
  let f230 = ( + )
  let f231 = ( + )
  let f232 = ( + )
  let f233 = ( + )
  let f234 = ( + )
  let f235 = ( + )
  let f236 = ( + )
  let f237 = ( + )
  let f238 = ( + )
  let f239 = ( + )
  let f240 = ( + )
  let f241 = ( + )
  let f242 = ( + )
  let f243 = ( + )
  let f244 = ( + )
  let f245 = ( + )
  let f246 = ( + )
  let f247 = ( + )
  let f248 = ( + )
  let f249 = ( + )
  let f250 = ( + )
  let f251 = ( + )
  let f252 = ( + )
  let f253 = ( + )
  let f254 = ( + )
  let f255 = ( + )
  let f256 = ( + )
  let f257 = ( + )
  let f258 = ( + )
  let f259 = ( + )
  let f260 = ( + )
  let f261 = ( + )
  let f262 = ( + )
  let f263 = ( + )
  let f264 = ( + )
  let f265 = ( + )
  let f266 = ( + )
  let f267 = ( + )
  let f268 = ( + )
  let f269 = ( + )
  let f270 = ( + )
  let f271 = ( + )
  let f272 = ( + )
  let f273 = ( + )
  let f274 = ( + )
  let f275 = ( + )
  let f276 = ( + )
  let f277 = ( + )
  let f278 = ( + )
  let f279 = ( + )
  let f280 = ( + )
  let f281 = ( + )
  let f282 = ( + )
  let f283 = ( + )
  let f284 = ( + )
  let f285 = ( + )
  let f286 = ( + )
  let f287 = ( + )
  let f288 = ( + )
  let f289 = ( + )
  let f290 = ( + )
  let f291 = ( + )
  let f292 = ( + )
  let f293 = ( + )
  let f294 = ( + )
  let f295 = ( + )
  let f296 = ( + )
  let f297 = ( + )
  let f298 = ( + )
  let f299 = ( + )
  let f300 = ( + )
  let f301 = ( + )
  let f302 = ( + )
  let f303 = ( + )
  let f304 = ( + )
  let f305 = ( + )
  let f306 = ( + )
  let f307 = ( + )
  let f308 = ( + )
  let f309 = ( + )
  let f310 = ( + )
  let f311 = ( + )
  let f312 = ( + )
  let f313 = ( + )
  let f314 = ( + )
  let f315 = ( + )
  let f316 = ( + )
  let f317 = ( + )
  let f318 = ( + )
  let f319 = ( + )
  let f320 = ( + )
  let f321 = ( + )
  let f322 = ( + )
  let f323 = ( + )
  let f324 = ( + )
  let f325 = ( + )
  let f326 = ( + )
  let f327 = ( + )
  let f328 = ( + )
  let f329 = ( + )
  let f330 = ( + )
  let f331 = ( + )
  let f332 = ( + )
  let f333 = ( + )
  let f334 = ( + )
  let f335 = ( + )
  let f336 = ( + )
  let f337 = ( + )
  let f338 = ( + )
  let f339 = ( + )
  let f340 = ( + )
  let f341 = ( + )
  let f342 = ( + )
  let f343 = ( + )
  let f344 = ( + )
  let f345 = ( + )
  let f346 = ( + )
  let f347 = ( + )
  let f348 = ( + )
  let f349 = ( + )
  let f350 = ( + )
  let f351 = ( + )
  let f352 = ( + )
  let f353 = ( + )
  let f354 = ( + )
  let f355 = ( + )
  let f356 = ( + )
  let f357 = ( + )
  let f358 = ( + )
  let f359 = ( + )
  let f360 = ( + )
  let f361 = ( + )
  let f362 = ( + )
  let f363 = ( + )
  let f364 = ( + )
  let f365 = ( + )
  let f366 = ( + )
  let f367 = ( + )
  let f368 = ( + )
  let f369 = ( + )
  let f370 = ( + )
  let f371 = ( + )
  let f372 = ( + )
  let f373 = ( + )
  let f374 = ( + )
  let f375 = ( + )
  let f376 = ( + )
  let f377 = ( + )
  let f378 = ( + )
  let f379 = ( + )
  let f380 = ( + )
  let f381 = ( + )
  let f382 = ( + )
  let f383 = ( + )
  let f384 = ( + )
  let f385 = ( + )
  let f386 = ( + )
  let f387 = ( + )
  let f388 = ( + )
  let f389 = ( + )
  let f390 = ( + )
  let f391 = ( + )
  let f392 = ( + )
  let f393 = ( + )
  let f394 = ( + )
  let f395 = ( + )
  let f396 = ( + )
  let f397 = ( + )
  let f398 = ( + )
  let f399 = ( + )
  let f400 = ( + )
  let f401 = ( + )
  let f402 = ( + )
  let f403 = ( + )
  let f404 = ( + )
  let f405 = ( + )
  let f406 = ( + )
  let f407 = ( + )
  let f408 = ( + )
  let f409 = ( + )
  let f410 = ( + )
  let f411 = ( + )
  let f412 = ( + )
  let f413 = ( + )
  let f414 = ( + )
  let f415 = ( + )
  let f416 = ( + )
  let f417 = ( + )
  let f418 = ( + )
  let f419 = ( + )
  let f420 = ( + )
  let f421 = ( + )
  let f422 = ( + )
  let f423 = ( + )
  let f424 = ( + )
  let f425 = ( + )
  let f426 = ( + )
  let f427 = ( + )
  let f428 = ( + )
  let f429 = ( + )
  let f430 = ( + )
  let f431 = ( + )
  let f432 = ( + )
  let f433 = ( + )
  let f434 = ( + )
  let f435 = ( + )
  let f436 = ( + )
  let f437 = ( + )
  let f438 = ( + )
  let f439 = ( + )
  let f440 = ( + )
  let f441 = ( + )
  let f442 = ( + )
  let f443 = ( + )
  let f444 = ( + )
  let f445 = ( + )
  let f446 = ( + )
  let f447 = ( + )
  let f448 = ( + )
  let f449 = ( + )
  let f450 = ( + )
  let f451 = ( + )
  let f452 = ( + )
  let f453 = ( + )
  let f454 = ( + )
  let f455 = ( + )
  let f456 = ( + )
  let f457 = ( + )
  let f458 = ( + )
  let f459 = ( + )
  let f460 = ( + )
  let f461 = ( + )
  let f462 = ( + )
  let f463 = ( + )
  let f464 = ( + )
  let f465 = ( + )
  let f466 = ( + )
  let f467 = ( + )
  let f468 = ( + )
  let f469 = ( + )
  let f470 = ( + )
  let f471 = ( + )
  let f472 = ( + )
  let f473 = ( + )
  let f474 = ( + )
  let f475 = ( + )
  let f476 = ( + )
  let f477 = ( + )
  let f478 = ( + )
  let f479 = ( + )
  let f480 = ( + )
  let f481 = ( + )
  let f482 = ( + )
  let f483 = ( + )
  let f484 = ( + )
  let f485 = ( + )
  let f486 = ( + )
  let f487 = ( + )
  let f488 = ( + )
  let f489 = ( + )
  let f490 = ( + )
  let f491 = ( + )
  let f492 = ( + )
  let f493 = ( + )
  let f494 = ( + )
  let f495 = ( + )
  let f496 = ( + )
  let f497 = ( + )
  let f498 = ( + )
  let f499 = ( + )
  let f500 = ( + )
  let f501 = ( + )
  let f502 = ( + )
  let f503 = ( + )
  let f504 = ( + )
  let f505 = ( + )
  let f506 = ( + )
  let f507 = ( + )
  let f508 = ( + )
  let f509 = ( + )
  let f510 = ( + )
  let f511 = ( + )
  let f512 = ( + )
  let f513 = ( + )
  let f514 = ( + )
  let f515 = ( + )
  let f516 = ( + )
  let f517 = ( + )
  let f518 = ( + )
  let f519 = ( + )
  let f520 = ( + )
  let f521 = ( + )
  let f522 = ( + )
  let f523 = ( + )
  let f524 = ( + )
  let f525 = ( + )
  let f526 = ( + )
  let f527 = ( + )
  let f528 = ( + )
  let f529 = ( + )
  let f530 = ( + )
  let f531 = ( + )
  let f532 = ( + )
  let f533 = ( + )
  let f534 = ( + )
  let f535 = ( + )
  let f536 = ( + )
  let f537 = ( + )
  let f538 = ( + )
  let f539 = ( + )
  let f540 = ( + )
  let f541 = ( + )
  let f542 = ( + )
  let f543 = ( + )
  let f544 = ( + )
  let f545 = ( + )
  let f546 = ( + )
  let f547 = ( + )
  let f548 = ( + )
  let f549 = ( + )
  let f550 = ( + )
  let f551 = ( + )
  let f552 = ( + )
  let f553 = ( + )
  let f554 = ( + )
  let f555 = ( + )
  let f556 = ( + )
  let f557 = ( + )
  let f558 = ( + )
  let f559 = ( + )
  let f560 = ( + )
  let f561 = ( + )
  let f562 = ( + )
  let f563 = ( + )
  let f564 = ( + )
  let f565 = ( + )
  let f566 = ( + )
  let f567 = ( + )
  let f568 = ( + )
  let f569 = ( + )
  let f570 = ( + )
  let f571 = ( + )
  let f572 = ( + )
  let f573 = ( + )
  let f574 = ( + )
  let f575 = ( + )
  let f576 = ( + )
  let f577 = ( + )
  let f578 = ( + )
  let f579 = ( + )
  let f580 = ( + )
  let f581 = ( + )
  let f582 = ( + )
  let f583 = ( + )
  let f584 = ( + )
  let f585 = ( + )
  let f586 = ( + )
  let f587 = ( + )
  let f588 = ( + )
  let f589 = ( + )
  let f590 = ( + )
  let f591 = ( + )
  let f592 = ( + )
  let f593 = ( + )
  let f594 = ( + )
  let f595 = ( + )
  let f596 = ( + )
  let f597 = ( + )
  let f598 = ( + )
  let f599 = ( + )
  let f600 = ( + )
  let f601 = ( + )
  let f602 = ( + )
  let f603 = ( + )
  let f604 = ( + )
  let f605 = ( + )
  let f606 = ( + )
  let f607 = ( + )
  let f608 = ( + )
  let f609 = ( + )
  let f610 = ( + )
  let f611 = ( + )
  let f612 = ( + )
  let f613 = ( + )
  let f614 = ( + )
  let f615 = ( + )
  let f616 = ( + )
  let f617 = ( + )
  let f618 = ( + )
  let f619 = ( + )
  let f620 = ( + )
  let f621 = ( + )
  let f622 = ( + )
  let f623 = ( + )
  let f624 = ( + )
  let f625 = ( + )
  let f626 = ( + )
  let f627 = ( + )
  let f628 = ( + )
  let f629 = ( + )
  let f630 = ( + )
  let f631 = ( + )
  let f632 = ( + )
  let f633 = ( + )
  let f634 = ( + )
  let f635 = ( + )
  let f636 = ( + )
  let f637 = ( + )
  let f638 = ( + )
  let f639 = ( + )
  let f640 = ( + )
  let f641 = ( + )
  let f642 = ( + )
  let f643 = ( + )
  let f644 = ( + )
  let f645 = ( + )
  let f646 = ( + )
  let f647 = ( + )
  let f648 = ( + )
  let f649 = ( + )
  let f650 = ( + )
  let f651 = ( + )
  let f652 = ( + )
  let f653 = ( + )
  let f654 = ( + )
  let f655 = ( + )
  let f656 = ( + )
  let f657 = ( + )
  let f658 = ( + )
  let f659 = ( + )
  let f660 = ( + )
  let f661 = ( + )
  let f662 = ( + )
  let f663 = ( + )
  let f664 = ( + )
  let f665 = ( + )
  let f666 = ( + )
  let f667 = ( + )
  let f668 = ( + )
  let f669 = ( + )
  let f670 = ( + )
  let f671 = ( + )
  let f672 = ( + )
  let f673 = ( + )
  let f674 = ( + )
  let f675 = ( + )
  let f676 = ( + )
  let f677 = ( + )
  let f678 = ( + )
  let f679 = ( + )
  let f680 = ( + )
  let f681 = ( + )
  let f682 = ( + )
  let f683 = ( + )
  let f684 = ( + )
  let f685 = ( + )
  let f686 = ( + )
  let f687 = ( + )
  let f688 = ( + )
  let f689 = ( + )
  let f690 = ( + )
  let f691 = ( + )
  let f692 = ( + )
  let f693 = ( + )
  let f694 = ( + )
  let f695 = ( + )
  let f696 = ( + )
  let f697 = ( + )
  let f698 = ( + )
  let f699 = ( + )
  let f700 = ( + )
  let f701 = ( + )
  let f702 = ( + )
  let f703 = ( + )
  let f704 = ( + )
  let f705 = ( + )
  let f706 = ( + )
  let f707 = ( + )
  let f708 = ( + )
  let f709 = ( + )
  let f710 = ( + )
  let f711 = ( + )
  let f712 = ( + )
  let f713 = ( + )
  let f714 = ( + )
  let f715 = ( + )
  let f716 = ( + )
  let f717 = ( + )
  let f718 = ( + )
  let f719 = ( + )
  let f720 = ( + )
  let f721 = ( + )
  let f722 = ( + )
  let f723 = ( + )
  let f724 = ( + )
  let f725 = ( + )
  let f726 = ( + )
  let f727 = ( + )
  let f728 = ( + )
  let f729 = ( + )
  let f730 = ( + )
  let f731 = ( + )
  let f732 = ( + )
  let f733 = ( + )
  let f734 = ( + )
  let f735 = ( + )
  let f736 = ( + )
  let f737 = ( + )
  let f738 = ( + )
  let f739 = ( + )
  let f740 = ( + )
  let f741 = ( + )
  let f742 = ( + )
  let f743 = ( + )
  let f744 = ( + )
  let f745 = ( + )
  let f746 = ( + )
  let f747 = ( + )
  let f748 = ( + )
  let f749 = ( + )
  let f750 = ( + )
  let f751 = ( + )
  let f752 = ( + )
  let f753 = ( + )
  let f754 = ( + )
  let f755 = ( + )
  let f756 = ( + )
  let f757 = ( + )
  let f758 = ( + )
  let f759 = ( + )
  let f760 = ( + )
  let f761 = ( + )
  let f762 = ( + )
  let f763 = ( + )
  let f764 = ( + )
  let f765 = ( + )
  let f766 = ( + )
  let f767 = ( + )
  let f768 = ( + )
  let f769 = ( + )
  let f770 = ( + )
  let f771 = ( + )
  let f772 = ( + )
  let f773 = ( + )
  let f774 = ( + )
  let f775 = ( + )
  let f776 = ( + )
  let f777 = ( + )
  let f778 = ( + )
  let f779 = ( + )
  let f780 = ( + )
  let f781 = ( + )
  let f782 = ( + )
  let f783 = ( + )
  let f784 = ( + )
  let f785 = ( + )
  let f786 = ( + )
  let f787 = ( + )
  let f788 = ( + )
  let f789 = ( + )
  let f790 = ( + )
  let f791 = ( + )
  let f792 = ( + )
  let f793 = ( + )
  let f794 = ( + )
  let f795 = ( + )
  let f796 = ( + )
  let f797 = ( + )
  let f798 = ( + )
  let f799 = ( + )
  let f800 = ( + )
  let f801 = ( + )
  let f802 = ( + )
  let f803 = ( + )
  let f804 = ( + )
  let f805 = ( + )
  let f806 = ( + )
  let f807 = ( + )
  let f808 = ( + )
  let f809 = ( + )
  let f810 = ( + )
  let f811 = ( + )
  let f812 = ( + )
  let f813 = ( + )
  let f814 = ( + )
  let f815 = ( + )
  let f816 = ( + )
  let f817 = ( + )
  let f818 = ( + )
  let f819 = ( + )
  let f820 = ( + )
  let f821 = ( + )
  let f822 = ( + )
  let f823 = ( + )
  let f824 = ( + )
  let f825 = ( + )
  let f826 = ( + )
  let f827 = ( + )
  let f828 = ( + )
  let f829 = ( + )
  let f830 = ( + )
  let f831 = ( + )
  let f832 = ( + )
  let f833 = ( + )
  let f834 = ( + )
  let f835 = ( + )
  let f836 = ( + )
  let f837 = ( + )
  let f838 = ( + )
  let f839 = ( + )
  let f840 = ( + )
  let f841 = ( + )
  let f842 = ( + )
  let f843 = ( + )
  let f844 = ( + )
  let f845 = ( + )
  let f846 = ( + )
  let f847 = ( + )
  let f848 = ( + )
  let f849 = ( + )
  let f850 = ( + )
  let f851 = ( + )
  let f852 = ( + )
  let f853 = ( + )
  let f854 = ( + )
  let f855 = ( + )
  let f856 = ( + )
  let f857 = ( + )
  let f858 = ( + )
  let f859 = ( + )
  let f860 = ( + )
  let f861 = ( + )
  let f862 = ( + )
  let f863 = ( + )
  let f864 = ( + )
  let f865 = ( + )
  let f866 = ( + )
  let f867 = ( + )
  let f868 = ( + )
  let f869 = ( + )
  let f870 = ( + )
  let f871 = ( + )
  let f872 = ( + )
  let f873 = ( + )
  let f874 = ( + )
  let f875 = ( + )
  let f876 = ( + )
  let f877 = ( + )
  let f878 = ( + )
  let f879 = ( + )
  let f880 = ( + )
  let f881 = ( + )
  let f882 = ( + )
  let f883 = ( + )
  let f884 = ( + )
  let f885 = ( + )
  let f886 = ( + )
  let f887 = ( + )
  let f888 = ( + )
  let f889 = ( + )
  let f890 = ( + )
  let f891 = ( + )
  let f892 = ( + )
  let f893 = ( + )
  let f894 = ( + )
  let f895 = ( + )
  let f896 = ( + )
  let f897 = ( + )
  let f898 = ( + )
  let f899 = ( + )
  let f900 = ( + )
  let f901 = ( + )
  let f902 = ( + )
  let f903 = ( + )
  let f904 = ( + )
  let f905 = ( + )
  let f906 = ( + )
  let f907 = ( + )
  let f908 = ( + )
  let f909 = ( + )
  let f910 = ( + )
  let f911 = ( + )
  let f912 = ( + )
  let f913 = ( + )
  let f914 = ( + )
  let f915 = ( + )
  let f916 = ( + )
  let f917 = ( + )
  let f918 = ( + )
  let f919 = ( + )
  let f920 = ( + )
  let f921 = ( + )
  let f922 = ( + )
  let f923 = ( + )
  let f924 = ( + )
  let f925 = ( + )
  let f926 = ( + )
  let f927 = ( + )
  let f928 = ( + )
  let f929 = ( + )
  let f930 = ( + )
  let f931 = ( + )
  let f932 = ( + )
  let f933 = ( + )
  let f934 = ( + )
  let f935 = ( + )
  let f936 = ( + )
  let f937 = ( + )
  let f938 = ( + )
  let f939 = ( + )
  let f940 = ( + )
  let f941 = ( + )
  let f942 = ( + )
  let f943 = ( + )
  let f944 = ( + )
  let f945 = ( + )
  let f946 = ( + )
  let f947 = ( + )
  let f948 = ( + )
  let f949 = ( + )
  let f950 = ( + )
  let f951 = ( + )
  let f952 = ( + )
  let f953 = ( + )
  let f954 = ( + )
  let f955 = ( + )
  let f956 = ( + )
  let f957 = ( + )
  let f958 = ( + )
  let f959 = ( + )
  let f960 = ( + )
  let f961 = ( + )
  let f962 = ( + )
  let f963 = ( + )
  let f964 = ( + )
  let f965 = ( + )
  let f966 = ( + )
  let f967 = ( + )
  let f968 = ( + )
  let f969 = ( + )
  let f970 = ( + )
  let f971 = ( + )
  let f972 = ( + )
  let f973 = ( + )
  let f974 = ( + )
  let f975 = ( + )
  let f976 = ( + )
  let f977 = ( + )
  let f978 = ( + )
  let f979 = ( + )
  let f980 = ( + )
  let f981 = ( + )
  let f982 = ( + )
  let f983 = ( + )
  let f984 = ( + )
  let f985 = ( + )
  let f986 = ( + )
  let f987 = ( + )
  let f988 = ( + )
  let f989 = ( + )
  let f990 = ( + )
  let f991 = ( + )
  let f992 = ( + )
  let f993 = ( + )
  let f994 = ( + )
  let f995 = ( + )
  let f996 = ( + )
  let f997 = ( + )
  let f998 = ( + )
  let f999 = ( + )
  let f1000 = ( + )
end

module X = Make(Make(Make(M)))
