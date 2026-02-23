(* TEST *)

(* Regression test for #14574:
   Generate a function with a large stack frame, so that offsets from
   the stack pointer may not fit in a single addition with immediates on Arm64.

   Generated with the following script:

let () =
  Format.printf "let f (a : int array) g =@.";
  for i = 0 to 520 do
    Format.printf "  let x%d = a.(%d) in@." i i
  done;
  Format.printf "  (* Force a stack frame with all variables live *)@.";
  Format.printf "  g ();@.";
  Format.printf "  [|@.";
  for i = 0 to 520 do
    Format.printf "    x%d;@." i
  done;
  Format.printf "  |]@.";
  ()

  The constant 520 was chosen to be a little above 512; the maximum
  offset representable as an immediate is 4095, and with 8 bytes per
  word on the stack this should be enough to trigger potential issues.
*)

let f (a : int array) g =
  let x0 = a.(0) in
  let x1 = a.(1) in
  let x2 = a.(2) in
  let x3 = a.(3) in
  let x4 = a.(4) in
  let x5 = a.(5) in
  let x6 = a.(6) in
  let x7 = a.(7) in
  let x8 = a.(8) in
  let x9 = a.(9) in
  let x10 = a.(10) in
  let x11 = a.(11) in
  let x12 = a.(12) in
  let x13 = a.(13) in
  let x14 = a.(14) in
  let x15 = a.(15) in
  let x16 = a.(16) in
  let x17 = a.(17) in
  let x18 = a.(18) in
  let x19 = a.(19) in
  let x20 = a.(20) in
  let x21 = a.(21) in
  let x22 = a.(22) in
  let x23 = a.(23) in
  let x24 = a.(24) in
  let x25 = a.(25) in
  let x26 = a.(26) in
  let x27 = a.(27) in
  let x28 = a.(28) in
  let x29 = a.(29) in
  let x30 = a.(30) in
  let x31 = a.(31) in
  let x32 = a.(32) in
  let x33 = a.(33) in
  let x34 = a.(34) in
  let x35 = a.(35) in
  let x36 = a.(36) in
  let x37 = a.(37) in
  let x38 = a.(38) in
  let x39 = a.(39) in
  let x40 = a.(40) in
  let x41 = a.(41) in
  let x42 = a.(42) in
  let x43 = a.(43) in
  let x44 = a.(44) in
  let x45 = a.(45) in
  let x46 = a.(46) in
  let x47 = a.(47) in
  let x48 = a.(48) in
  let x49 = a.(49) in
  let x50 = a.(50) in
  let x51 = a.(51) in
  let x52 = a.(52) in
  let x53 = a.(53) in
  let x54 = a.(54) in
  let x55 = a.(55) in
  let x56 = a.(56) in
  let x57 = a.(57) in
  let x58 = a.(58) in
  let x59 = a.(59) in
  let x60 = a.(60) in
  let x61 = a.(61) in
  let x62 = a.(62) in
  let x63 = a.(63) in
  let x64 = a.(64) in
  let x65 = a.(65) in
  let x66 = a.(66) in
  let x67 = a.(67) in
  let x68 = a.(68) in
  let x69 = a.(69) in
  let x70 = a.(70) in
  let x71 = a.(71) in
  let x72 = a.(72) in
  let x73 = a.(73) in
  let x74 = a.(74) in
  let x75 = a.(75) in
  let x76 = a.(76) in
  let x77 = a.(77) in
  let x78 = a.(78) in
  let x79 = a.(79) in
  let x80 = a.(80) in
  let x81 = a.(81) in
  let x82 = a.(82) in
  let x83 = a.(83) in
  let x84 = a.(84) in
  let x85 = a.(85) in
  let x86 = a.(86) in
  let x87 = a.(87) in
  let x88 = a.(88) in
  let x89 = a.(89) in
  let x90 = a.(90) in
  let x91 = a.(91) in
  let x92 = a.(92) in
  let x93 = a.(93) in
  let x94 = a.(94) in
  let x95 = a.(95) in
  let x96 = a.(96) in
  let x97 = a.(97) in
  let x98 = a.(98) in
  let x99 = a.(99) in
  let x100 = a.(100) in
  let x101 = a.(101) in
  let x102 = a.(102) in
  let x103 = a.(103) in
  let x104 = a.(104) in
  let x105 = a.(105) in
  let x106 = a.(106) in
  let x107 = a.(107) in
  let x108 = a.(108) in
  let x109 = a.(109) in
  let x110 = a.(110) in
  let x111 = a.(111) in
  let x112 = a.(112) in
  let x113 = a.(113) in
  let x114 = a.(114) in
  let x115 = a.(115) in
  let x116 = a.(116) in
  let x117 = a.(117) in
  let x118 = a.(118) in
  let x119 = a.(119) in
  let x120 = a.(120) in
  let x121 = a.(121) in
  let x122 = a.(122) in
  let x123 = a.(123) in
  let x124 = a.(124) in
  let x125 = a.(125) in
  let x126 = a.(126) in
  let x127 = a.(127) in
  let x128 = a.(128) in
  let x129 = a.(129) in
  let x130 = a.(130) in
  let x131 = a.(131) in
  let x132 = a.(132) in
  let x133 = a.(133) in
  let x134 = a.(134) in
  let x135 = a.(135) in
  let x136 = a.(136) in
  let x137 = a.(137) in
  let x138 = a.(138) in
  let x139 = a.(139) in
  let x140 = a.(140) in
  let x141 = a.(141) in
  let x142 = a.(142) in
  let x143 = a.(143) in
  let x144 = a.(144) in
  let x145 = a.(145) in
  let x146 = a.(146) in
  let x147 = a.(147) in
  let x148 = a.(148) in
  let x149 = a.(149) in
  let x150 = a.(150) in
  let x151 = a.(151) in
  let x152 = a.(152) in
  let x153 = a.(153) in
  let x154 = a.(154) in
  let x155 = a.(155) in
  let x156 = a.(156) in
  let x157 = a.(157) in
  let x158 = a.(158) in
  let x159 = a.(159) in
  let x160 = a.(160) in
  let x161 = a.(161) in
  let x162 = a.(162) in
  let x163 = a.(163) in
  let x164 = a.(164) in
  let x165 = a.(165) in
  let x166 = a.(166) in
  let x167 = a.(167) in
  let x168 = a.(168) in
  let x169 = a.(169) in
  let x170 = a.(170) in
  let x171 = a.(171) in
  let x172 = a.(172) in
  let x173 = a.(173) in
  let x174 = a.(174) in
  let x175 = a.(175) in
  let x176 = a.(176) in
  let x177 = a.(177) in
  let x178 = a.(178) in
  let x179 = a.(179) in
  let x180 = a.(180) in
  let x181 = a.(181) in
  let x182 = a.(182) in
  let x183 = a.(183) in
  let x184 = a.(184) in
  let x185 = a.(185) in
  let x186 = a.(186) in
  let x187 = a.(187) in
  let x188 = a.(188) in
  let x189 = a.(189) in
  let x190 = a.(190) in
  let x191 = a.(191) in
  let x192 = a.(192) in
  let x193 = a.(193) in
  let x194 = a.(194) in
  let x195 = a.(195) in
  let x196 = a.(196) in
  let x197 = a.(197) in
  let x198 = a.(198) in
  let x199 = a.(199) in
  let x200 = a.(200) in
  let x201 = a.(201) in
  let x202 = a.(202) in
  let x203 = a.(203) in
  let x204 = a.(204) in
  let x205 = a.(205) in
  let x206 = a.(206) in
  let x207 = a.(207) in
  let x208 = a.(208) in
  let x209 = a.(209) in
  let x210 = a.(210) in
  let x211 = a.(211) in
  let x212 = a.(212) in
  let x213 = a.(213) in
  let x214 = a.(214) in
  let x215 = a.(215) in
  let x216 = a.(216) in
  let x217 = a.(217) in
  let x218 = a.(218) in
  let x219 = a.(219) in
  let x220 = a.(220) in
  let x221 = a.(221) in
  let x222 = a.(222) in
  let x223 = a.(223) in
  let x224 = a.(224) in
  let x225 = a.(225) in
  let x226 = a.(226) in
  let x227 = a.(227) in
  let x228 = a.(228) in
  let x229 = a.(229) in
  let x230 = a.(230) in
  let x231 = a.(231) in
  let x232 = a.(232) in
  let x233 = a.(233) in
  let x234 = a.(234) in
  let x235 = a.(235) in
  let x236 = a.(236) in
  let x237 = a.(237) in
  let x238 = a.(238) in
  let x239 = a.(239) in
  let x240 = a.(240) in
  let x241 = a.(241) in
  let x242 = a.(242) in
  let x243 = a.(243) in
  let x244 = a.(244) in
  let x245 = a.(245) in
  let x246 = a.(246) in
  let x247 = a.(247) in
  let x248 = a.(248) in
  let x249 = a.(249) in
  let x250 = a.(250) in
  let x251 = a.(251) in
  let x252 = a.(252) in
  let x253 = a.(253) in
  let x254 = a.(254) in
  let x255 = a.(255) in
  let x256 = a.(256) in
  let x257 = a.(257) in
  let x258 = a.(258) in
  let x259 = a.(259) in
  let x260 = a.(260) in
  let x261 = a.(261) in
  let x262 = a.(262) in
  let x263 = a.(263) in
  let x264 = a.(264) in
  let x265 = a.(265) in
  let x266 = a.(266) in
  let x267 = a.(267) in
  let x268 = a.(268) in
  let x269 = a.(269) in
  let x270 = a.(270) in
  let x271 = a.(271) in
  let x272 = a.(272) in
  let x273 = a.(273) in
  let x274 = a.(274) in
  let x275 = a.(275) in
  let x276 = a.(276) in
  let x277 = a.(277) in
  let x278 = a.(278) in
  let x279 = a.(279) in
  let x280 = a.(280) in
  let x281 = a.(281) in
  let x282 = a.(282) in
  let x283 = a.(283) in
  let x284 = a.(284) in
  let x285 = a.(285) in
  let x286 = a.(286) in
  let x287 = a.(287) in
  let x288 = a.(288) in
  let x289 = a.(289) in
  let x290 = a.(290) in
  let x291 = a.(291) in
  let x292 = a.(292) in
  let x293 = a.(293) in
  let x294 = a.(294) in
  let x295 = a.(295) in
  let x296 = a.(296) in
  let x297 = a.(297) in
  let x298 = a.(298) in
  let x299 = a.(299) in
  let x300 = a.(300) in
  let x301 = a.(301) in
  let x302 = a.(302) in
  let x303 = a.(303) in
  let x304 = a.(304) in
  let x305 = a.(305) in
  let x306 = a.(306) in
  let x307 = a.(307) in
  let x308 = a.(308) in
  let x309 = a.(309) in
  let x310 = a.(310) in
  let x311 = a.(311) in
  let x312 = a.(312) in
  let x313 = a.(313) in
  let x314 = a.(314) in
  let x315 = a.(315) in
  let x316 = a.(316) in
  let x317 = a.(317) in
  let x318 = a.(318) in
  let x319 = a.(319) in
  let x320 = a.(320) in
  let x321 = a.(321) in
  let x322 = a.(322) in
  let x323 = a.(323) in
  let x324 = a.(324) in
  let x325 = a.(325) in
  let x326 = a.(326) in
  let x327 = a.(327) in
  let x328 = a.(328) in
  let x329 = a.(329) in
  let x330 = a.(330) in
  let x331 = a.(331) in
  let x332 = a.(332) in
  let x333 = a.(333) in
  let x334 = a.(334) in
  let x335 = a.(335) in
  let x336 = a.(336) in
  let x337 = a.(337) in
  let x338 = a.(338) in
  let x339 = a.(339) in
  let x340 = a.(340) in
  let x341 = a.(341) in
  let x342 = a.(342) in
  let x343 = a.(343) in
  let x344 = a.(344) in
  let x345 = a.(345) in
  let x346 = a.(346) in
  let x347 = a.(347) in
  let x348 = a.(348) in
  let x349 = a.(349) in
  let x350 = a.(350) in
  let x351 = a.(351) in
  let x352 = a.(352) in
  let x353 = a.(353) in
  let x354 = a.(354) in
  let x355 = a.(355) in
  let x356 = a.(356) in
  let x357 = a.(357) in
  let x358 = a.(358) in
  let x359 = a.(359) in
  let x360 = a.(360) in
  let x361 = a.(361) in
  let x362 = a.(362) in
  let x363 = a.(363) in
  let x364 = a.(364) in
  let x365 = a.(365) in
  let x366 = a.(366) in
  let x367 = a.(367) in
  let x368 = a.(368) in
  let x369 = a.(369) in
  let x370 = a.(370) in
  let x371 = a.(371) in
  let x372 = a.(372) in
  let x373 = a.(373) in
  let x374 = a.(374) in
  let x375 = a.(375) in
  let x376 = a.(376) in
  let x377 = a.(377) in
  let x378 = a.(378) in
  let x379 = a.(379) in
  let x380 = a.(380) in
  let x381 = a.(381) in
  let x382 = a.(382) in
  let x383 = a.(383) in
  let x384 = a.(384) in
  let x385 = a.(385) in
  let x386 = a.(386) in
  let x387 = a.(387) in
  let x388 = a.(388) in
  let x389 = a.(389) in
  let x390 = a.(390) in
  let x391 = a.(391) in
  let x392 = a.(392) in
  let x393 = a.(393) in
  let x394 = a.(394) in
  let x395 = a.(395) in
  let x396 = a.(396) in
  let x397 = a.(397) in
  let x398 = a.(398) in
  let x399 = a.(399) in
  let x400 = a.(400) in
  let x401 = a.(401) in
  let x402 = a.(402) in
  let x403 = a.(403) in
  let x404 = a.(404) in
  let x405 = a.(405) in
  let x406 = a.(406) in
  let x407 = a.(407) in
  let x408 = a.(408) in
  let x409 = a.(409) in
  let x410 = a.(410) in
  let x411 = a.(411) in
  let x412 = a.(412) in
  let x413 = a.(413) in
  let x414 = a.(414) in
  let x415 = a.(415) in
  let x416 = a.(416) in
  let x417 = a.(417) in
  let x418 = a.(418) in
  let x419 = a.(419) in
  let x420 = a.(420) in
  let x421 = a.(421) in
  let x422 = a.(422) in
  let x423 = a.(423) in
  let x424 = a.(424) in
  let x425 = a.(425) in
  let x426 = a.(426) in
  let x427 = a.(427) in
  let x428 = a.(428) in
  let x429 = a.(429) in
  let x430 = a.(430) in
  let x431 = a.(431) in
  let x432 = a.(432) in
  let x433 = a.(433) in
  let x434 = a.(434) in
  let x435 = a.(435) in
  let x436 = a.(436) in
  let x437 = a.(437) in
  let x438 = a.(438) in
  let x439 = a.(439) in
  let x440 = a.(440) in
  let x441 = a.(441) in
  let x442 = a.(442) in
  let x443 = a.(443) in
  let x444 = a.(444) in
  let x445 = a.(445) in
  let x446 = a.(446) in
  let x447 = a.(447) in
  let x448 = a.(448) in
  let x449 = a.(449) in
  let x450 = a.(450) in
  let x451 = a.(451) in
  let x452 = a.(452) in
  let x453 = a.(453) in
  let x454 = a.(454) in
  let x455 = a.(455) in
  let x456 = a.(456) in
  let x457 = a.(457) in
  let x458 = a.(458) in
  let x459 = a.(459) in
  let x460 = a.(460) in
  let x461 = a.(461) in
  let x462 = a.(462) in
  let x463 = a.(463) in
  let x464 = a.(464) in
  let x465 = a.(465) in
  let x466 = a.(466) in
  let x467 = a.(467) in
  let x468 = a.(468) in
  let x469 = a.(469) in
  let x470 = a.(470) in
  let x471 = a.(471) in
  let x472 = a.(472) in
  let x473 = a.(473) in
  let x474 = a.(474) in
  let x475 = a.(475) in
  let x476 = a.(476) in
  let x477 = a.(477) in
  let x478 = a.(478) in
  let x479 = a.(479) in
  let x480 = a.(480) in
  let x481 = a.(481) in
  let x482 = a.(482) in
  let x483 = a.(483) in
  let x484 = a.(484) in
  let x485 = a.(485) in
  let x486 = a.(486) in
  let x487 = a.(487) in
  let x488 = a.(488) in
  let x489 = a.(489) in
  let x490 = a.(490) in
  let x491 = a.(491) in
  let x492 = a.(492) in
  let x493 = a.(493) in
  let x494 = a.(494) in
  let x495 = a.(495) in
  let x496 = a.(496) in
  let x497 = a.(497) in
  let x498 = a.(498) in
  let x499 = a.(499) in
  let x500 = a.(500) in
  let x501 = a.(501) in
  let x502 = a.(502) in
  let x503 = a.(503) in
  let x504 = a.(504) in
  let x505 = a.(505) in
  let x506 = a.(506) in
  let x507 = a.(507) in
  let x508 = a.(508) in
  let x509 = a.(509) in
  let x510 = a.(510) in
  let x511 = a.(511) in
  let x512 = a.(512) in
  let x513 = a.(513) in
  let x514 = a.(514) in
  let x515 = a.(515) in
  let x516 = a.(516) in
  let x517 = a.(517) in
  let x518 = a.(518) in
  let x519 = a.(519) in
  let x520 = a.(520) in
  (* Force a stack frame with all variables live *)
  g ();
  [|
    x0;
    x1;
    x2;
    x3;
    x4;
    x5;
    x6;
    x7;
    x8;
    x9;
    x10;
    x11;
    x12;
    x13;
    x14;
    x15;
    x16;
    x17;
    x18;
    x19;
    x20;
    x21;
    x22;
    x23;
    x24;
    x25;
    x26;
    x27;
    x28;
    x29;
    x30;
    x31;
    x32;
    x33;
    x34;
    x35;
    x36;
    x37;
    x38;
    x39;
    x40;
    x41;
    x42;
    x43;
    x44;
    x45;
    x46;
    x47;
    x48;
    x49;
    x50;
    x51;
    x52;
    x53;
    x54;
    x55;
    x56;
    x57;
    x58;
    x59;
    x60;
    x61;
    x62;
    x63;
    x64;
    x65;
    x66;
    x67;
    x68;
    x69;
    x70;
    x71;
    x72;
    x73;
    x74;
    x75;
    x76;
    x77;
    x78;
    x79;
    x80;
    x81;
    x82;
    x83;
    x84;
    x85;
    x86;
    x87;
    x88;
    x89;
    x90;
    x91;
    x92;
    x93;
    x94;
    x95;
    x96;
    x97;
    x98;
    x99;
    x100;
    x101;
    x102;
    x103;
    x104;
    x105;
    x106;
    x107;
    x108;
    x109;
    x110;
    x111;
    x112;
    x113;
    x114;
    x115;
    x116;
    x117;
    x118;
    x119;
    x120;
    x121;
    x122;
    x123;
    x124;
    x125;
    x126;
    x127;
    x128;
    x129;
    x130;
    x131;
    x132;
    x133;
    x134;
    x135;
    x136;
    x137;
    x138;
    x139;
    x140;
    x141;
    x142;
    x143;
    x144;
    x145;
    x146;
    x147;
    x148;
    x149;
    x150;
    x151;
    x152;
    x153;
    x154;
    x155;
    x156;
    x157;
    x158;
    x159;
    x160;
    x161;
    x162;
    x163;
    x164;
    x165;
    x166;
    x167;
    x168;
    x169;
    x170;
    x171;
    x172;
    x173;
    x174;
    x175;
    x176;
    x177;
    x178;
    x179;
    x180;
    x181;
    x182;
    x183;
    x184;
    x185;
    x186;
    x187;
    x188;
    x189;
    x190;
    x191;
    x192;
    x193;
    x194;
    x195;
    x196;
    x197;
    x198;
    x199;
    x200;
    x201;
    x202;
    x203;
    x204;
    x205;
    x206;
    x207;
    x208;
    x209;
    x210;
    x211;
    x212;
    x213;
    x214;
    x215;
    x216;
    x217;
    x218;
    x219;
    x220;
    x221;
    x222;
    x223;
    x224;
    x225;
    x226;
    x227;
    x228;
    x229;
    x230;
    x231;
    x232;
    x233;
    x234;
    x235;
    x236;
    x237;
    x238;
    x239;
    x240;
    x241;
    x242;
    x243;
    x244;
    x245;
    x246;
    x247;
    x248;
    x249;
    x250;
    x251;
    x252;
    x253;
    x254;
    x255;
    x256;
    x257;
    x258;
    x259;
    x260;
    x261;
    x262;
    x263;
    x264;
    x265;
    x266;
    x267;
    x268;
    x269;
    x270;
    x271;
    x272;
    x273;
    x274;
    x275;
    x276;
    x277;
    x278;
    x279;
    x280;
    x281;
    x282;
    x283;
    x284;
    x285;
    x286;
    x287;
    x288;
    x289;
    x290;
    x291;
    x292;
    x293;
    x294;
    x295;
    x296;
    x297;
    x298;
    x299;
    x300;
    x301;
    x302;
    x303;
    x304;
    x305;
    x306;
    x307;
    x308;
    x309;
    x310;
    x311;
    x312;
    x313;
    x314;
    x315;
    x316;
    x317;
    x318;
    x319;
    x320;
    x321;
    x322;
    x323;
    x324;
    x325;
    x326;
    x327;
    x328;
    x329;
    x330;
    x331;
    x332;
    x333;
    x334;
    x335;
    x336;
    x337;
    x338;
    x339;
    x340;
    x341;
    x342;
    x343;
    x344;
    x345;
    x346;
    x347;
    x348;
    x349;
    x350;
    x351;
    x352;
    x353;
    x354;
    x355;
    x356;
    x357;
    x358;
    x359;
    x360;
    x361;
    x362;
    x363;
    x364;
    x365;
    x366;
    x367;
    x368;
    x369;
    x370;
    x371;
    x372;
    x373;
    x374;
    x375;
    x376;
    x377;
    x378;
    x379;
    x380;
    x381;
    x382;
    x383;
    x384;
    x385;
    x386;
    x387;
    x388;
    x389;
    x390;
    x391;
    x392;
    x393;
    x394;
    x395;
    x396;
    x397;
    x398;
    x399;
    x400;
    x401;
    x402;
    x403;
    x404;
    x405;
    x406;
    x407;
    x408;
    x409;
    x410;
    x411;
    x412;
    x413;
    x414;
    x415;
    x416;
    x417;
    x418;
    x419;
    x420;
    x421;
    x422;
    x423;
    x424;
    x425;
    x426;
    x427;
    x428;
    x429;
    x430;
    x431;
    x432;
    x433;
    x434;
    x435;
    x436;
    x437;
    x438;
    x439;
    x440;
    x441;
    x442;
    x443;
    x444;
    x445;
    x446;
    x447;
    x448;
    x449;
    x450;
    x451;
    x452;
    x453;
    x454;
    x455;
    x456;
    x457;
    x458;
    x459;
    x460;
    x461;
    x462;
    x463;
    x464;
    x465;
    x466;
    x467;
    x468;
    x469;
    x470;
    x471;
    x472;
    x473;
    x474;
    x475;
    x476;
    x477;
    x478;
    x479;
    x480;
    x481;
    x482;
    x483;
    x484;
    x485;
    x486;
    x487;
    x488;
    x489;
    x490;
    x491;
    x492;
    x493;
    x494;
    x495;
    x496;
    x497;
    x498;
    x499;
    x500;
    x501;
    x502;
    x503;
    x504;
    x505;
    x506;
    x507;
    x508;
    x509;
    x510;
    x511;
    x512;
    x513;
    x514;
    x515;
    x516;
    x517;
    x518;
    x519;
    x520;
  |]
