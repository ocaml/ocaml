add_char_closure_1827
     66: (    0) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     66: (    3) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     71: (    1) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
     71: (    4) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
     77: (    2) resize_closure_1750: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=57,eval_size=-53,eval_benefit=5,functor=false,branch_depth=1}=no))
     77: (    5) resize_closure_1750: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=57,eval_size=-53,eval_benefit=5,functor=false,branch_depth=1}=no))

to_bytes_closure_1591
     29: (    1) Bytes.sub_closure_1692: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=43,eval_size=-39,eval_benefit=18,functor=false,branch_depth=0}=no))
     29: (    3) Bytes.sub_closure_1692: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=43,eval_size=-39,eval_benefit=18,functor=false,branch_depth=0}=no))
     58: (    0) Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     58: (    2) Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large

contents_closure_1575
     28: (    1) Bytes.sub_string_closure_1728: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=0}=yes))
     58: (    2) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     58: (    4) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     65: (    0) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: function obviously too large
     65: (    3) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=43,eval_size=-39,eval_benefit=18,functor=false,branch_depth=0}=no))
     65: (    5) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=43,eval_size=-39,eval_benefit=18,functor=false,branch_depth=0}=no))

add_bytes_closure_1959
     95: (    1) add_string_closure_1926 -> resize_closure_1750: function obviously too large
     95: (    4) add_string_closure_1926 -> resize_closure_1750: function obviously too large
     96: (    2) add_string_closure_1926 -> Bytes.blit_string_closure_1866: function obviously too large
     96: (    5) add_string_closure_1926 -> Bytes.blit_string_closure_1866: function obviously too large
     99: (    0) Bytes.anon-fn_2863: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=0,eval_size=4,eval_benefit=5,functor=false,branch_depth=0}=yes))
     99: (    3) add_string_closure_1926: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=26,eval_size=-22,eval_benefit=5,functor=false,branch_depth=0}=no))
     99: (    6) add_string_closure_1926: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=26,eval_size=-22,eval_benefit=5,functor=false,branch_depth=0}=no))

add_channel_closure_2024
     66: (    2) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     66: (    5) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     71: (    3) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
     71: (    6) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
    115: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
    115: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
    116: (    4) resize_closure_1750: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=56,eval_size=-52,eval_benefit=5,functor=false,branch_depth=1}=no))
    116: (    7) resize_closure_1750: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=56,eval_size=-52,eval_benefit=5,functor=false,branch_depth=1}=no))
    117: (    8) add_channel_rec_1988: did not try copying decl (did not try unrolling)

blit_closure_1643
     40: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     40: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))

add_buffer_closure_1973
     83: (    2) add_subbytes_closure_1910 -> add_substring_closure_1855 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     83: (    6) add_subbytes_closure_1910 -> add_substring_closure_1855 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     85: (    3) add_subbytes_closure_1910 -> add_substring_closure_1855 -> resize_closure_1750: function obviously too large
     85: (    7) add_subbytes_closure_1910 -> add_substring_closure_1855 -> resize_closure_1750: function obviously too large
     86: (    4) add_subbytes_closure_1910 -> add_substring_closure_1855 -> Bytes.blit_string_closure_1866: function obviously too large
     86: (    8) add_subbytes_closure_1910 -> add_substring_closure_1855 -> Bytes.blit_string_closure_1866: function obviously too large
     90: (    0) add_subbytes_closure_1910 -> add_substring_closure_1855: function obviously too large
     90: (    5) add_subbytes_closure_1910 -> add_substring_closure_1855: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=43,eval_size=-39,eval_benefit=18,functor=false,branch_depth=0}=no))
     90: (    9) add_subbytes_closure_1910 -> add_substring_closure_1855: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=43,eval_size=-39,eval_benefit=18,functor=false,branch_depth=0}=no))
    102: (    1) add_subbytes_closure_1910: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=0}=yes))

find_ident_closure_2226
     39: (    6) String.sub_closure_1592 -> Bytes.sub_closure_1692: function obviously too large
     39: (    9) String.sub_closure_1592 -> Bytes.sub_closure_1692: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=3}=no))
     39: (   11) String.sub_closure_1592 -> Bytes.sub_closure_1692: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=3}=no))
     39: (   24) String.sub_closure_1592 -> Bytes.sub_closure_1692: function obviously too large
     39: (   27) String.sub_closure_1592 -> Bytes.sub_closure_1692: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=2}=no))
     39: (   29) String.sub_closure_1592 -> Bytes.sub_closure_1692: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=2}=no))
     58: (    8) String.sub_closure_1592 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     58: (   10) String.sub_closure_1592 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     58: (   26) String.sub_closure_1592 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     58: (   28) String.sub_closure_1592 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
    134: (   14) advance_to_closing_closure_2111 -> advance_2865 -> advance_2865: function obviously too large
    134: (   19) advance_to_closing_closure_2111 -> advance_2906 -> advance_2906: function obviously too large
    136: (   15) advance_to_closing_closure_2111 -> advance_2865 -> advance_2865: function obviously too large
    136: (   20) advance_to_closing_closure_2111 -> advance_2906 -> advance_2906: function obviously too large
    137: (   16) advance_to_closing_closure_2111 -> advance_2865 -> advance_2865: function obviously too large
    137: (   21) advance_to_closing_closure_2111 -> advance_2906 -> advance_2906: function obviously too large
    138: (   17) advance_to_closing_closure_2111 -> advance_2865: function obviously too large
    138: (   22) advance_to_closing_closure_2111 -> advance_2906: function obviously too large
    144: (    0) advance_to_non_alpha_closure_2173 -> advance_2711 -> advance_2711: function obviously too large
    144: (    3) advance_to_non_alpha_closure_2173 -> advance_2741 -> advance_2741: function obviously too large
    146: (    1) advance_to_non_alpha_closure_2173 -> advance_2711: function obviously too large
    146: (    4) advance_to_non_alpha_closure_2173 -> advance_2741: function obviously too large
    155: (   12) closing_closure_2082: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=16,eval_size=-12,eval_benefit=5,functor=false,branch_depth=2}=no))
    155: (   13) closing_closure_2082: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=16,eval_size=-12,eval_benefit=5,functor=false,branch_depth=2}=no))
    155: (   18) advance_to_closing_closure_2111: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=71,eval_size=-67,eval_benefit=5,functor=false,branch_depth=2}=no))
    155: (   23) advance_to_closing_closure_2111: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=71,eval_size=-67,eval_benefit=5,functor=false,branch_depth=2}=no))
    156: (   25) String.sub_closure_1592: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=2}=yes))
    159: (    2) advance_to_non_alpha_closure_2173: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=64,eval_size=-60,eval_benefit=5,functor=false,branch_depth=3}=no))
    159: (    5) advance_to_non_alpha_closure_2173: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=64,eval_size=-60,eval_benefit=5,functor=false,branch_depth=3}=no))
    160: (    7) String.sub_closure_1592: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=3}=yes))

add_substitute_closure_2292
     39: (   21) subst_2304 -> find_ident_closure_2226 -> Bytes.sub_closure_1692: function obviously too large
     39: (   24) subst_2304 -> find_ident_closure_2226 -> Bytes.sub_closure_1692: function obviously too large
     39: (   27) subst_2304 -> find_ident_closure_2226 -> Bytes.sub_closure_1692: function obviously too large
     39: (   30) subst_2304 -> find_ident_closure_2226 -> Bytes.sub_closure_1692: function obviously too large
     77: (    0) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (    2) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (    4) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (    6) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (    9) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (   11) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (   15) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (   17) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (   39) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     77: (   41) subst_2304 -> add_char_closure_1827 -> resize_closure_1750: function obviously too large
     95: (   32) subst_2304 -> add_string_closure_1926 -> resize_closure_1750: function obviously too large
     95: (   35) subst_2304 -> add_string_closure_1926 -> resize_closure_1750: function obviously too large
     96: (   33) subst_2304 -> add_string_closure_1926 -> Bytes.blit_string_closure_1866: function obviously too large
     96: (   36) subst_2304 -> add_string_closure_1926 -> Bytes.blit_string_closure_1866: function obviously too large
    155: (   22) subst_2304 -> find_ident_closure_2226 -> closing_closure_2082: function obviously too large
    155: (   23) subst_2304 -> find_ident_closure_2226 -> advance_to_closing_closure_2111: function obviously too large
    155: (   28) subst_2304 -> find_ident_closure_2226 -> closing_closure_2082: function obviously too large
    155: (   29) subst_2304 -> find_ident_closure_2226 -> advance_to_closing_closure_2111: function obviously too large
    159: (   20) subst_2304 -> find_ident_closure_2226 -> advance_to_non_alpha_closure_2173: function obviously too large
    159: (   26) subst_2304 -> find_ident_closure_2226 -> advance_to_non_alpha_closure_2173: function obviously too large
    170: (   16) subst_2304 -> add_char_closure_1827: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=3}=no))
    170: (   18) subst_2304 -> add_char_closure_1827: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=3}=no))
    171: (   19) subst_2304 -> subst_2304: function obviously too large
    174: (   25) subst_2304 -> find_ident_closure_2226: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=76,eval_size=-72,eval_benefit=5,functor=false,branch_depth=3}=no))
    174: (   31) subst_2304 -> find_ident_closure_2226: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=76,eval_size=-72,eval_benefit=5,functor=false,branch_depth=3}=no))
    175: (   34) subst_2304 -> add_string_closure_1926: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=26,eval_size=-22,eval_benefit=5,functor=false,branch_depth=3}=no))
    175: (   37) subst_2304 -> add_string_closure_1926: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=26,eval_size=-22,eval_benefit=5,functor=false,branch_depth=3}=no))
    176: (   38) subst_2304 -> subst_2304: function obviously too large
    178: (    1) subst_2304 -> add_char_closure_1827: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=20,eval_size=-16,eval_benefit=5,functor=false,branch_depth=3}=no))
    178: (    3) subst_2304 -> add_char_closure_1827: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=20,eval_size=-16,eval_benefit=5,functor=false,branch_depth=3}=no))
    179: (    5) subst_2304 -> add_char_closure_1827: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=3}=no))
    179: (    7) subst_2304 -> add_char_closure_1827: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=3}=no))
    180: (    8) subst_2304 -> subst_2304: function obviously too large
    182: (   14) subst_2304 -> subst_2304: function obviously too large
    184: (   10) subst_2304 -> add_char_closure_1827: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=4}=no))
    184: (   12) subst_2304 -> add_char_closure_1827: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=4}=no))
    185: (   13) subst_2304 -> subst_2304: function obviously too large
    187: (   40) subst_2304 -> add_char_closure_1827: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=2}=no))
    187: (   42) subst_2304 -> add_char_closure_1827: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=19,eval_size=-15,eval_benefit=5,functor=false,branch_depth=2}=no))
    188: (   43) subst_2304: function obviously too large

add_subbytes_closure_1910
     83: (    1) add_substring_closure_1855 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     83: (    5) add_substring_closure_1855 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     85: (    2) add_substring_closure_1855 -> resize_closure_1750: function obviously too large
     85: (    6) add_substring_closure_1855 -> resize_closure_1750: function obviously too large
     86: (    3) add_substring_closure_1855 -> Bytes.blit_string_closure_1866: function obviously too large
     86: (    7) add_substring_closure_1855 -> Bytes.blit_string_closure_1866: function obviously too large
     90: (    0) Bytes.anon-fn_2863: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=0,eval_size=4,eval_benefit=5,functor=false,branch_depth=0}=yes))
     90: (    4) add_substring_closure_1855: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=0}=no))
     90: (    8) add_substring_closure_1855: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=0}=no))

advance_to_non_alpha_closure_2173
    144: (    0) advance_2179 -> advance_2179: did not try copying decl (did not try unrolling)
    146: (    1) advance_2179: did not try copying decl (did not try unrolling)

add_substring_closure_1855
     66: (    2) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     66: (    5) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     71: (    3) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
     71: (    6) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
     83: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     83: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     85: (    4) resize_closure_1750: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=56,eval_size=-52,eval_benefit=5,functor=false,branch_depth=1}=no))
     85: (    7) resize_closure_1750: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=56,eval_size=-52,eval_benefit=5,functor=false,branch_depth=1}=no))
     86: (    9) Bytes.blit_string_closure_1866: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=54,eval_size=-50,eval_benefit=5,functor=false,branch_depth=0}=no))
     86: (   11) Bytes.blit_string_closure_1866: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=54,eval_size=-50,eval_benefit=5,functor=false,branch_depth=0}=no))
     89: (    8) Bytes.blit_string_closure_1866 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     89: (   10) Bytes.blit_string_closure_1866 -> Pervasives.invalid_arg_closure_1911: function obviously too large

advance_to_closing_closure_2111
    134: (    0) advance_2120 -> advance_2120: did not try copying decl (did not try unrolling)
    136: (    1) advance_2120 -> advance_2120: did not try copying decl (did not try unrolling)
    137: (    2) advance_2120 -> advance_2120: did not try copying decl (did not try unrolling)
    138: (    3) advance_2120: did not try copying decl (did not try unrolling)

resize_closure_1750
     66: (    0) Pervasives.failwith_closure_1900: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=2}=no))
     66: (    1) Pervasives.failwith_closure_1900: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=2}=no))
     71: (    3) Bytes.blit_closure_1821: tried but failed (copying body ({benefit={call=1,alloc=0,prim=2,branch=2},orig_size=4,new_size=44,eval_size=-40,eval_benefit=31,functor=false,branch_depth=0}=no))
     71: (    5) Bytes.blit_closure_1821: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=2,branch=2},orig_size=4,new_size=44,eval_size=-40,eval_benefit=31,functor=false,branch_depth=0}=no))
     83: (    2) Bytes.blit_closure_1821 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     83: (    4) Bytes.blit_closure_1821 -> Pervasives.invalid_arg_closure_1911: function obviously too large

add_channel_rec_1988
    107: (    1) Pervasives.input_closure_2423: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=41,eval_size=-37,eval_benefit=5,functor=false,branch_depth=1}=no))
    107: (    3) Pervasives.input_closure_2423: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=41,eval_size=-37,eval_benefit=5,functor=false,branch_depth=1}=no))
    110: (    4) add_channel_rec_1988: did not try copying decl (did not try unrolling)
    388: (    0) Pervasives.input_closure_2423 -> Pervasives.invalid_arg_closure_1911: function obviously too large
    388: (    2) Pervasives.input_closure_2423 -> Pervasives.invalid_arg_closure_1911: function obviously too large

add_string_closure_1926
     66: (    0) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     66: (    3) resize_closure_1750 -> Pervasives.failwith_closure_1900: function obviously too large
     71: (    1) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
     71: (    4) resize_closure_1750 -> Bytes.blit_closure_1821: function obviously too large
     89: (    6) Bytes.blit_string_closure_1866 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     89: (    8) Bytes.blit_string_closure_1866 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     95: (    2) resize_closure_1750: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=56,eval_size=-52,eval_benefit=5,functor=false,branch_depth=1}=no))
     95: (    5) resize_closure_1750: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=56,eval_size=-52,eval_benefit=5,functor=false,branch_depth=1}=no))
     96: (    7) Bytes.blit_string_closure_1866: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=49,eval_size=-45,eval_benefit=18,functor=false,branch_depth=0}=no))
     96: (    9) Bytes.blit_string_closure_1866: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=49,eval_size=-45,eval_benefit=18,functor=false,branch_depth=0}=no))

nth_closure_1690
     47: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     47: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))

sub_closure_1607
     33: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     33: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     34: (    3) Bytes.sub_string_closure_1728: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=1}=yes))
     58: (    4) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     58: (    6) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     65: (    2) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: function obviously too large
     65: (    5) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=1}=no))
     65: (    7) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=48,eval_size=-44,eval_benefit=5,functor=false,branch_depth=1}=no))

output_buffer_closure_2065
    120: (    1) Pervasives.output_closure_2277: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=36,eval_size=-32,eval_benefit=18,functor=false,branch_depth=0}=no))
    120: (    3) Pervasives.output_closure_2277: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=1},orig_size=4,new_size=36,eval_size=-32,eval_benefit=18,functor=false,branch_depth=0}=no))
    339: (    0) Pervasives.output_closure_2277 -> Pervasives.invalid_arg_closure_1911: function obviously too large
    339: (    2) Pervasives.output_closure_2277 -> Pervasives.invalid_arg_closure_1911: function obviously too large

# vim:fdm=expr:filetype=plain:foldexpr=getline(v\:lnum)=~'^\\s*$'&&getline(v\:lnum+1)=~'\\S'?'<1'\:1
