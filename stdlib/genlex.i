make_lexer_closure_1767
     20: (  279) escape_1882 -> Char.chr_closure_1269 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     20: (  281) escape_1882 -> Char.chr_closure_1269 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     33: (   15) keyword_or_error_closure_1820 -> String.make_closure_1548 -> Bytes.make_closure_1623: function obviously too large
     33: (   17) keyword_or_error_closure_1820 -> String.make_closure_1548 -> Bytes.make_closure_1623: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=21,eval_size=-17,eval_benefit=5,functor=false,branch_depth=0}=no))
     33: (   18) keyword_or_error_closure_1820 -> String.make_closure_1548 -> Bytes.make_closure_1623: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=21,eval_size=-17,eval_benefit=5,functor=false,branch_depth=0}=no))
     33: (   59) next_token_1872 -> keyword_or_error_closure_1820 -> Bytes.make_closure_1623: function obviously too large
     33: (   63) next_token_1872 -> keyword_or_error_closure_1820 -> Bytes.make_closure_1623: function obviously too large
     33: (  296) maybe_comment_1883 -> keyword_or_error_closure_1820 -> Bytes.make_closure_1623: function obviously too large
     33: (  300) maybe_comment_1883 -> keyword_or_error_closure_1820 -> Bytes.make_closure_1623: function obviously too large
     46: (    0) Hashtbl.create_closure_2152: function obviously too large
     47: (    1) anon-fn[genlex.ml:47,12--54]_1784 -> Hashtbl.add_closure_2390: function obviously too large
     47: (    4) List.iter_2073 -> iter_2964: function obviously too large
     47: (   11) List.iter_2073 -> iter_2964: inlined (copying body (unconditionally))
     47: (   13) List.iter_2073: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=22,eval_size=-18,eval_benefit=13,functor=false,branch_depth=0}=no))
     49: (   14) ident_or_keyword_closure_1802 -> Hashtbl.find_closure_2505: function obviously too large
     49: (  106) ident_1873 -> ident_or_keyword_closure_1802 -> Hashtbl.find_closure_2505: function obviously too large
     49: (  108) ident_1873 -> ident_or_keyword_closure_1802 -> Hashtbl.find_closure_2505: function obviously too large
     49: (  123) ident2_1874 -> ident_or_keyword_closure_1802 -> Hashtbl.find_closure_2505: function obviously too large
     49: (  125) ident2_1874 -> ident_or_keyword_closure_1802 -> Hashtbl.find_closure_2505: function obviously too large
     52: (   16) keyword_or_error_closure_1820 -> String.make_closure_1548: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=0}=yes))
     53: (   19) keyword_or_error_closure_1820 -> Hashtbl.find_closure_2505: function obviously too large
     53: (   60) next_token_1872 -> keyword_or_error_closure_1820 -> Hashtbl.find_closure_2505: function obviously too large
     53: (   64) next_token_1872 -> keyword_or_error_closure_1820 -> Hashtbl.find_closure_2505: function obviously too large
     53: (  297) maybe_comment_1883 -> keyword_or_error_closure_1820 -> Hashtbl.find_closure_2505: function obviously too large
     53: (  301) maybe_comment_1883 -> keyword_or_error_closure_1820 -> Hashtbl.find_closure_2505: function obviously too large
     54: (   20) keyword_or_error_closure_1820 -> Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=40,eval_size=-36,eval_benefit=8,functor=false,branch_depth=2}=no))
     54: (   21) keyword_or_error_closure_1820 -> Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=40,eval_size=-36,eval_benefit=8,functor=false,branch_depth=2}=no))
     54: (   61) next_token_1872 -> keyword_or_error_closure_1820 -> Pervasives.^_closure_2002: function obviously too large
     54: (   65) next_token_1872 -> keyword_or_error_closure_1820 -> Pervasives.^_closure_2002: function obviously too large
     54: (  298) maybe_comment_1883 -> keyword_or_error_closure_1820 -> Pervasives.^_closure_2002: function obviously too large
     54: (  302) maybe_comment_1883 -> keyword_or_error_closure_1820 -> Pervasives.^_closure_2002: function obviously too large
     57: (   23) next_token_1872 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
     59: (   68) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     59: (   70) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     59: (   71) next_token_1872 -> next_token_1872: function obviously too large
     61: (   73) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     61: (   75) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     62: (   76) next_token_1872 -> reset_buffer_closure_1684: inlined (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=14,functor=false,branch_depth=2}=yes))
     62: (   77) next_token_1872 -> store_closure_1698: function obviously too large
     62: (   78) next_token_1872 -> ident_1873: function obviously too large
     65: (  102) ident_1873 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  104) ident_1873 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  119) ident2_1874 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  121) ident2_1874 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  162) number_1876 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  164) number_1876 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  181) decimal_part_1877 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  183) decimal_part_1877 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  204) end_exponent_part_1879 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  206) end_exponent_part_1879 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  228) string_1880 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     65: (  230) string_1880 -> get_string_closure_1745 -> Bytes.sub_closure_1692: function obviously too large
     66: (   80) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     66: (   82) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     67: (   83) next_token_1872 -> reset_buffer_closure_1684: inlined (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=14,functor=false,branch_depth=2}=yes))
     67: (   84) next_token_1872 -> store_closure_1698: function obviously too large
     67: (   85) next_token_1872 -> ident2_1874: function obviously too large
     69: (   87) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     69: (   89) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     70: (   90) next_token_1872 -> reset_buffer_closure_1684: inlined (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=14,functor=false,branch_depth=2}=yes))
     70: (   91) next_token_1872 -> store_closure_1698: function obviously too large
     70: (   92) next_token_1872 -> number_1876: function obviously too large
     72: (   36) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     72: (   38) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     73: (    5) List.iter_2073 -> iter_2964 -> iter_2979: function obviously too large
     73: (    7) List.iter_2073 -> iter_2979 -> iter_2964 -> iter_2979: function obviously too large
     73: (    9) List.iter_2073 -> iter_2979 -> iter_2964 -> iter_2979: function obviously too large
     73: (   10) List.iter_2073 -> iter_2964 -> iter_2979: function obviously too large
     73: (   12) List.iter_2073 -> iter_2964 -> iter_2979: function obviously too large
     74: (   39) next_token_1872 -> char_1881: function obviously too large
     75: (    2) List.iter_2073 -> iter_2964 -> anon-fn[genlex.ml:47,12--54]_1784: function obviously too large
     75: (    3) List.iter_2073 -> iter_2964 -> iter_2964: function obviously too large
     75: (    6) List.iter_2073 -> iter_2979 -> anon-fn[genlex.ml:47,12--54]_1784: function obviously too large
     75: (    8) List.iter_2073 -> iter_2979 -> iter_2964: inlined (copying body (unconditionally))
     77: (   41) next_token_1872 -> Stream.peek_closure_1838: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     77: (   43) next_token_1872 -> Stream.peek_closure_1838: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     78: (   45) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=7}=no))
     78: (   47) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=7}=no))
     82: (   49) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     82: (   51) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     83: (   52) next_token_1872 -> reset_buffer_closure_1684: tried but failed (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=14,functor=false,branch_depth=5}=no))
     83: (   53) next_token_1872 -> reset_buffer_closure_1684: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=14,functor=false,branch_depth=5}=no))
     83: (   54) next_token_1872 -> string_1880: function obviously too large
     84: (   26) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     84: (   28) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     84: (   29) next_token_1872 -> neg_number_1875: function obviously too large
     85: (   31) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     85: (   33) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
     85: (   34) next_token_1872 -> maybe_comment_1883: function obviously too large
     86: (   56) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     86: (   58) next_token_1872 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     86: (   62) next_token_1872 -> keyword_or_error_closure_1820: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=42,eval_size=-38,eval_benefit=5,functor=false,branch_depth=2}=no))
     86: (   66) next_token_1872 -> keyword_or_error_closure_1820: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=42,eval_size=-38,eval_benefit=5,functor=false,branch_depth=2}=no))
     89: (   94) ident_1873 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
     92: (   97) ident_1873 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     92: (   99) ident_1873 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     92: (  100) ident_1873 -> store_closure_1698: function obviously too large
     92: (  101) ident_1873 -> ident_1873: function obviously too large
     93: (   22) next_token_1872 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (   24) next_token_1872 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (   40) next_token_1872 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (   42) next_token_1872 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (   93) ident_1873 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (   95) ident_1873 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  103) ident_1873 -> get_string_closure_1745: tried but failed (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
     93: (  105) ident_1873 -> get_string_closure_1745: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
     93: (  107) ident_1873 -> ident_or_keyword_closure_1802: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=27,eval_size=-23,eval_benefit=5,functor=false,branch_depth=1}=no))
     93: (  109) ident_1873 -> ident_or_keyword_closure_1802: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=27,eval_size=-23,eval_benefit=5,functor=false,branch_depth=1}=no))
     93: (  110) ident2_1874 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  112) ident2_1874 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  127) neg_number_1875 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  129) neg_number_1875 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  141) number_1876 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  143) number_1876 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  166) decimal_part_1877 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  168) decimal_part_1877 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  185) exponent_part_1878 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  187) exponent_part_1878 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  195) end_exponent_part_1879 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  197) end_exponent_part_1879 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  208) string_1880 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  210) string_1880 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  232) char_1881 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  234) char_1881 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  244) escape_1882 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  246) escape_1882 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  263) escape_1882 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  265) escape_1882 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  271) escape_1882 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  273) escape_1882 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  287) maybe_comment_1883 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  289) maybe_comment_1883 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  304) comment_1884 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  306) comment_1884 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  322) maybe_nested_comment_1885 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  324) maybe_nested_comment_1885 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  336) maybe_end_comment_1886 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     93: (  338) maybe_end_comment_1886 -> Stream.peek_closure_1838 -> Stream.peek_data_1741: function obviously too large
     95: (  111) ident2_1874 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
     99: (  114) ident2_1874 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     99: (  116) ident2_1874 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
     99: (  117) ident2_1874 -> store_closure_1698: function obviously too large
     99: (  118) ident2_1874 -> ident2_1874: function obviously too large
    100: (  120) ident2_1874 -> get_string_closure_1745: tried but failed (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    100: (  122) ident2_1874 -> get_string_closure_1745: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    100: (  124) ident2_1874 -> ident_or_keyword_closure_1802: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=27,eval_size=-23,eval_benefit=5,functor=false,branch_depth=1}=no))
    100: (  126) ident2_1874 -> ident_or_keyword_closure_1802: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=27,eval_size=-23,eval_benefit=5,functor=false,branch_depth=1}=no))
    102: (  128) neg_number_1875 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    104: (  131) neg_number_1875 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    104: (  133) neg_number_1875 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    105: (  134) neg_number_1875 -> reset_buffer_closure_1684: inlined (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=14,functor=false,branch_depth=2}=yes))
    105: (  135) neg_number_1875 -> store_closure_1698: function obviously too large
    105: (  136) neg_number_1875 -> store_closure_1698: function obviously too large
    105: (  137) neg_number_1875 -> number_1876: function obviously too large
    106: (  138) neg_number_1875 -> reset_buffer_closure_1684: inlined (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=14,functor=false,branch_depth=1}=yes))
    106: (  139) neg_number_1875 -> store_closure_1698: function obviously too large
    106: (  140) neg_number_1875 -> ident2_1874: function obviously too large
    108: (  142) number_1876 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    109: (   25) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   27) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   30) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   32) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   35) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   37) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   44) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   46) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   48) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   50) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   55) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   57) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   67) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   69) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   72) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   74) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   79) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   81) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   86) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   88) next_token_1872 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   96) ident_1873 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (   98) ident_1873 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  113) ident2_1874 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  115) ident2_1874 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  130) neg_number_1875 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  132) neg_number_1875 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  144) number_1876 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  146) number_1876 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  150) number_1876 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  152) number_1876 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  156) number_1876 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  158) number_1876 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  169) decimal_part_1877 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  171) decimal_part_1877 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  175) decimal_part_1877 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  177) decimal_part_1877 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  188) exponent_part_1878 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  190) exponent_part_1878 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  198) end_exponent_part_1879 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  200) end_exponent_part_1879 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  211) string_1880 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  213) string_1880 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  217) string_1880 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  219) string_1880 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  224) string_1880 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  226) string_1880 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  235) char_1881 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  237) char_1881 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  239) char_1881 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  241) char_1881 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  247) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  249) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  251) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  253) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  255) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  257) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  259) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  261) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  267) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  269) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  275) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  277) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  283) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  285) escape_1882 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  290) maybe_comment_1883 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  292) maybe_comment_1883 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  307) comment_1884 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  309) comment_1884 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  312) comment_1884 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  314) comment_1884 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  317) comment_1884 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  319) comment_1884 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  325) maybe_nested_comment_1885 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  327) maybe_nested_comment_1885 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  330) maybe_nested_comment_1885 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  332) maybe_nested_comment_1885 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  339) maybe_end_comment_1886 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  341) maybe_end_comment_1886 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  344) maybe_end_comment_1886 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  346) maybe_end_comment_1886 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  349) maybe_end_comment_1886 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    109: (  351) maybe_end_comment_1886 -> Stream.junk_closure_1899 -> Stream.junk_data_1850: function obviously too large
    110: (  145) number_1876 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    110: (  147) number_1876 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    110: (  148) number_1876 -> store_closure_1698: function obviously too large
    110: (  149) number_1876 -> number_1876: function obviously too large
    112: (  151) number_1876 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    112: (  153) number_1876 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    112: (  154) number_1876 -> store_closure_1698: function obviously too large
    112: (  155) number_1876 -> decimal_part_1877: function obviously too large
    114: (  157) number_1876 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    114: (  159) number_1876 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    114: (  160) number_1876 -> store_closure_1698: function obviously too large
    114: (  161) number_1876 -> exponent_part_1878: function obviously too large
    115: (  163) number_1876 -> get_string_closure_1745: tried but failed (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    115: (  165) number_1876 -> get_string_closure_1745: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    117: (  167) decimal_part_1877 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    119: (  170) decimal_part_1877 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    119: (  172) decimal_part_1877 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    119: (  173) decimal_part_1877 -> store_closure_1698: function obviously too large
    119: (  174) decimal_part_1877 -> decimal_part_1877: function obviously too large
    121: (  176) decimal_part_1877 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    121: (  178) decimal_part_1877 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    121: (  179) decimal_part_1877 -> store_closure_1698: function obviously too large
    121: (  180) decimal_part_1877 -> exponent_part_1878: function obviously too large
    122: (  182) decimal_part_1877 -> get_string_closure_1745: tried but failed (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    122: (  184) decimal_part_1877 -> get_string_closure_1745: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    124: (  186) exponent_part_1878 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    126: (  189) exponent_part_1878 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    126: (  191) exponent_part_1878 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    126: (  192) exponent_part_1878 -> store_closure_1698: function obviously too large
    126: (  193) exponent_part_1878 -> end_exponent_part_1879: function obviously too large
    127: (  194) exponent_part_1878 -> end_exponent_part_1879: function obviously too large
    129: (  196) end_exponent_part_1879 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    131: (  199) end_exponent_part_1879 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    131: (  201) end_exponent_part_1879 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    131: (  202) end_exponent_part_1879 -> store_closure_1698: function obviously too large
    131: (  203) end_exponent_part_1879 -> end_exponent_part_1879: function obviously too large
    132: (  205) end_exponent_part_1879 -> get_string_closure_1745: tried but failed (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    132: (  207) end_exponent_part_1879 -> get_string_closure_1745: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=1}=no))
    134: (  209) string_1880 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    135: (  225) string_1880 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    135: (  227) string_1880 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    135: (  229) string_1880 -> get_string_closure_1745: tried but failed (copying body ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=2}=no))
    135: (  231) string_1880 -> get_string_closure_1745: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=3,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=14,functor=false,branch_depth=2}=no))
    137: (  218) string_1880 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    137: (  220) string_1880 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    139: (  221) string_1880 -> escape_1882: function obviously too large
    142: (  222) string_1880 -> store_closure_1698: function obviously too large
    142: (  223) string_1880 -> string_1880: function obviously too large
    143: (  212) string_1880 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    143: (  214) string_1880 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    143: (  215) string_1880 -> store_closure_1698: function obviously too large
    143: (  216) string_1880 -> string_1880: function obviously too large
    146: (  233) char_1881 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    148: (  240) char_1881 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    148: (  242) char_1881 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    149: (  243) char_1881 -> escape_1882: function obviously too large
    152: (  236) char_1881 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    152: (  238) char_1881 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    155: (  245) escape_1882 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    156: (  256) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    156: (  258) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    157: (  252) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    157: (  254) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    158: (  248) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    158: (  250) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=4}=no))
    160: (  260) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    160: (  262) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    161: (  264) escape_1882 -> Stream.peek_closure_1838: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    161: (  266) escape_1882 -> Stream.peek_closure_1838: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    163: (  268) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
    163: (  270) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
    164: (  272) escape_1882 -> Stream.peek_closure_1838: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
    164: (  274) escape_1882 -> Stream.peek_closure_1838: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=5}=no))
    166: (  276) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=7}=no))
    166: (  278) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=7}=no))
    167: (  280) escape_1882 -> Char.chr_closure_1269: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=7}=no))
    167: (  282) escape_1882 -> Char.chr_closure_1269: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=7}=no))
    174: (  284) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    174: (  286) escape_1882 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    177: (  288) maybe_comment_1883 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    179: (  291) maybe_comment_1883 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    179: (  293) maybe_comment_1883 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    179: (  294) maybe_comment_1883 -> comment_1884: function obviously too large
    179: (  295) maybe_comment_1883 -> next_token_1872: function obviously too large
    180: (  299) maybe_comment_1883 -> keyword_or_error_closure_1820: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=43,eval_size=-39,eval_benefit=5,functor=false,branch_depth=1}=no))
    180: (  303) maybe_comment_1883 -> keyword_or_error_closure_1820: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=43,eval_size=-39,eval_benefit=5,functor=false,branch_depth=1}=no))
    182: (  305) comment_1884 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    183: (  313) comment_1884 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    183: (  315) comment_1884 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    183: (  316) comment_1884 -> maybe_nested_comment_1885: function obviously too large
    184: (  308) comment_1884 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    184: (  310) comment_1884 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    184: (  311) comment_1884 -> maybe_end_comment_1886: function obviously too large
    185: (  318) comment_1884 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    185: (  320) comment_1884 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    185: (  321) comment_1884 -> comment_1884: function obviously too large
    188: (  323) maybe_nested_comment_1885 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    189: (  331) maybe_nested_comment_1885 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    189: (  333) maybe_nested_comment_1885 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    189: (  334) maybe_nested_comment_1885 -> comment_1884: function obviously too large
    189: (  335) maybe_nested_comment_1885 -> comment_1884: function obviously too large
    190: (  326) maybe_nested_comment_1885 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    190: (  328) maybe_nested_comment_1885 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    190: (  329) maybe_nested_comment_1885 -> comment_1884: function obviously too large
    193: (  337) maybe_end_comment_1886 -> Stream.peek_closure_1838: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=0}=yes))
    194: (  350) maybe_end_comment_1886 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    194: (  352) maybe_end_comment_1886 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=2}=no))
    195: (  345) maybe_end_comment_1886 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    195: (  347) maybe_end_comment_1886 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    195: (  348) maybe_end_comment_1886 -> maybe_end_comment_1886: function obviously too large
    196: (  340) maybe_end_comment_1886 -> Stream.junk_closure_1899: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    196: (  342) maybe_end_comment_1886 -> Stream.junk_closure_1899: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=8,eval_size=-4,eval_benefit=5,functor=false,branch_depth=3}=no))
    196: (  343) maybe_end_comment_1886 -> comment_1884: function obviously too large
    199: (  353) anon-fn[genlex.ml:199,2--58]_2788 -> anon-fn[genlex.ml:199,27--58]_2793 -> next_token_1872: function obviously too large
    199: (  354) anon-fn[genlex.ml:199,2--58]_2788 -> Stream.from_closure_2062: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=28,eval_size=-24,eval_benefit=5,functor=false,branch_depth=0}=no))
    199: (  355) anon-fn[genlex.ml:199,2--58]_2788 -> Stream.from_closure_2062: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=28,eval_size=-24,eval_benefit=5,functor=false,branch_depth=0}=no))

store_closure_1698
     34: (    1) Bytes.blit_closure_1821: tried but failed (copying body ({benefit={call=1,alloc=0,prim=2,branch=2},orig_size=4,new_size=44,eval_size=-40,eval_benefit=31,functor=false,branch_depth=1}=no))
     34: (    3) Bytes.blit_closure_1821: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=2,branch=2},orig_size=4,new_size=44,eval_size=-40,eval_benefit=31,functor=false,branch_depth=1}=no))
     83: (    0) Bytes.blit_closure_1821 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     83: (    2) Bytes.blit_closure_1821 -> Pervasives.invalid_arg_closure_1911: function obviously too large

get_string_closure_1745
     41: (    1) Bytes.sub_string_closure_1728: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=0}=yes))
     65: (    0) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: function obviously too large
     65: (    2) Bytes.sub_string_closure_1728 -> Bytes.sub_closure_1692: function obviously too large

# vim:fdm=expr:filetype=plain:foldexpr=getline(v\:lnum)=~'^\\s*$'&&getline(v\:lnum+1)=~'\\S'?'<1'\:1
