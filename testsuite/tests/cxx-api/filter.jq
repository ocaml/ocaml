# filter reports
.[] | select(

# only errors and warnings

(.kind != "note")


# Flexible Array Members (FAM) are a C99 feature but not standard C++. It's a
# common extension.
#
# In file included from all-includes.h:50:
# /Users/user/ocaml/trunk/runtime/caml/bigarray.h:86:10: error: ISO C++ forbids flexible array member 'dim' [-Werror=pedantic]
#    86 |   intnat dim[/* num_dims */]; /* Size in each dimension */
#       |          ^~~

and (.message | contains("flexible array member") | not)

)
