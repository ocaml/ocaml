# This script remove the module aliases from stdlib.ml and stdlib.mli
# so that ocamldep doesn't register dependencies from stdlib to all
# other modules
BEGIN { in_aliases=0 }
NR == 1 { printf ("# 1 \"%s\"\n", FILENAME) }
/^\(\*MODULE_ALIASES\*\)\r?$/ { in_aliases=1 }
!in_aliases { print }
