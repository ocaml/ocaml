#!/bin/bash

echo 'let builtin_exceptions = [|'
cat "$1" | tr -d '\r' | \
    sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$|  \1;|p'
echo '|]'

echo 'let builtin_primitives = [|'
sed -e 's/.*/  "&";/' "$2"
echo '|]'

