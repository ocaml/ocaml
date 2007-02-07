#!/bin/sh

cd `dirname $0`/..

sed \
    -e 's/^#ml \(.*\)/\1/' \
    -e 's/\\"/"/g' \
    -e 's/^\(#.*\)$/(* \1 *)/' \
    -e 's/^\(.*\$([0-9]).*\)$/(* \1 *)/' \
    -e 's/^\([^(=]*\)=\([^"]*\)$/let <lower>\1<\/lower> = "\2";;/' \
    -e 's/\$(\([^)]*\))/"\^<lower>\1<\/lower>\^"/g' \
    -e 's/""\^//g' \
    -e 's/\^""//g' \
    -e 's/^let <lower>\(MAKE\|DO\).*$//g' \
    -e 's/"true"/true/g' \
    -e 's/"false"/false/g' \
    config/Makefile > myocamlbuild_config.tmp.ml

#
# Some gory awk script to workaround the \L sed feature
# that is only part of gnused.
#
awk '{
    x = $0 ;
    while (x ~ /<lower>/) {
      i = index(x, "<lower>");
      j = index(x, "</lower>");
      xi = substr(x, 0, i - 1);
      i2 = i + length("<lower>");
      xj = substr(x, i2, j - i2);
      k = j + length("</lower>");
      xk = substr(x, k, length(x) - k + 1);
      x = sprintf("%s%s%s", xi, tolower(xj), xk)
    };
    printf("%s\n", x) }' \
 < myocamlbuild_config.tmp.ml > myocamlbuild_config.ml

rm -f myocamlbuild_config.tmp.ml

