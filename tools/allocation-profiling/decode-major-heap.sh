#!/bin/bash

set -eu -o pipefail

profile_output="$1"
binary="$2"

temp=$(mktemp)
temp_cmds=$(mktemp)

cat $profile_output | grep -v "^blocks unaccounted for" \
  | awk '{h[$1] += $2; c[$1] += 1}END{for(i in h) print i,h[i],c[i]}' \
  > $temp

cat $temp | awk '{print $1}' | sed 's/^/inf line */' > $temp_cmds

paste <(cat $temp | awk '{print $2" "$3}') \
  <(gdb -x $temp_cmds --batch $binary) \
  | sed 's/__/./g' \
  | sed 's/<caml\([A-Z]\)/<\1/g' \
  | sed 's/ starts at address /: /' \
  | sed 's/and ends at/->/' \
  | sort -n

rm -f $temp $temp_cmds
