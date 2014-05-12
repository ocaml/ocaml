#!/bin/bash

set -eu -o pipefail

profile_output="$1"
binary="$2"

decode ()
{
  heap=$1

  temp=$(mktemp)
  temp_cmds=$(mktemp)

  grep $heap $profile_output \
    | awk -F, '{h[$2] += $3; c[$2] += 1}END{for(i in h) print i,h[i],c[i]}' \
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
}

echo MINOR HEAP
decode minor
echo MAJOR HEAP
decode major
