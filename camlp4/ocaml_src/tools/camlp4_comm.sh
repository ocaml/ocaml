#!/bin/sh

if test "`basename $1 .mli`.mli" = "$1"; then
  echo cp $1 `basename $1 .mli`.ppi
  cp $1 `basename $1 .mli`.ppi
else
  echo cp $1 `basename $1 .ml`.ppo
  cp $1 `basename $1 .ml`.ppo
fi
