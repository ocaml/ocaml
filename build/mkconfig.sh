#!/bin/sh
# $Id$

cd `dirname $0`/..

sed -e 's/^\(.*\$([0-9]).*\)$/# \1/' \
    -e 's/\$(\([^)]*\))/${\1}/g' \
    -e 's/^FLEX.*$//g' \
    -e 's/^\([^#=]*\)=\([^"]*\)$/if [ "x$\1" = "x" ]; then \1="\2"; fi/' \
    config/Makefile > config/config.sh


