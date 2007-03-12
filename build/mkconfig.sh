#!/bin/sh
# $Id$

cd `dirname $0`/..

sed -e 's/\$(\([^)]*\))/${\1}/g' \
    -e 's/^\([^=]*\)=\([^"]*\)$/\1="\2"/' \
    -e 's/^\(.*\$([0-9]).*\)$/# \1/' \
    config/Makefile > config/config.sh
