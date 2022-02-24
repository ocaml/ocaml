#!/bin/sh

set -eu

program="${1}"
# https://stackoverflow.com/questions/29613304/is-it-possible-to-escape-regex-metacharacters-reliably-with-sed/29626460#29626460
program_escaped=$(echo ${program} | sed 's/[^^\\]/[&]/g; s/\^/\\^/g; s/\\/\\\\/g')
regex_backtrace='^.*(\(.*\)+0x[[:xdigit:]]*)[0x[[:xdigit:]]*]$'
regex_trim_fun='^\(caml.*\)_[[:digit:]]*$'

# - Ignore backtrace not coming from the program binary
# - Discard the number suffix from OCaml function name
# - Keep the other lines
sed -e \
  "/${regex_backtrace}/ {
    /^${program_escaped}/ ! d
    s/${regex_backtrace}/\1/
    s/${regex_trim_fun}/\1/
  }"
