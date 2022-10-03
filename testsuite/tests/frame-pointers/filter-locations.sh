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
sed_script="/${regex_backtrace}/ {
  /^${program_escaped}/ ! d
  s/${regex_backtrace}/\1/
  s/${regex_trim_fun}/\1/
}"

# Remove potential functions beyond 'caml_program' up  to 'main' if they are
# expected and appear in the correct order
awk_script='/caml_program/ {
  print
  getline
  if (match($0, "^caml_start_program$")) { getline; }
  if (match($0, "^caml_startup_common$")) { getline }
  if (match($0, "^caml_startup_exn$")) { getline }
  if (match($0, "^caml_startup$")) { getline }
  if (match($0, "^caml_main$")) { getline }
  if (match($0, "^main$")) { if (getline == 0) { exit } }
}
{ print }'

# Also remove strange '[0x.....]' entries inserted by some implementation
#   of backtrace_symbols_fd
sed -e "${sed_script}" -e '/^\[0x/d' | awk "${awk_script}"
