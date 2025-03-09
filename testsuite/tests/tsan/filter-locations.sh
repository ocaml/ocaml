#!/bin/sh
set -eu

# - Remove mangling of functions (NOTE: functions of the same name, or
#   anonymous functions, become indistinguishable) and replace it with
#   '<implemspecific>'
# - Replace file+hexadecimal locations with '<implemspecific>'
# - Replace mutex IDs like 'M87' with 'M<implemspecific>'
# - Replace the complete path of the program by '<systemspecific>/' followed by
#   the program filename.
script1='s/pid=[[:digit:]]+/pid=<implemspecific>/
s/tid=[[:digit:]]+/tid=<implemspecific>/

/([Rr]ead|[Ww]rite) of size/ {
  s/of size ([[:digit:]]+) at 0x[[:xdigit:]]+/of size \1 at <implemspecific>/
}

/Mutex M.* created at:/ {
  s/M([0-9]+) \(0x[[:xdigit:]]+\)/M\1 (<implemspecific>)/
}

/#[0-9]+/ {
  s/(#[0-9]+) ([^ ]*) [^ ]*(\(discriminator [0-9]+\))? \(([^ ]*)\)/\1 \2 <implemspecific> (\4)/
  s/(caml[a-zA-Z_0-9]+\$[a-zA-Z_0-9]+)_[[:digit:]]+/\1_<implemspecific>/
  s/\((.+)+0x[[:xdigit:]]+\)/(<implemspecific>)/
  s/ \(BuildId: [[:xdigit:]]+\)//
}

s/ M[0-9]+/ M<implemspecific>/

/SUMMARY/ {
  s/data race \(.*\/.+\+0x[[:xdigit:]]+\) in /data race (<systemspecific>:<implemspecific>) in /
  s/data race .+:.+ in /data race (<systemspecific>:<implemspecific>) in /
  s/(caml[a-zA-Z_0-9]+\$[a-zA-Z_0-9]+)_[[:digit:]]+(\+0x[[:xdigit:]]+)?/\1_<implemspecific>/
}'

# To ignore differences in compiler function inlining, kill backtrace after
# caml_start_program or caml_startup*.
script2='/^[[:space:]]+#[[:digit:]]+ (caml_start_program|caml_startup)/, /^$/ {
  /^$/ !d
}'

# Under FreeBSD, pthread_mutex_init has a leading underscore
script3='s/_pthread_mutex_init/pthread_mutex_init/'

sed -E -e "${script1}" -e "${script2}" -e "${script3}"
