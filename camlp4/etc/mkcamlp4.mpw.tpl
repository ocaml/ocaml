#######################################################################
#                                                                     #
#                             Camlp4                                  #
#                                                                     #
#            Damien Doligez, projet Para, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 1999 Institut National de Recherche en Informatique et   #
#  en Automatique.  Distributed only by permission.                   #
#                                                                     #
#######################################################################

# $Id$

set OLIB OLIBDIR
set LIB LIBDIR

set INTERFACES ""
set OPTS ""
set INCL "-I :"

loop
  exit if "{1}" == ""
  if "{1}" == "-I"
    set INCL "{INCL} -I `quote "{2}"`"
    shift
  else if "{1}" =~ /([Â:])¨0([Â:]*)¨1.cmi/
    set first `echo {¨0} | translate a-z A-Z`
    set INTERFACES "{INTERFACES} {first}{¨1}"
  else
    set OPTS "{OPTS} `quote "{1}"`"
  end
  shift
end

set CRC crc_temporary_file

"{LIB}extract_crc" -I "{OLIB}" {INCL} {INTERFACES} > "{CRC}.ml"
echo "let _ = Dynlink.add_available_units crc_unit_list" >> "{CRC}.ml"
ocamlc -I "{LIB}" odyl.cma camlp4.cma "{CRC}.ml" {INCL} {OPTS} ¶
       odyl.cmo -linkall
delete -i "{CRC}.ml" "{CRC}.cmi" "{CRC}.cmo"
