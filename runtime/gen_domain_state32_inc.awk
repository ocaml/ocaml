#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*         KC Sivaramakrishnan, Indian Institute of Technology, Madras    *
#*                                                                        *
#*   Copyright 2019 Indian Institute of Technology, Madras                *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BEGIN{FS="[,)] *";count=0};
/DOMAIN_STATE/{
  print "Store_" $2 " MACRO reg1, reg2";
  print "  mov [reg1+" count "], reg2";
  print "ENDM";
  print "Load_" $2 " MACRO reg1, reg2";
  print "  mov reg2, [reg1+" count "]";
  print  "ENDM";
  print "Push_" $2 " MACRO reg1";
  print "  push [reg1+" count "]";
  print "ENDM";
  print "Pop_" $2 " MACRO reg1";
  print "  pop [reg1+" count "]";
  print "ENDM";
  print "Cmp_" $2 " MACRO reg1, reg2";
  print "  cmp reg2, [reg1+" count "]";
  print "ENDM";
  print "Sub_" $2 " MACRO reg1, reg2";
  print "  sub reg2, [reg1+" count "]";
  print "ENDM";
  count+=8
}
