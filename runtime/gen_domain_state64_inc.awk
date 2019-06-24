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

BEGIN{FS=",";count=0};
/DOMAIN_STATE/{
  print "Store_" substr($2,2,length($2)-2) " MACRO reg";
  print "  mov [r14+" count "], reg";
  print "ENDM";
  print "Load_" substr($2,2,length($2)-2) " MACRO reg";
  print "  mov reg, [r14+" count "]";
  print  "ENDM";
  print "Push_" substr($2,2, length($2)-2) " MACRO";
  print "  push [r14 + " count "]";
  print "ENDM";
  print "Pop_" substr($2, 2, length($2)-2) " MACRO";
  print "  pop [r14 + " count "]";
  print "ENDM";
  print "Cmp_" substr($2, 2, length($2)-2) " MACRO reg";
  print "  cmp reg, [r14 + " count "]";
  print "ENDM";
  count+=8
}
