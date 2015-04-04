let opcode_counter = ref 0
#define Instruction(name) \
  let op##name = !opcode_counter ;; \
  incr opcode_counter ;;
#include "instruct.tbl"
#undef Instruction

