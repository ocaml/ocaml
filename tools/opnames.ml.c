let names_of_instructions = [|
#define Instruction(name) #name;
#include "instruct.tbl"
#undef Instruction
|]
