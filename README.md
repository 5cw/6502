#  rust 6502 emulator and assembler

This was just a little project to get back into coding more frequently.

**Do not use this project in production code**

## reorganization

tried my hand at reorganizing the 6502 opcodes

[Spreadsheet](https://docs.google.com/spreadsheets/d/e/2PACX-1vRvVaSI80LN8ZKo1b-XaSaV4YMD5VVVtm9c1_yJoy8foDK-HCUhdIizXOdTDTOwh8PQ406uLyA0suNF/pubhtml?gid=657397901&single=true)

first of all, the goals for this project are arbitrary and emotional. I just found myself frustrated with some of the strange decisions that were made for the opcodes, I'm sure there were reasons for all of them, this is for fun.

Goals:

- Create most functionality possible without straying from original design
- Make rules for determining behavior simple
- Make the table look prettier
- Don't worry about leaving room for futureproofing because that feels hard

### Things I did that I like
many properties of an instruction can be discerned purely through bitmath alone.


### note 1
CMP X and CMP Y are omitted because this behavior is covered by CPX A and CPY A. One of these spaces is filled by CXY, or "compare X to Y."
### note 2
TAS, TSA, TYS, and TSY are added as instructions along with TXS and TSX by the following logic:

- if you would be storing a register to itself, transfer it to the stack pointer instead
- if you would be storing the accumulator to another register, transfer the stack pointer to it instead. STA X and STA Y are not needed because this behavior is covered by LDX A and LDY A.
- if you would be loading the accumulator from itself, load it from the stack pointer instead (X and Y cannot load from themselves, that mode is immediate for them)
### note 3
$00-$7F each row is split into two major instructions. $00-$2F they're separated into x1-x7,x8-xE, $30-$7F into x1-x6,x7-xE
### note 4
Just like in the original instruction set, if loading or storing X in an addressing mode which references X, replace X with Y.
