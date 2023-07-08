mod assembler;
mod cpu;




fn main() {
    let code = "LDA #$FF
   LDY #$09
   loop:
   STA $1000,Y ; absolute indexed addressing
   DEY
   BPL loop   ";
    let result = assembler::assemble(code.to_string());
    let mut memory = [0xEAu8; 0x10000];
    memory[0xEA00..0xEA0A].copy_from_slice(&result[..]);
    memory[0x500] = 0x4C;
    memory[0x501] = 0x00;
    memory[0xFFFC] = 0x00;
    memory[0xFFFD] = 0x05;
    let mut computer = cpu::CPU6502::new(memory);
    for _ in 0..10 {
        computer.exec_next();
    }
}
