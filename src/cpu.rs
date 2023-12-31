use std::sync::atomic::Ordering::Acquire;

const DEBUG: bool = true;

const FLAG_C: u8 = 1;      //carry
const FLAG_Z: u8 = 1 << 1; //zero
const FLAG_I: u8 = 1 << 2; //interrupt disable
const FLAG_D: u8 = 1 << 3; //decimal
const FLAG_B: u8 = 1 << 4; //break
const FLAG_V: u8 = 1 << 6; //overflow
const FLAG_N: u8 = 1 << 7; //negative

enum AdrMode {
    Address(u16),
    Accumulator,
    X,
    Y,
    S,
    Stack,
    Immediate,
    Relative,
    Implied
}


pub(crate) struct CPU6502 {
    prog_counter: u16,
    stack_ptr: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    proc_flags: u8,
    memory: [u8; 0x10000],
}

const INSTRUCTION_NAMES: [&str; 255] =
    ["BRK b","ORA (d,X)","cop b","ora d,S","Tsb d","ORA d","ASL d","ora [d]","PHP","ORA #","ASL A","phd","Tsb a","ORA a","ASL a","ora al",
    "BPL r","ORA (d),Y","Ora (d)","ora (d,S),Y","Trb d","ORA d,X","ASL d,X","ora [d],Y","CLC","ORA a,Y","Inc A","tcs","Trb a","ORA a,X","ASL a,X","ora al,X",
    "JSR a","AND (d,X)","jsl al","and d,S","BIT d","AND d","ROL d","and [d]","PLP","AND #","ROL A","pld","BIT a","AND a","ROL a","and al",
    "BMI r","AND (d),Y","And (d)","and (d,S),Y","Bit d,X","AND d,X","ROL d,X","and [d],Y","SEC","AND a,Y","Dec A","tsc","Bit a,X","AND a,X","ROL a,X","and al,X",
    "RTI","EOR (d,X)","wdm","eor d,S","mvp s,d","EOR d","LSR d","eor [d]","PHA","EOR #","LSR A","phk","JMP a","EOR a","LSR a","eor al",
    "BVC r","EOR (d),Y","Eor (d)","eor (d,S),Y","mvn s,d","EOR d,X","LSR d,X","eor [d],Y","CLI","EOR a,Y","Phy","tcd","jmp al","EOR a,X","LSR a,X","eor al,X",
    "RTS","ADC (d,X)","per rl","adc d,S","Stz d","ADC d","ROR d","adc [d]","PLA","ADC #","ROR A","rtl","JMP (a)","ADC a","ROR a","adc al",
    "BVS r","ADC (d),Y","Adc (d)","adc (d,S),Y","Stz d,X","ADC d,X","ROR d,X","adc [d],Y","SEI","ADC a,Y","Ply","tdc","Jmp (a,X)","ADC a,X","ROR a,X","adc al,X",
    "Bra r","STA (d,X)","brl rl","sta d,S","STY d","STA d","STX d","sta [d]","DEY","Bit #","TXA","phb","STY a","STA a","STX a","sta al",
    "BCC r","STA (d),Y","Sta (d)","sta (d,S),Y","STY d,X","STA d,X","STX d,Y","sta [d],Y","TYA","STA a,Y","TXS","txy","Stz a","STA a,X","Stz a,X","sta al,X",
    "LDY #","LDA (d,X)","LDX #","lda d,S","LDY d","LDA d","LDX d","lda [d]","TAY","LDA #","TAX","plb","LDY a","LDA a","LDX a","lda al",
    "BCS r","LDA (d),Y","Lda (d)","lda (d,S),Y","LDY d,X","LDA d,X","LDX d,Y","lda [d],Y","CLV","LDA a,Y","TSX","tyx","LDY a,X","LDA a,X","LDX a,Y","lda al,X",
    "CPY #","CMP (d,X)","rep #","cmp d,S","CPY d","CMP d","DEC d","cmp [d]","INY","CMP #","DEX","wai","CPY a","CMP a","DEC a","cmp al",
    "BNE r","CMP (d),Y","Cmp (d)","cmp (d,S),Y","pei d","CMP d,X","DEC d,X","cmp [d],Y","CLD","CMP a,Y","Phx","stp","jml (a)","CMP a,X","DEC a,X","cmp al,X",
    "CPX #","SBC (d,X)","sep #","sbc d,S","CPX d","SBC d","INC d","sbc [d]","INX","SBC #","NOP","xba","CPX a","SBC a","INC a","sbc al",
    "BEQ r ,SBC (d),Y","Sbc (d)","sbc (d,S),Y","pea a","SBC d,X","INC d,X","sbc [d],Y","SED","SBC a,Y","Plx","xce","jsr (a,X)","SBC a,X","INC a,X","sbc al,X"];


impl CPU6502 {
    pub fn new(mem: [u8; 0x10000]) -> Self {
        let prog_counter = mem[0xFFFC] as u16 + ((mem[0xFFFD] as u16) << 8);
        Self { prog_counter, stack_ptr: 0xFF, accumulator: 0, x: 0, y: 0, proc_flags: FLAG_I, memory: mem }
    }

    pub fn exec_next(&mut self) {
        let instruction = self.fetch_instruction();

        let mode = (instruction & 0b11100) >> 2;
        let group = instruction & 0b11;
        let code = (instruction & 0b11100000) >> 5;
        let instr_size = match mode {
            0b011 | 0b110 | 0b111 => 3,
            _ => 2
        };
        if DEBUG {
            let instr_size  = match instruction {
                0x4C | 0x6C => 3,
                _ => instr_size
            };
            let i1 = self.fetch_byte(self.prog_counter + 1);
            let i2 = self.fetch_byte(self.prog_counter + 2);
            let b = if instruction & 0x0F == 0x08 || instruction & 0x8F == 0x8A {
                "".to_string()
            } else if instr_size > 2 {
                format!("{i1:02X} {i2:02X}")
            } else {
                format!("{i1:02X}")
            };
            let name = INSTRUCTION_NAMES[instruction as usize];
            let count = self.prog_counter;
            let (x,y,a,sp) = (self.x, self.y, self.accumulator, self.stack_ptr);
            println!("x={x:02X},y={y:02X},acc={a:02X},sp={sp:02X}");
            println!("{count:04X}:{instruction:02X} {b}: executing instruction {name} {b}");

        }
        //irregular instructions
        let mut irregular = true;
        let result = match instruction {
            0xCA | 0xE8 => { //DEX / INX decrement/increment x
                self.x = if instruction == 0xCA {
                    self.x.wrapping_sub(1)
                } else {
                    self.x.wrapping_add(1)
                };
                self.x
            },
            0x88 | 0xC8 => { //DEY / INY decrement/increment y
                self.y = if instruction == 0x88 {
                    self.y.wrapping_sub(1)
                } else {
                    self.y.wrapping_add(1)
                };
                self.y
            },
            0x8A => { //TXA transfer x to accumulator
                self.accumulator = self.x;
                self.x
            },
            0x9A => { //TXS transfer x to stack pointer
                self.stack_ptr = self.x;
                self.x
            },
            0xAA => { //TAX transfer accumulator to x
                self.x = self.accumulator;
                self.accumulator
            },
            0xA8 => { //TAY transfer accumulator to y
                self.y = self.accumulator;
                self.accumulator
            },
            0x98 => { //TYA transfer y to accumulator
                self.accumulator = self.y;
                self.y
            },
            0xBA => { //TSX transfer stack pointer to x
                self.x = self.stack_ptr;
                self.stack_ptr
            },
            0xEA => 0, //NOP no operation
            _ => {
                irregular = false;
                0
            }
        };

        if irregular {
            if instruction != 0xEA {self.set_zn(result)} //NOP does not set zero/negative flags
            self.prog_counter += 1;
            return
        }

        if instruction & 0b10011111 == 0 { //interrupts, calls, returns
            match code {
                0b00 => { //BRK forcing interrupt
                    self.push_short(self.prog_counter);
                    self.push_byte(self.proc_flags);
                    self.proc_flags |= FLAG_B;
                    self.prog_counter = self.fetch_short(0xFFFE);
                },
                0b10 => { //RTI return from interrupt
                    self.proc_flags = self.pop_byte();
                    self.prog_counter = self.pop_short() + 1;
                },
                0b01 => { //JSR jump to subroutine
                    self.push_short(self.prog_counter + 2);
                    self.prog_counter = self.fetch_short(self.prog_counter + 1);
                },
                0b11 => { //RTS return from subroutine
                    self.prog_counter = self.pop_short() + 1;
                }
                _ => {}
            }
            return
        }

        let mut a_mode = false; //accumulator mode is unique to a few instructions so it is handled specially

        /*determine address based on mode and group, also execute
        pushes/pops, branches, and sets/clears because they
        are grouped differently to other instructions*/
        let address = match (mode, group) {
            (0b000, 0b01) => Some(self.fetch_byte( //(zero page + x) 2 bytes
                self.fetch_byte(self.prog_counter + 1)
                    .wrapping_add(self.x) as u16)
                as u16),
            (0b000, _) => None, //immediate 2 bytes
            (0b001, _) => Some(self.fetch_byte(self.prog_counter + 1) as u16), //zero page 2 bytes
            (0b011, _) => Some(self.fetch_short(self.prog_counter + 1)), //absolute 3 bytes
            (0b010, 0b00) => { //pushes and pops from the stack
                match code {
                    0b00 => { //PHP push process flags
                        self.push_byte(self.proc_flags);
                    },
                    0b10 => { //PLP pull process flags
                        self.proc_flags = self.pop_byte();
                    },
                    0b01 => { //PHA push accumulator
                        self.push_byte(self.accumulator);
                    },
                    0b11 => { //PLA pull accumulator
                        self.accumulator = self.pop_byte();
                        self.set_zn(self.accumulator);
                    }
                    _ => {}
                }
                self.prog_counter += 1;
                return
            }
            (0b010, 0b10) => {
                a_mode = true;
                None
            },
            (0b010, 0b01) => None, //immediate 2 bytes
            (0b100, 0b00) => {
                //branch if flag bit is equal to third bit of code
                if ((self.proc_flags & match code { //branch
                    0b000 | 0b001 => FLAG_N, //BPL / BMI if plus/minus
                    0b010 | 0b011 => FLAG_V, //BVC / BVS if overflow clear/set
                    0b100 | 0b101 => FLAG_C, //BCC / BCS if carry clear/set
                    0b110 | 0b111 => FLAG_Z, //BNE / BEQ if not equal/equal
                    _ => 0
                }) > 0) == (code & 1 > 0) {
                    let offset = self.fetch_byte(self.prog_counter + 1) as u16;
                    self.prog_counter += offset;
                    if offset > 0x7F {self.prog_counter -= 0x100}
                } else {
                    self.prog_counter += 2;
                }
                return
            }
            (0b100, 0b01) => Some(self.fetch_byte( // (zero page) + y 2 bytes
                self.fetch_byte(self.prog_counter + 1) as u16) as u16
                + self.y as u16),
            (0b101, _) => {
                let offset = match (group, code) {
                    (0b10, 0b100 | 0b101) => self.y,
                    _ => self.x
                } as u16;
                Some(self.fetch_byte(self.prog_counter + 1) as u16 + offset)
            }, //zero page + x 2 bytes
            (0b110, 0b00) => { //clears and sets
                let mask = match code & 0b110 {
                    0b000 | 0b001 => FLAG_C, //CLC / SEC carry flag
                    0b010 | 0b011 => FLAG_I, //CLI / SEI interrupt flag
                    // (can only set with operation which overflows)
                    // 0b100 taken by TYA
                            0b101 => FLAG_V,       //CLV clear overflow flag
                    0b110 | 0b111 => FLAG_D, //CLD / SED decimal flag
                    _ => 0
                };
                self.proc_flags &= !mask;
                // set the bit if third bit of code is set and it isn't clear overflow
                self.proc_flags |= mask * (code & 1) * (code != 0b101) as u8;
                self.prog_counter += 1;
                return
            }
            (0b110 | 0b111, _) => {
                let offset = match (mode, group, code) {
                    (0b110, 0b01, _) | (_, 0b10, 0b101) => self.y,
                    _ => self.x
                } as u16;
                Some(self.fetch_short(self.prog_counter + 1) + offset)
            },//absolute + x/y 3 bytes
            _ => {
                println!("invalid addressing mode for instruction {instruction:x}");
                None
            } //invalid or immediate
        };

        let val = if let Some(address) = address {
            self.fetch_byte(address)
        } else if a_mode {
            self.accumulator
        } else {
            self.fetch_byte(self.prog_counter + 1)
        };

        self.prog_counter += if a_mode {1} else {instr_size};

        match group {
            0b01 => {
                match code { //group 1
                    0b000 => //ORA bitwise or
                        self.accumulator |= val,
                    0b001 => //AND bitwise and
                        self.accumulator &= val,
                    0b010 => //EOR bitwise exclusive or
                        self.accumulator ^= val,
                    0b011 => //ADC add with carry
                        self.add(val),
                    0b100 => //STA store accumulator to memory
                        self.store_byte(address, self.accumulator),
                    0b101 => //LDA load accumulator from memory
                        self.accumulator = val,
                    0b110 => //CMP compare
                        self.compare(self.accumulator, val),
                    0b111 => //SBC sub with carry
                        self.sub(val),
                    _ => {} //All cases covered
                }
                match code {
                    0b100 | 0b110 => {} //STA does not need to set zn, CMP sets it differently
                    _ => self.set_zn(self.accumulator)
                }
            }
            0b10 => match code { // group 2
                0b000 | 0b001 | 0b010 | 0b011 => {
                    // ASL, ROL, LSR, ROR
                    // shift left, rotate left, shift right, rotate right
                    let carry = self.proc_flags & FLAG_C & code;
                    self.proc_flags &= !FLAG_C;
                    let (val, overflow) = if code < 0b010 {
                        ((val << 1) | carry, (0 < (0b10000000 & val)) as u8)
                    } else {
                        ((val >> 1) | (carry * 0b10000000), (1 & val))
                    };
                    self.proc_flags |= overflow;
                    self.set_zn(val);
                    if a_mode {
                        self.accumulator = val;
                    } else {
                        self.store_byte(address, val)
                    }
                },
                0b100 => self.store_byte(address, self.x), //STX store x to memory
                0b101 => { //LDX load x from memory
                    self.x = val;
                    self.set_zn(val);
                },
                0b110 | 0b111 => { //DEC/INC decrement/increment memory
                    let val = if code & 1 == 0 {
                        val.wrapping_sub(1)
                    } else {
                        val.wrapping_add(1)
                    };
                    self.store_byte(address, val);
                    self.set_zn(val);

                }
                _ => {} // All cases covered
            }
            0b00 => match code { //group 3
                0b001 => //BIT bit test
                    {
                        self.proc_flags &= !(FLAG_Z | FLAG_V | FLAG_N); //clear Z, V, N
                        if val & self.accumulator < 1 {
                            self.proc_flags |= FLAG_Z
                        }
                        self.proc_flags |= val & 0b11000000;
                    }
                0b010 | 0b011 => //JMP jump absolute / indirect
                    if let Some(address) = address {
                        self.prog_counter = if code & 1 == 0 {
                            address
                        } else {
                            ((self.fetch_byte(
                                (address & 0xFF00) + ((address + 1) & 0xFF)
                            ) as u16) << 8) +
                                self.fetch_byte(address) as u16 //replicating bug documented on wikipedia
                        };
                    },
                0b100 => //STY store y to memory
                    self.store_byte(address, self.y)
                ,
                0b101 => //LDY load y from memory
                    {
                        self.y = val;
                        self.set_zn(self.y);
                    },
                0b110 => //CPY compare to y
                    self.compare(self.y, val),
                0b111 => //CPX compare to x
                    self.compare(self.x, val),
                _ => {
                    //not implemented
                }
            },

            _ => {
                //TODO: deal with undefined cases
            }
        }
    }

    pub fn exec_next_redux(&mut self) {


        use AdrMode::*;

        let instruction = self.fetch_instruction();



        let code = (instruction & 0xF0) >> 4;
        let mode = instruction & 0xF;
        let limited_mode = mode % 8;
        let limited = code > 7;
        let split = mode > 7;
        let unlimited_quadrant = split && !limited;
        let index= if unlimited_quadrant || code == 8 {self.y} else {self.x} as u16;

        match instruction { //misc
            0xFF => { //BRK forcing interrupt
                self.push_short(self.prog_counter);
                self.push_byte(self.proc_flags);
                self.proc_flags |= FLAG_B;
                self.prog_counter = self.fetch_short(0xFFFE);
                return
            },
            0xBE | 0xCE => { //RTS/RTI return from subroutine / return from interrupt
                if code == 0xC { self.proc_flags = self.pop_byte() }
                self.prog_counter = self.pop_short() + 1;
                return
            },
            0xEE => return, //NOP
            _ => ()
        }


        let byte = self.fetch_byte(self.prog_counter + 1) as u16;
        let ind_byte = self.fetch_byte(byte) as u16;
        let short = self.fetch_short(self.prog_counter + 1);
        let ind_short = self.fetch_short(short);


        let adr_mode = match (limited_mode, unlimited_quadrant) {
            (0, false) => Address(short),                                   //Absolute
            (0, true) => Address(ind_short),                                //(abs)
            (1, _) => Address(short + index),                               //abs, x/y
            (2, false) => Address(byte),                                    //Zero Page
            (2, true) => Address(ind_byte),                                 //(zp)
            (3, _) => Address(byte + index),                                //zp, x/y
            (4, false) => Address(self.fetch_short(byte + index)),  //(zp, x/y)
            (4, true) => Address(ind_byte + self.y as u16),                 //(zp), y
            (5, false) => match (mode, code) {
                (5, 0xA) => Stack,                                          //STA A == PHA
                (5, 6..=0xD) | (0xD, 0xC..=0xF) => Accumulator,             //Accumulator
                _ => Immediate                                              //Immediate
            },
            (5, true) => Address(self.fetch_short(short + self.x as u16)), //(abs, x)
            (6, true) => Address(ind_short + self.y as u16),                //(abs), y
            _ => match (mode, code) {
                (6, 8) | (6, 0xF) | (7, 9) => Stack,                        //PHX, PHP, PHY
                (7, 0xF) => S,                                              //Stack Pointer
                (6, _) => X,                                                //X
                (7, _) => Y,                                                //Y
                (0xE, 0xB) => Accumulator,                                  //BIT acc
                (0xE, 0xC..=0xE) | (0xF, 8..=0xF) => Implied,               //Implied
                (0xE, _) => Stack,                                          //Stack pulls
                (0xF, _) | _ => Relative,                                   //branches relative
            }
        };

        let bytes =
            match (limited_mode, &adr_mode) {
                (_, Implied | Accumulator | X | Y) => 1,
                (2..=4, _) | (_, Immediate | Relative) => 2,
                _ => 3
            };
        self.prog_counter += bytes;

        let val = match &adr_mode {
            Address(address) => self.fetch_byte(*address),
            Immediate | Relative => byte as u8,
            X => self.x,
            Y => self.y,
            S => self.stack_ptr,
            P => self.proc_flags,
            Accumulator => self.accumulator,
            Stack => self.pop_byte(),
            _ => 0
        };



        if mode == 0xF { //branches, clears/sets
            let flag = match (code & 0b11, limited) {
                (0, _) =>     FLAG_C, //BCC, BCS, CLC, SEC
                (1, false) => FLAG_Z, //BNE, BEQ
                (1, true) =>  FLAG_I, //          CLI, SEI
                (2, false) => FLAG_N, //BPL, BMI
                (2, true)  => FLAG_D, //          CLD, SED
                (3, _) | _ => FLAG_V, //BVC, BVS, CLV

            };
            if !limited { //branches
                if (self.proc_flags & flag > 0) == (code > 0xB) {
                    self.prog_counter += val as u16;
                    if val > 0x7F {self.prog_counter -= 0x100}
                }
            } else { //clears/sets
                self.proc_flags &= !flag;
                self.proc_flags |= match code {
                    0xC..=0xE => flag,
                    _ => 0
                };
            }
            return
        }



        match code {
            0 => //ORA bitwise or
                self.accumulator |= val,
            1 => //AND bitwise and
                self.accumulator &= val,
            2 => //EOR bitwise exclusive or
                self.accumulator ^= val,
            3 => //CMP compare to accumulator
                self.compare(self.accumulator, val),
            4 => //ADC add with carry
                self.add(val),
            5 => //SBC subtract with carry
                self.sub(val),
            6 | 7 => //JSR / JMP
                {
                    if code == 6 { self.push_short(self.prog_counter + 2) } // JSR
                    self.prog_counter = match &adr_mode {
                        Address(address) => *address,
                        _ => val as u16
                    };
                }
            _ => match (code, mode) {
                (8..=0xB, 0..=7) | (0xF, 6 | 7) | (0xE, 6) => { //stores/transfers/pushes
                    let to_store = match code {
                        8 => self.x,
                        9 => self.y,
                        0xA => self.accumulator,
                        0xE => self.stack_ptr,
                        0xF => if mode == 6 {self.proc_flags} else {self.x},
                        _ => 0
                    };
                    match (&adr_mode, code) {
                        (Stack, _) | (_, 0xB | 0xF) => (),
                        _ => self.set_zn(to_store)
                    }
                    self.store_destination(&adr_mode, to_store)
                },
                (8..=0xA, 8..=0xE) | (0xF, 0xE) => { //loads/pulls
                    self.set_zn(val);
                    match code {
                        8 => self.x = val,
                        9 => self.y = val,
                        0xA => self.accumulator = val,
                        _ => self.proc_flags = val
                    }
                },
                (0xB, _) => { //BIT
                    self.proc_flags &= !(FLAG_Z | FLAG_V | FLAG_N); //clear Z, V, N
                    if val & self.accumulator == 0 {
                        self.proc_flags |= FLAG_Z
                    }
                    self.proc_flags |= val & 0b11000000;
                },
                (0xC..=0xF, 8..=0xD) => {
                    let carry = self.proc_flags & FLAG_C & code;
                    self.proc_flags &= !FLAG_C;
                    let left = code <= 0xD;
                    let (val, overflow) = if left {
                        ((val << 1) | carry, (0 < (0b10000000 & val)) as u8)
                    } else {
                        ((val >> 1) | (carry * 0b10000000), (1 & val))
                    };
                    self.proc_flags |= overflow;
                    self.set_zn(val);
                    self.store_destination(&adr_mode, val);
                },
                (0xC | 0xD, _) => {
                    let val = if code == 0xC {
                        val.wrapping_add(1)
                    } else {
                        val.wrapping_sub(1)
                    };
                    self.set_zn(val);
                    self.store_destination(&adr_mode, val);
                }
                (0xE, _) => self.compare(self.x, val),
                (0xF, _) | _ => self.compare(self.y, val)
            }
        }
        if code <= 5 {self.set_zn(self.accumulator)}

    }



    fn fetch_instruction(&self) -> u8 {
        self.memory[self.prog_counter as usize]
    }

    fn store_byte(&mut self, address: Option<u16>, b: u8) {
        if let Some(address) = address {
            self.memory[address as usize] = b;
        }
    }

    fn fetch_byte(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn fetch_short(&self, address: u16) -> u16 {
        (self.memory[address as usize] as u16)
            + ((self.memory[address as usize + 1] as u16) << 8)
    }

    fn push_byte(&mut self, b: u8) {
        self.memory[self.stack_ptr as usize + 0x0100] = b;
        self.stack_ptr = self.stack_ptr.wrapping_sub(1);
    }

    fn push_short(&mut self, s: u16) {
        self.push_byte((s >> 8) as u8);
        self.push_byte(s as u8);
    }

    fn pop_byte(&mut self) -> u8 {
        self.stack_ptr = self.stack_ptr.wrapping_add(1);
        self.memory[self.stack_ptr as usize + 0x0100]
    }

    fn pop_short(&mut self) -> u16 {
        (self.pop_byte() as u16) + ((self.pop_byte() as u16) << 8)
    }

    fn compare(&mut self, v1: u8, v2: u8) {
        self.proc_flags &= !(FLAG_Z | FLAG_C | FLAG_N); //clear z, c, n flags
        self.proc_flags |= (v1 >= v2) as u8 * FLAG_C;
        self.proc_flags |= (v1 == v2) as u8 * FLAG_Z;
        self.proc_flags |= ((v1.wrapping_sub(v2)) & 0b10000000 > 0) as u8 * FLAG_N;
    }

    fn set_zn(&mut self, val: u8) {
        self.proc_flags &= !(FLAG_Z | FLAG_N);
        self.proc_flags |= (val == 0) as u8 * FLAG_Z;
        self.proc_flags |= (val & 0b10000000 > 0) as u8 * FLAG_N;
    }

    fn add(&mut self, val: u8) {
        self.add_sub(val, false)
    }

    fn sub(&mut self, val: u8) {
        self.add_sub(val, true)
    }

    fn store_destination(&mut self, dest: &AdrMode, val: u8) {
        use AdrMode::*;
        match dest {
            X => self.x = val,
            Y => self.y = val,
            S => self.stack_ptr = val,
            Accumulator => self.accumulator = val,
            Address(address) => self.store_byte(Some(*address), val),
            Stack => self.push_byte(val),
            _ => ()
        }
    }

    fn add_sub(&mut self, val: u8, sub: bool) {
        let carry = self.proc_flags & FLAG_C;
        let (carry, overflow) = if self.proc_flags & FLAG_D > 0 {
            //binary coded decimal mode
            let (ad, vd) =
                ((self.accumulator / 16 * 10 + self.accumulator % 16) as i32,
                 (val / 16 * 10 + val % 16) as i32);

            let buffer = if sub { //SBC
                ad - vd - 1
            } else { //ADC
                ad + vd
            } + carry as i32;

            self.accumulator = {
                let m = (buffer % 100) as u8;
                m%10+m/10*16
            };

            (buffer > if sub {0} else {100}, buffer < -0x80 || buffer > 0x7F)
        } else {
            //binary mode
            let (a, v) = (self.accumulator as i32, val as i32);

            let buffer = if sub { //SBC
                a - v - 1
            } else { //ADC
                a + v
            } + carry as i32;

            self.accumulator = (buffer % 0x100) as u8;
            (buffer > if sub {0} else {0x100}, buffer < -0x80 || buffer > 0x7F)
        };
        self.proc_flags &= !(FLAG_C | FLAG_V);
        if carry { self.proc_flags |= FLAG_C }
        if overflow { self.proc_flags |= FLAG_V}
    }
}