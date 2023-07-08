use std::collections::HashMap;
use regex::Regex;
use crate::assembler::Operand::Byte;

#[derive(Debug, Clone, PartialEq)]
enum Operand {
    Label(String),
    NoOperand,
    Byte(u8),
    Short(u16)
}

struct Instruction {
    name: String,
    code: u8,
    mode_mask: u16,
    mode: AdrMode,
    operand: Operand,
    address: u16
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum IdxReg {
    X,Y
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum AdrMode {
    Implied,
    Relative,
    Absolute,
    Indirect,
    Immediate,
    Accumulator,
    ZeroPage,
    AbsIndexed(IdxReg),
    ZPIndexed(IdxReg),
    IndexedIndirect,
    IndirectIndexed
}

pub fn assemble(s: String) -> Vec<u8> {
    let mut labels: HashMap<&str, u16> = HashMap::new();
    let mut bare_instructions = vec![];
    let mut lines = s.lines()
        .map(|line| line.split(";").next().unwrap().trim())
        .filter(|line| !line.is_empty()); //remove comments
    let mut pc = 0;
    use AdrMode::*;
    use IdxReg::*;
    use Operand::*;
    for line in lines{
        let mut parts: Vec<&str> = line.split_whitespace().collect();
        let (label, cm, operand, name) = match parts[..] {
            [first] => {
                let m = match_opcode(first);
                if m.is_some() {
                    (None, m, None, Some(first))
                } else {
                    (Some(first), None, None, None)
                }
            },
            [first, second] => {
                let m = match_opcode(first);
                if m.is_some() {
                    (None, m, Some(second), Some(first))
                } else {
                    let m = match_opcode(second);
                    if m.is_some() {
                        (Some(first), m, None, Some(second))
                    } else {
                        panic!("{second} not a recognized opcode")
                    }
                }
            },
            [first, second, third] => {
                let m = match_opcode(first);
                if m.is_some() {
                    panic!("cannot use {first} as label, it is an opcode")
                } else {
                    let m = match_opcode(second);
                    if m.is_some() {
                        (Some(first), m, Some(third), Some(second))
                    } else {
                        panic!("{second} not a recognized opcode")
                    }
                }
            }

            _ => {panic!("{line}: invalid number of arguments!")}
        };

        if let Some(label) = label {
            let re = Regex::new(r"^([A-Za-z]+)?:?$").unwrap();
            if let Some(label) = re.captures(label).unwrap().get(1) {
                labels.insert(label.as_str(), pc);
            } else {
                panic!("{label} contains non-alphabetic characters")
            }
        }
        if let Some((code, mode_mask)) = cm {
            let name = name.unwrap();
            let (operand, mode) = if let Some(operand) = operand {
                if let Some(m) = decode_num(operand) {
                    m
                } else {
                    let mode = if code & 0b11111 == 0b10000 {
                        Relative
                    } else if name == "JMP" || name == "JSR" {
                        Absolute
                    } else {
                        panic!("Not a valid operand for {}", name)
                    };
                    (Label(operand.to_string()), mode)
                }
            } else {
                (NoOperand, Implied)
            };
            bare_instructions.push(Instruction{code, mode_mask, operand, name: name.to_string(), mode, address: pc});

            pc += match mode {
                Accumulator | Implied => 1,
                Immediate | Relative | ZeroPage | ZPIndexed(_)  => 2,
                _ => 3
            };
        }
    }

    let mut program = vec![];
    for i in bare_instructions.iter() {

        if i.operand != NoOperand {
            let mode_code = match (i.mode, i.code & 0b11) {
                (Relative,_) => 0,
                (Indirect,_) => 0b1000,
                (Absolute,_) => 0b011,
                (ZeroPage,_) => 0b001,
                (ZPIndexed(_),_) => 0b101,
                (AbsIndexed(X),_) => 0b111,
                (AbsIndexed(Y),0b01) => 0b110,
                (AbsIndexed(Y),0b10) => 0b111,
                (Immediate, 0b01) => 0b010,
                (Immediate, _) => 0b000,
                (Accumulator, 0b01) => 0b010,
                (IndirectIndexed, 0b01) => 0b100,
                (IndexedIndirect, 0b01) => 0b000,
                _ => panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name)
            };

            match (i.mode, i.name.as_str()) {
                (ZPIndexed(Y), "LDX" | "STX") => (),
                (ZPIndexed(Y), _) => panic!("Only LDX and STX can use that addressing mode"),
                (AbsIndexed(X) | ZPIndexed(X), "LDX" | "STX") =>
                    panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name),
                _ => ()
            }

            if 0 == (i.mode_mask & (1 << mode_code)) && !(i.mode == Relative && i.code & 0b11111 == 0b10000) {
                panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name)
            }
            program.push(i.code | (mode_code << 2));




            //only the bitshift operations both exist in a mode and can have no operands
        } else if i.mode_mask > 0{
            if (i.mode_mask & 0b100 == 0 || i.code & 0b11 != 0b10) {
                panic!("Cannot use {} with no operands", i.name)
            } else {
                program.push(i.code + 8);
            }
        } else {
            program.push(i.code)
        }

        let operand = match &i.operand {
            Label(s) => {
                let dest = *labels.get(s.as_str()).unwrap();
                match i.mode {
                    Absolute => {
                        Short(dest)
                    },
                    Relative => {
                        let offset = dest as i32 - i.address as i32;
                        if !(-128..=127).contains(&offset){
                            panic!("label {s} is too far to branch to")
                        }
                        Byte(if offset < 0{
                            (offset + 0x100) as u8
                        } else {
                            offset as u8
                        })
                    },
                    _ => panic!("unreachable")
                }
            },
            _ => i.operand.clone()
        };

        match operand {
            Byte(x) => program.push(x),
            Short(x) => {
                program.push(x as u8);
                program.push((x >> 8) as u8);
            },
            _ => {}
        }

    }
    program
}

fn decode_num(num: &str) -> Option<(Operand, AdrMode)> {
    use AdrMode::*;
    use IdxReg::*;
    let re = Regex::new(r"^([#(]?)([$%]?)(-?[0-9A-Fa-f]+|A)(\)?)((?:,[xyXY])?)(\)?)$").unwrap();
    let (_,[imm_p, base, val_s, close_p, ind, ind_p])
        = re.captures(num)?.extract();
    let base = match base {
        "$" => 16,
        "%" => 2,
        _ => 10
    };
    let val = match val_s {
        "A" => return Some((NoOperand, Accumulator)),
        _ => i32::from_str_radix(val_s, base).ok()?
    };

    let val = if !(-128..=0xFFFF).contains(&val) {
        return None
    } else if val < 0 {
        (val + 0x100) as u16
    } else {
        val as u16
    };



    let zp = val_s.len() < 4 && val < 0x100;
    let mode = match (imm_p, close_p, ind, ind_p) {
        ("#","","","") => Immediate,
        ("(",")","","") => Indirect,
        ("","","","") => if zp {ZeroPage} else {Absolute},
        ("","",_,"") => {
            let idx = match ind {
                ",x"|",X" => X,
                _ => Y,
            };
            if zp {ZPIndexed(idx)} else {AbsIndexed(idx)}
        },
        ("(",")",",y"|",Y","") => IndexedIndirect,
        ("(","",",x"|",X",")") => IndirectIndexed,
        _ => return None
    };

    use Operand::*;

    let val = match mode {
        Immediate | Relative | ZeroPage | ZPIndexed(_)  =>
            if val < 0x100 {
                Byte(val as u8)
            } else {
                panic!("{val} too large for this addressing mode")
            },
        _ => {
            Short(val)
        }
    };
    Some((val, mode))
}

fn match_opcode(op: &str) -> Option<(u8,u16)> {
    let op = op.to_uppercase();
    let code = match op.as_str() {
        "BPL" => 0x10,"BMI" => 0x30,"BVC" => 0x50,"BVS" => 0x70,
        "BCC" => 0x90,"BCS" => 0xB0,"BNE" => 0xD0,"BEQ" => 0xF0,

        "BRK" => 0x00,"JSR" => 0x20,"RTI" => 0x40,"RTS" => 0x60,

        "PHP" => 0x08,"PLP" => 0x28,"PHA" => 0x48,"PLA" => 0x68,

        "DEY" => 0x88,"INY" => 0xC8,"DEX" => 0xCA,"INX" => 0xE8,

        "TAY" => 0xA8,"TYA" => 0x98,"TXA" => 0x8A,"TAX" => 0xAA,
        "TXS" => 0x9A,"TSX" => 0xBA,

        "NOP" => 0xEA,

        "CLC" => 0x18,"SEC" => 0x38,"CLI" => 0x58,"SEI" => 0x78,
        "CLV" => 0xB8,"CLD" => 0xD8,"SED" => 0xF8,

        "ORA" => 0x01,"AND" => 0x21,"EOR" => 0x41,"ADC" => 0x61,
        "STA" => 0x81,"LDA" => 0xA1,"CMP" => 0xC1,"SBC" => 0xE1,

        "ASL" => 0x02,"ROL" => 0x22,"LSR" => 0x42,"ROR" => 0x62,
        "STX" => 0x82,"LDX" => 0xA2,"DEC" => 0xC2,"INC" => 0xE2,

        "BIT" => 0x20,"JMP" => 0x40,"STY" => 0x80,"LDY" => 0xA0,
        "CPY" => 0xC0,"CPX" => 0xE0,
        _ => -1
    };

    let mode_mask = match op.as_str() {
        "ORA"|"AND"|"EOR"|"ADC"|"LDA"|"CMP"|"SBC" => 0b11111111,
        "STA" => 0b11111011, //no immediate addressing
        "ASL"|"ROL"|"LSR"|"ROR" => 0b10101110,
        "STX" => 0b00101010,
        "LDX" => 0b10101011,
        "DEC" | "INC" => 0b10101010,
        "BIT" => 0b1010,
        "STY" => 0b00101010,
        "LDY" => 0b10101011,
        "CPY"|"CPX" => 0b1011,
        "JMP" => 0b100000100,

        _ => 0
    };
    if code < 0 {return None}
    Some((code as u8, mode_mask))
}
