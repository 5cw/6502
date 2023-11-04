use std::collections::HashMap;
use regex::Regex;

const RELATIVE_MASK: u16 = 0b1000_0000_0000_0000;
use AdrMode::*;
use IdxReg::*;
use Operand::*;
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

enum Directive {
    Instr(Instruction),
    LabelDir(String),
    Org(u16)
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum IdxReg {
    X,Y
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum AdrMode {
    Implied,
    Relative,
    Absolute(bool),
    Indirect(bool),
    Immediate,
    Accumulator,
    Indexed(IdxReg, bool),
    IndexedIndirect(bool),
    IndirectIndexed(IdxReg, bool),
    XReg,
    YReg,
    Stack,
}

pub fn assemble(s: String, redux: bool) -> Vec<u8> {
    let mut labels: HashMap<&str, u16> = HashMap::new();
    let mut bare_instructions = vec![];
    let mut lines = s.lines()
        .map(|line| line.split(";").next().unwrap().trim()); //remove comments
    let mut pc = 0;
    use AdrMode::*;
    use IdxReg::*;
    use Operand::*;
    for line in lines {
        match parse_line(line, if redux {match_opcode_redux} else {match_opcode}, pc) {
            Directive::Org(num) => pc = num,
            Directive::LabelDir(s) => { labels.insert(s.as_str(), pc); },
            Directive::Instr(i) => {
                pc += match i.mode {
                    Implied | Accumulator | XReg | YReg | Stack => 1,
                    Immediate | Relative => 2,
                    Absolute(long)|Indirect(long)|Indexed(_, long)|
                    IndexedIndirect(long)|IndirectIndexed(_, long) => 2 + long as u16
                };
                bare_instructions.push(i)
            }
        }
    }

    let mut program = vec![];
    for i in bare_instructions.iter() {

        program.push(if redux {final_code_redux(i)} else {final_code(i)});

        let operand = match &i.operand {
            Label(s) => {
                let dest = *labels.get(s.as_str()).unwrap();
                match i.mode {
                    Absolute(true) | Indirect(true) |
                    Indexed(_, true) | IndirectIndexed(_, true) | IndexedIndirect(true) => {
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


fn parse_line(line: &str, match_opcode: fn(&str) -> Option<(u8, u16)>, pc: u16) -> Directive {
    let parts: Vec<&str> = line.split_whitespace().collect();
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
            if first == "ORG" {
                if let Some((operand, Absolute(_))) = decode_num(second) {
                    return Directive::Org(match operand {
                        Byte(num) => num as u16,
                        Short(num) => num,
                        _ => panic!("invalid ORG address")
                    })
                }
            }
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
        return Directive::LabelDir(sanitize_label(label).to_string())
    }
    if let Some((code, mode_mask)) = cm {
        let name = name.unwrap();
        let (operand, mode) = if let Some(operand) = operand {
            if let Some((o,a)) = decode_num(operand) {
                if mode_mask & RELATIVE_MASK > 0 {(o, Relative)} else {(o,a)}
            } else {
                panic!("Not a valid operand for {}", name);
            }
        } else {
            (NoOperand, Implied)
        };
        return Directive::Instr(Instruction{code, mode_mask, operand, name: name.to_string(), mode, address: pc});
    }
    panic!("unreachable")
}

fn decode_num(num: &str) -> Option<(Operand, AdrMode)> {
    use AdrMode::*;
    use IdxReg::*;
    let num = num.to_uppercase().as_str();
    let re = Regex::new(r"^([#(]?)([$%]?)(-?[0-9A-Fa-f]+|[AXY]|STACK)(\)?)((?:,[XY])?)(\)?)$").unwrap();
    let (_,[imm_p, base, val_s, close_p, ind, ind_p])
        = re.captures(num)?.extract();
    let base = match base {
        "$" => 16,
        "%" => 2,
        _ => 10
    };
    let val = match i32::from_str_radix(val_s, base).ok() {
        Some(val) => {
            let val = if !(-128..=0xFFFF).contains(&val) {
                return None
            } else if val < 0 {
                (val + 0x100) as u16
            } else {
                val as u16
            };
            let zp = !((val_s.len() < 4 || (base < 10 && val_s.len() < 16)) && val < 0x100);
            if zp {Byte(val as u8)} else {Short(val)}
        },
        None => match num {
            "A" => return Some((NoOperand, Accumulator)),
            "X" => return Some((NoOperand, XReg)),
            "Y" => return Some((NoOperand, YReg)),
            "STACK" => return Some((NoOperand, Stack)),
            _ => Label(sanitize_label(num).to_string())
        }
    };

    let zp = match val {
        Byte(_) => false,
        _ => true
    };

    let idx = match ind {
        ",X" => X,
        _ => Y,
    };


    let mode = match (imm_p, close_p, ind, ind_p) {
        ("#","","","") => Immediate,
        ("(",")","","") => Indirect(zp),
        ("","","","") => Absolute(zp),
        ("","",_,"") => Indexed(idx, zp),
        ("(",")",",Y","") => IndexedIndirect(zp),
        ("(","",_,")") => IndirectIndexed(idx, zp),
        _ => return None
    };

    use Operand::*;

    Some((val, mode))
}

fn sanitize_label(label: &str) -> &str {
    let re = Regex::new(r"^([A-Za-z]+)?:?$").unwrap();
    if let Some(label) = re.captures(label).unwrap().get(1) {
        return label.as_str()
    } else {
        panic!("{label} contains non-alphabetic characters")
    }
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
        _ => return None
    };

    let mode_mask = match op.as_str() {
        "ORA"|"AND"|"EOR"|"ADC"|"LDA"|"CMP"|"SBC" => 0b1111_1111,
        "STA" => 0b1111_1011, //no immediate addressing
        "ASL"|"ROL"|"LSR"|"ROR" => 0b1010_1110,
        "STX" => 0b0010_1010,
        "LDX" => 0b1010_1011,
        "DEC" | "INC" => 0b1010_1010,
        "BIT" => 0b1010,
        "STY" => 0b0010_1010,
        "LDY" => 0b1010_1011,
        "CPY"|"CPX" => 0b1011,
        "JMP" => 0b1_0000_0100,
        "BPL"|"BMI"|"BVC"|"BVS"|"BCC"|"BCS"|"BNE"|"BEQ" => RELATIVE_MASK,
        _ => 0
    };
    Some((code, mode_mask))
}

fn final_code(i: &Instruction) -> u8 {
    if i.operand != NoOperand {
        let mode_code = match (i.mode, i.code & 0b11) {
            (Relative,_) => 0,
            (Indirect(true),_) => 0b1000,
            (Absolute(true),_) => 0b011,
            (Absolute(false),_) => 0b001,
            (Indexed(_,false),_) => 0b101,
            (Indexed(X, true),_) => 0b111,
            (Indexed(Y, true),0b01) => 0b110,
            (Indexed(Y, true),0b10) => 0b111,
            (Immediate, 0b01) => 0b010,
            (Immediate, _) => 0b000,
            (Accumulator, 0b01) => 0b010,
            (IndirectIndexed(X, false), 0b01) => 0b100,
            (IndexedIndirect(false), 0b01) => 0b000,
            _ => panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name)
        };

        match (i.mode, i.name.as_str()) {
            (Indexed(Y, false), "LDX" | "STX") => (),
            (Indexed(Y, false), _) => panic!("Only LDX and STX can use that addressing mode"),
            (Indexed(X, true) | Indexed(X, true), "LDX" | "STX") =>
                panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name),
            _ => ()
        }

        if 0 == (i.mode_mask & (1 << mode_code)) && !(i.mode == Relative && i.code & 0b11111 == 0b10000) {
            panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name)
        }

        i.code | (mode_code << 2)

        //only the bitshift operations both exist in a mode and can have no operands
    } else if i.mode_mask > 0 {
        if i.mode_mask & 0b100 == 0 || i.code & 0b11 != 0b10 {
            panic!("Cannot use {} with no operands", i.name)
        } else {
            i.code + 8
        }
    } else {
        i.code
    }
}


fn match_opcode_redux(op: &str) -> Option<(u8,u16)> {
    let op = op.to_uppercase();
    let code = match op.as_str() {
        "ORA" => 0x00,"AND" => 0x10,"EOR" => 0x20,"CMP" => 0x30,
        "ADC" => 0x40,"SBC" => 0x50,"JMP" => 0x60,"JSR" => 0x70,

        "STX" => 0x80,"TXA" => 0x85,"PHX" => 0x86,"TXY" => 0x87,"LDX" => 0x88,"PLX" => 0x8E,
        "STY" => 0x90,"TYA" => 0x95,"TYX" => 0x96,"PHY" => 0x97,"LDY" => 0x98,"PLY" => 0x9E,
        "STA" => 0xA0,"PHA" => 0xA5,"TAX" => 0xA6,"TAY" => 0xA7,"LDA" => 0xA8,"PLA" => 0xAE,
        "STZ" => 0xB0,"TZA" => 0xB5,"TZX" => 0xB6,"TZY" => 0xB7,
                                    "TSX" => 0xE6,"TXS" => 0xF7,
                                    "PHP" => 0xF6,                            "PLP" => 0xFE,

        "BIT" => 0xB8,
        "INC" => 0xC0,"INA" => 0xC5,"INX" => 0xC6,"INY" => 0xC7,
        "DEC" => 0xD0,"DEA" => 0xD5,"DEX" => 0xD6,"DEY" => 0xD7,

        "CPX" => 0xE0,                            "CXY" => 0xE7,
        "CPY" => 0xF0,

        "ASL" => 0xC8,"ROL" => 0xD8,"LSR" => 0xE8,"ROR" => 0xF8,

        "BCC" => 0x0F,"BNE" => 0x1F,"BPL" => 0x2F,"BVC" => 0x3F,
        "BCS" => 0x4F,"BEQ" => 0x5F,"BMI" => 0x6F,"BVS" => 0x7F,
        "CLC" => 0x8F,"CLI" => 0x9F,"CLD" => 0xAF,"CLV" => 0xBF,
        "SEC" => 0xCF,"SEI" => 0xDF,"SED" => 0xEF,

        "RTS" => 0xCE,"RTI" => 0xDE,"NOP" => 0xEE,"BRK" => 0xFF,

        _ => return None
    };

    let mode_mask = match op.as_str() {
        //0bSAIXY
        //Stack, Accumulator, Immediate, X, Y
        "ORA"|"AND"|"EOR"|"CMP"|"ADC"|"SBC" => 0b00111,
        "JMP"|"JSR"|"DEC"|"INC" => 0b01011,
        "STA" => 0b10011,
        "STX" => 0b11010,
        "STY" => 0b11001,
        "LDA"|"LDX"|"LDY" => 0b10100,
        "CPX" => 0b00101,
        "CPY" => 0b00100,
        "BIT" => 0b01100,
        "ASL"|"ROL"|"LSR"|"ROR" => 0b01000,
        "BPL"|"BMI"|"BVC"|"BVS"|"BCC"|"BCS"|"BNE"|"BEQ" => RELATIVE_MASK, //Relative
        _ => 0
    };
    Some((code, mode_mask))
}

fn final_code_redux(i: &Instruction) -> u8 {
    if i.code & 0b111 > 0 {
        return i.code
    }
    //0bSAIXY
    // this must be calculated separately because mode 5 is both accumulator and immediate
    // and that would interpret STX #14 as a valid alias for TXA but TXA doesn't take any args
    let mask = match i.mode {
        Relative => RELATIVE_MASK,
        Stack => 0b10000,
        Accumulator => 0b1000,
        Immediate => 0b100,
        XReg => 0b10,
        YReg => 0b1,
        _ => 0
    };
    if mask > 0 && mask & i.mode_mask < 0 {
        panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name)
    }
    let mode = match (i.mode, i.code) {
        (Indexed(X, _)|IndirectIndexed(X, false), 0x80 | 0x88)
        => panic!("cannot use STX or LDX with X index"),
        (Indexed(Y,false), 0x80 | 0x88) => 1,
        (Indexed(Y,true), 0x80 | 0x88) => 3,
        (IndirectIndexed(Y, false), 0x80 | 0x88) => 4,
        (Stack, 0x90) => 7, //STY Stack -> PHY
        (Stack, 0xA0) => 5, //STA Stack -> PHA
        (Accumulator, 0xA8) => 6, //BIT A
        _ => match i.mode {
            Absolute(true) => 0,
            Indexed(X, true) => 1,
            Absolute(false) => 2,
            Indexed(X, false) => 3,
            IndirectIndexed(X, false) => 4,
            Immediate | Accumulator => 5,
            XReg | Stack => 6,
            YReg => 7,
            _ => if i.code < 0x80 {
                match i.mode {
                    Indirect(true) => 8,
                    Indexed(Y, true) => 9,
                    Indirect(false) => 0xA,
                    Indexed(Y, false) => 0xB,
                    IndexedIndirect(false) => 0xC,
                    IndirectIndexed(X, true) => 0xD,
                    IndexedIndirect(true) => 0xE,
                    _ => panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name)
                }
            } else {
                panic!("Cannot use {:?} addressing mode for {}", i.mode, i.name)
            }
        }
    };
    i.code | mode
}