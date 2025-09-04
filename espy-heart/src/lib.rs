//! espy bytecode constants.
//!
//! Shared between the compiler and interpreter.

pub mod prelude {
    pub use super::*;
}

pub type ProgramCounter = u32;
pub type StackPointer = i32;
pub type BlockId = u32;
pub type StringId = u32;
pub type StringSet = u32;

// TODO: Reorder these before release.
pub mod instruction {
    // Stack and control flow: 0x00-0x0F
    pub const CLONE: u8 = 0x00;
    pub const POP: u8 = 0x01;
    pub const COLLAPSE: u8 = 0x02;
    pub const JUMP: u8 = 0x03;
    pub const IF: u8 = 0x04;
    pub const FOR: u8 = 0x05;

    // Push ops: 0x10-0x2F
    pub const PUSH_UNIT: u8 = 0x10;
    pub const PUSH_TRUE: u8 = 0x11;
    pub const PUSH_FALSE: u8 = 0x12;
    pub const PUSH_I64: u8 = 0x13;
    pub const PUSH_STRING: u8 = 0x14;
    pub const PUSH_FUNCTION: u8 = 0x15;
    pub const PUSH_ENUM: u8 = 0x16;

    // Operations: 0x30..
    pub const ADD: u8 = 0x30;
    pub const SUB: u8 = 0x31;
    pub const MUL: u8 = 0x32;
    pub const DIV: u8 = 0x33;
    pub const PIPE: u8 = 0x34;
    pub const BITWISE_AND: u8 = 0x35;
    pub const BITWISE_OR: u8 = 0x36;
    pub const BITWISE_XOR: u8 = 0x37;
    pub const EQUAL_TO: u8 = 0x38;
    pub const NOT_EQUAL_TO: u8 = 0x39;
    pub const GREATER: u8 = 0x3A;
    pub const GREATER_EQUAL: u8 = 0x3B;
    pub const LESSER: u8 = 0x3C;
    pub const LESSER_EQUAL: u8 = 0x3D;
    pub const LOGICAL_AND: u8 = 0x3E;
    pub const LOGICAL_OR: u8 = 0x3F;

    pub const CALL: u8 = 0x40;
    pub const TUPLE: u8 = 0x41;
    pub const INDEX: u8 = 0x42;
    pub const NAME: u8 = 0x43;
    pub const NEST: u8 = 0x44;
    pub const NEGATIVE: u8 = 0x45;
    pub const DEREF: u8 = 0x46;
    pub const SET: u8 = 0x47;
}

// TODO: Reorder these before release.
pub mod builtins {
    use crate::StackPointer;

    pub const OPTION: StackPointer = -1;
    pub const ANY: StackPointer = -2;
    pub const UNIT: StackPointer = -3;
    pub const I64: StackPointer = -4;
    pub const MUT: StackPointer = -5;

    pub fn from_str(s: &str) -> Option<StackPointer> {
        match s {
            "option" => Some(OPTION),
            "any" => Some(ANY),
            "unit" => Some(UNIT),
            "i64" => Some(I64),
            "mut" => Some(MUT),
            _ => None,
        }
    }
}
