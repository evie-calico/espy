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

pub mod instruction {
    pub const CLONE: u8 = 0;
    pub const POP: u8 = 1;
    pub const COLLAPSE: u8 = 2;
    pub const JUMP: u8 = 3;
    pub const IF: u8 = 4;
    pub const PUSH_UNIT: u8 = 5;
    pub const PUSH_TRUE: u8 = 6;
    pub const PUSH_FALSE: u8 = 7;
    pub const PUSH_I64: u8 = 8;
    pub const PUSH_STRING: u8 = 9;
    pub const PUSH_FUNCTION: u8 = 10;
    pub const PUSH_ENUM: u8 = 11;
    pub const ADD: u8 = 12;
    pub const SUB: u8 = 13;
    pub const MUL: u8 = 14;
    pub const DIV: u8 = 15;
    pub const PIPE: u8 = 16;
    pub const BITWISE_AND: u8 = 17;
    pub const BITWISE_OR: u8 = 18;
    pub const BITWISE_XOR: u8 = 19;
    pub const EQUAL_TO: u8 = 20;
    pub const NOT_EQUAL_TO: u8 = 21;
    pub const GREATER: u8 = 22;
    pub const GREATER_EQUAL: u8 = 23;
    pub const LESSER: u8 = 24;
    pub const LESSER_EQUAL: u8 = 25;
    pub const LOGICAL_AND: u8 = 26;
    pub const LOGICAL_OR: u8 = 27;
    pub const CALL: u8 = 28;
    pub const TUPLE: u8 = 29;
    pub const INDEX: u8 = 30;
    pub const NAME: u8 = 31;
    pub const NEST: u8 = 32;
    pub const NEGATIVE: u8 = 33;
    pub const DEREF: u8 = 34;
    pub const SET: u8 = 35;
    pub const MATCHES: u8 = 36;
}

pub mod builtins {
    use crate::StackPointer;

    pub const ANY: StackPointer = -1;
    pub const UNIT: StackPointer = -2;
    pub const I64: StackPointer = -3;
    pub const OPTION: StackPointer = -4;
    pub const MUT: StackPointer = -5;

    #[must_use]
    pub fn from_str(s: &str) -> Option<StackPointer> {
        match s {
            "any" => Some(ANY),
            "unit" => Some(UNIT),
            "i64" => Some(I64),
            "option" => Some(OPTION),
            "mut" => Some(MUT),
            _ => None,
        }
    }
}
