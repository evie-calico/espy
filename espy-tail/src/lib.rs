//! Compiles an espyscript abstract syntax tree into bytecode.
//!
//! ```rust
//! use espy_eyes::Lexer;
//! use espy_ears::Block;
//! use espy_tail::Program;
//!
//! let mut lexer = Lexer::from("1 + 2").peekable();
//! let block = Block::from(&mut lexer);
//! // note: this *may* panic if any branch of `block` contains error diagnostics.
//! // even if it does not, a block containing error diagnostics may not produce the intended program.
//! let program = Program::try_from(block).unwrap();
//! let bytecode = program.compile();
//! ```

use espy_ears::{Action, Block, BlockResult, Diagnostics, Expression, For, If, Node, Statement};
use espy_eyes::{Lexigram, Token};
use espy_heart::prelude::*;
use std::{cell::Cell, iter, mem, num::ParseIntError};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub enum Error<'source> {
    ProgramLimitExceeded,
    InvalidBreak(Token<'source>),
    InvalidInteger(ParseIntError),
    UndefinedSymbol(&'source str),
    UnexpectedEnumResult,
    InvalidAst(espy_ears::Error<'source>),
}

impl From<ParseIntError> for Error<'_> {
    fn from(e: ParseIntError) -> Self {
        Self::InvalidInteger(e)
    }
}

fn try_validate(diagnostics: Diagnostics) -> Result<(), Error> {
    if let Some(error) = diagnostics.errors.into_iter().next() {
        Err(Error::InvalidAst(error))
    } else {
        Ok(())
    }
}

// These are practically used as functions which return iterators over bytes at this point.
// There isn't a good reason for this other than that they used to be stored for a little while,
// so if true functions make more sense for any reason this type can be removed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    /// Copy a value from the given position and put it on the top of the stack.
    Clone(StackPointer),
    /// Pop a value off the stack.
    Pop,
    /// Pop a value off the stack and write it to the given position.
    /// Then, pop each value above this position and discard it.
    Collapse(StackPointer),
    /// Set the program counter.
    Jump(ProgramCounter),
    /// Pop a boolean value off the stack.
    /// If it is *false*, set the program counter.
    If(ProgramCounter),
    /// Shortcut for for loops.
    /// This reads the top value on the stack but does not pop it,
    /// unwraps the inner value and pushes it to the stack if it is Some,
    /// and jumps to the following address if it is None.
    For(ProgramCounter),

    PushUnit,
    PushI64(i64),
    /// Pop the top `captures` values off the stack and onto a new stack,
    /// and then push a function containing the new stack and the proceeding block id to the current stack.
    PushFunction {
        captures: StackPointer,
        function: BlockId,
    },
    PushTrue,
    PushFalse,
    /// Pop the top value off the stack;
    /// it should be unit or a named tuple of functions.
    /// Use this value as the enum's methods.
    ///
    /// Then, pop a value off the stack for each string in the string set `statics`.
    /// Each of these values is a static member of the enum;
    /// any functions may construct this enum and access its fields.
    ///
    /// Pop the next value off the stack;
    /// it should be unit or a named tuple of types.
    /// Use this value as the enum's variants.
    ///
    /// Push the resulting enum type to the stack.
    PushEnum {
        statics: StringSet,
    },
    /// Pop the top value off the stack;
    /// it should be unit or a named tuple of functions.
    /// Use this value as the struct's methods.
    ///
    /// Then, pop a value off the stack for each string in the string set `statics`.
    /// Each of these values is a static member of the struct;
    /// any functions may construct this enum and access its fields.
    ///
    /// Pop the next value off the stack;
    /// it should be a type.
    /// Use this type as the type of the struct's inner value.
    ///
    /// Push the resulting struct type to the stack.
    PushStruct {
        statics: StringSet,
    },
    PushString(StringId),

    Add,
    Sub,
    Mul,
    Div,
    /// Pop the top value off the stack and push it to the function's stack.
    /// The next value is the function to be called.
    /// After pushing the value, jump to the function's block id.
    /// It will return a single value which is placed to the stack in its place.
    Call,
    /// Pop the first value off the stack, then pop the second and index it using the first.
    /// Push the result.
    Index,
    /// Pop two values off the stack and combine them into a tuple.
    /// If one of them is already a tuple, concatenate them (always producing a flat tuple).
    /// The topmost value should be appended to the value underneath it.
    Tuple,
    /// Pop a value off the stack and turn it into a named tuple with a single value,
    /// and a name according to the following string id.
    Name(StringId),
    Positive,
    Negative,
    Pipe,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    EqualTo,
    NotEqualTo,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    LogicalAnd,
    LogicalOr,
}

pub struct InstructionIter {
    instruction: Instruction,
    index: usize,
}

impl Iterator for InstructionIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! decompose {
            ($instruction:expr, $($arg:ident as $offset:literal..=$end:literal),*) => {
                match self.index {
                    0 => $instruction,
                    $(
                        $offset..=$end => $arg.to_le_bytes()[self.index - $offset],
                    )*
                    _ => return None,
                }
            };
        }
        let byte = match self.instruction {
            Instruction::Clone(from) => decompose!(instruction::CLONE, from as 1..=4),
            Instruction::Pop => decompose!(instruction::POP,),
            Instruction::Collapse(to) => decompose!(instruction::COLLAPSE, to as 1..=4),
            Instruction::Jump(pc) => decompose!(instruction::JUMP, pc as 1..=4),
            Instruction::If(pc) => decompose!(instruction::IF, pc as 1..=4),
            Instruction::For(escape) => decompose!(instruction::FOR, escape as 1..=4),

            Instruction::PushUnit => decompose!(instruction::PUSH_UNIT,),
            Instruction::PushI64(literal) => decompose!(instruction::PUSH_I64, literal as 1..=8),
            Instruction::PushFunction { captures, function } => {
                decompose!(instruction::PUSH_FUNCTION, captures as 1..=4, function as 5..=8)
            }
            Instruction::PushTrue => decompose!(instruction::PUSH_TRUE,),
            Instruction::PushFalse => decompose!(instruction::PUSH_FALSE,),
            Instruction::PushEnum { statics } => {
                decompose!(instruction::PUSH_ENUM, statics as 1..=4)
            }
            Instruction::PushStruct { statics } => {
                decompose!(instruction::PUSH_STRUCT, statics as 1..=4)
            }

            Instruction::PushString(s) => decompose!(instruction::PUSH_STRING, s as 1..=4),

            Instruction::Add => decompose!(instruction::ADD,),
            Instruction::Sub => decompose!(instruction::SUB,),
            Instruction::Mul => decompose!(instruction::MUL,),
            Instruction::Div => decompose!(instruction::DIV,),
            Instruction::Call => decompose!(instruction::CALL,),
            Instruction::Index => decompose!(instruction::INDEX,),
            Instruction::Tuple => decompose!(instruction::TUPLE,),
            Instruction::Name(name) => decompose!(instruction::NAME, name as 1..=4),
            Instruction::Positive => decompose!(instruction::POSITIVE,),
            Instruction::Negative => decompose!(instruction::NEGATIVE,),
            Instruction::Pipe => decompose!(instruction::PIPE,),
            Instruction::BitwiseAnd => decompose!(instruction::BITWISE_AND,),
            Instruction::BitwiseOr => decompose!(instruction::BITWISE_OR,),
            Instruction::BitwiseXor => decompose!(instruction::BITWISE_XOR,),
            Instruction::EqualTo => decompose!(instruction::EQUAL_TO,),
            Instruction::NotEqualTo => decompose!(instruction::NOT_EQUAL_TO,),
            Instruction::Greater => decompose!(instruction::GREATER,),
            Instruction::GreaterEqual => decompose!(instruction::GREATER_EQUAL,),
            Instruction::Lesser => decompose!(instruction::LESSER,),
            Instruction::LesserEqual => decompose!(instruction::LESSER_EQUAL,),
            Instruction::LogicalAnd => decompose!(instruction::LOGICAL_AND,),
            Instruction::LogicalOr => decompose!(instruction::LOGICAL_OR,),
        };
        self.index += 1;
        Some(byte)
    }
}

impl IntoIterator for Instruction {
    type Item = u8;
    type IntoIter = InstructionIter;
    fn into_iter(self) -> Self::IntoIter {
        InstructionIter {
            instruction: self,
            index: 0,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program<'source> {
    blocks: Vec<Vec<u8>>,
    strings: Vec<&'source str>,
    /// To index this, subtract 1 from the string set's Id.
    /// A string set 0 should be considered an empty set.
    string_sets: Vec<Vec<StringId>>,
}

impl<'source> Program<'source> {
    pub fn compile(self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend((self.blocks.len() as u32).to_le_bytes());
        output.extend((self.strings.len() as u32).to_le_bytes());
        output.extend((self.string_sets.len() as u32).to_le_bytes());
        // Reserve space for vector offsets.
        // Blocks, strings, and string sets are only referred to by index,
        // so this is the only program-wide retroactive filling required.
        let block_offsets = output.len();
        output.extend(iter::repeat_n(0, self.blocks.len() * size_of::<u32>()));
        // same for strings.
        let string_offsets = output.len();
        output.extend(iter::repeat_n(0, self.strings.len() * size_of::<u32>()));
        // and string sets (these are currently only used for enums)
        let string_set_offsets = output.len();
        output.extend(iter::repeat_n(0, self.string_sets.len() * size_of::<u32>()));

        // Fill in offsets.
        for (block_id, block) in self.blocks.into_iter().enumerate() {
            let src = output.len() as u32;
            let dest = block_offsets + block_id * size_of::<u32>();
            output[dest..(dest + size_of::<u32>())].copy_from_slice(&src.to_le_bytes());
            output.extend(block);
        }
        for (string_id, string) in self.strings.into_iter().enumerate() {
            let src = output.len() as u32;
            let dest = string_offsets + string_id * size_of::<u32>();
            output[dest..(dest + size_of::<u32>())].copy_from_slice(&src.to_le_bytes());
            output.extend(string.bytes());
        }
        for (string_set_id, string_set) in self.string_sets.into_iter().enumerate() {
            let src = output.len() as u32;
            let dest = string_set_offsets + string_set_id * size_of::<u32>();
            output[dest..(dest + size_of::<u32>())].copy_from_slice(&src.to_le_bytes());
            output.extend(string_set.into_iter().flat_map(|x| x.to_le_bytes()));
        }

        output
    }

    fn create_block(&mut self) -> Result<BlockId, Error<'source>> {
        self.blocks.push(Vec::new());
        (self.blocks.len() - 1)
            .try_into()
            .map_err(|_| Error::ProgramLimitExceeded)
    }

    fn create_string(&mut self, s: &'source str) -> Result<StringId, Error<'source>> {
        if let Some((i, _)) = self.strings.iter().enumerate().find(|(_, x)| **x == s) {
            i
        } else {
            self.strings.push(s);
            self.strings.len() - 1
        }
        .try_into()
        .map_err(|_| Error::ProgramLimitExceeded)
    }

    fn add_block(
        &mut self,
        block_id: BlockId,
        block: Block<'source>,
        mut scope: Scope<'_, 'source>,
    ) -> Result<(), Error<'source>> {
        self.insert_block(block_id, block, &mut scope)?;
        scope.resolve_breaks(&mut self.blocks[block_id as usize]);
        Ok(())
    }

    fn insert_block(
        &mut self,
        block_id: BlockId,
        block: Block<'source>,
        scope: &mut Scope<'_, 'source>,
    ) -> Result<(), Error<'source>> {
        try_validate(block.diagnostics)?;
        for i in block.statements {
            self.add_statement(block_id, i, scope)?;
        }
        // Collapse the scope if any additional values are left the stack.
        // This occurs when statements bind variables.
        // We have to check this here because BlockResult::Function is about to move the scope,
        // but the instruction is not emitted until after the result is pushed to the stack.
        // This is fine because block results must push one and only one value.
        let collapse_point = scope
            .parent
            .filter(|parent| parent.stack_pointer < scope.stack_pointer)
            .map(|parent| parent.stack_pointer);
        match block.result {
            BlockResult::Expression(i) => {
                self.add_expression(block_id, i, scope)?;
            }
            BlockResult::Break {
                break_token,
                expression,
            } => {
                self.add_expression(block_id, expression, scope)?;
                let mut candidate = &*scope;
                while !candidate.implicit_break {
                    let Some(parent) = candidate.parent else {
                        return Err(Error::InvalidBreak(break_token));
                    };
                    candidate = parent;
                }
                if candidate.stack_pointer < scope.stack_pointer {
                    self.blocks[block_id as usize].extend(Instruction::Collapse(
                        scope.stack_pointer - candidate.stack_pointer,
                    ));
                }
                self.blocks[block_id as usize].extend(Instruction::Jump(0));
                candidate.request_break(self.blocks[block_id as usize].len() as ProgramCounter - 4);
                return Ok(());
            }
            BlockResult::Function(function) => {
                try_validate(function.diagnostics)?;
                let mut scope = scope.promote();
                let captures = scope.stack_pointer;
                // Note that, while a function takes a single argument,
                // the inner block recieves it destructured as expected by `with`.
                for argument in function.arguments {
                    scope.stack_pointer += 1;
                    scope.insert(argument.origin);
                }
                let function_id = self.create_block()?;
                self.add_block(function_id, function.block, scope)?;
                self.blocks[block_id as usize].extend(Instruction::PushFunction {
                    captures,
                    function: function_id,
                })
            }
        }
        if let Some(collapse_point) = collapse_point {
            self.blocks[block_id as usize].extend(Instruction::Collapse(collapse_point));
        }
        Ok(())
    }

    /// # Panic
    ///
    /// Panics if provided an invalid statement.
    /// Statements should be validated beforehand by checking the `diagnostics.errors` field.
    fn add_statement(
        &mut self,
        block_id: BlockId,
        statement: Statement<'source>,
        scope: &mut Scope<'_, 'source>,
    ) -> Result<(), Error<'source>> {
        macro_rules! block {
            () => {
                self.blocks[block_id as usize]
            };
        }
        try_validate(statement.diagnostics)?;
        match statement.action {
            Action::Evaluate(binding, expression) => {
                // If the binding exists but not an expression, we need to generate a unit value.
                if expression.is_some() || binding.is_some() {
                    self.add_expression(block_id, expression.unwrap_or_default(), scope)?;
                }
                if let Some(binding) = binding {
                    scope.insert(
                        binding
                            .ident_token
                            .expect("invalid statement structure")
                            .origin,
                    );
                } else {
                    block!().extend(Instruction::Pop);
                    scope.stack_pointer -= 1;
                }
            }
            Action::For(For {
                binding,
                iterator,
                block,
                diagnostics,
                ..
            }) => {
                try_validate(diagnostics)?;
                self.add_expression(block_id, iterator, scope)?;
                let for_address = block!().len();
                block!().extend(Instruction::For(0));
                scope.stack_pointer += 1;
                let escape_address = block!().len() - 4;
                let mut child = scope.child().implicit_break();
                if let Some(binding) = binding {
                    child.insert(binding.origin);
                }
                // Note the insert-we need to inject two more instructions into this block.
                self.insert_block(block_id, block, &mut child)?;
                // For loop return values can't be used.
                block!().extend(Instruction::Pop);
                block!().extend(Instruction::Jump(for_address as u32));
                // Now the inside of the for has been resolved.
                child.resolve_breaks(&mut block!());
                let escape_target = block!().len() as u32;
                block!()[escape_address..(escape_address + size_of::<u32>())]
                    .copy_from_slice(&escape_target.to_le_bytes());
                // Remove the iterator from the stack.
                block!().extend(Instruction::Pop);
            }
            Action::Implementation(_) => todo!(),
        }
        Ok(())
    }

    fn add_expression(
        &mut self,
        block_id: BlockId,
        expression: Expression<'source>,
        scope: &mut Scope<'_, 'source>,
    ) -> Result<(), Error<'source>> {
        // Shortcut for re-indexing self.blocks.
        // This is necessary because add_block etc take &mut self.
        macro_rules! block {
            () => {
                self.blocks[block_id as usize]
            };
        }
        macro_rules! binop {
            ($instruction:expr) => {{
                scope.stack_pointer -= 1;
                block!().extend($instruction)
            }};
        }
        try_validate(expression.diagnostics)?;
        if expression.contents.is_empty() {
            scope.stack_pointer += 1;
            block!().extend(Instruction::PushUnit);
            return Ok(());
        }
        for node in expression.contents {
            match node {
                Node::Unit => {
                    scope.stack_pointer += 1;
                    block!().extend(Instruction::PushUnit)
                }
                Node::Number(Token { origin, .. }) => {
                    let integer = origin.parse()?;
                    scope.stack_pointer += 1;
                    block!().extend(Instruction::PushI64(integer))
                }
                Node::String(Token { origin, .. }) => {
                    // trim starting and ending quotes.
                    // adjust this if additional string formats are added.
                    let string = self.create_string(&origin[1..origin.len() - 1])?;
                    scope.stack_pointer += 1;
                    block!().extend(Instruction::PushString(string))
                }
                Node::Variable(Token { origin, .. }) => {
                    let value = scope.get(origin).ok_or(Error::UndefinedSymbol(origin))?;
                    scope.stack_pointer += 1;
                    block!().extend(Instruction::Clone(value.index))
                }
                Node::Add(_) => binop!(Instruction::Add),
                Node::Sub(_) => binop!(Instruction::Sub),
                Node::Mul(_) => binop!(Instruction::Mul),
                Node::Div(_) => binop!(Instruction::Div),
                Node::Tuple(_) => binop!(Instruction::Tuple),
                Node::Call(_) => binop!(Instruction::Call),
                Node::Pipe(_) => binop!(Instruction::Pipe),
                Node::BitwiseAnd(_) => binop!(Instruction::BitwiseAnd),
                Node::BitwiseOr(_) => binop!(Instruction::BitwiseOr),
                Node::BitwiseXor(_) => binop!(Instruction::BitwiseXor),
                Node::EqualTo(_) => binop!(Instruction::EqualTo),
                Node::NotEqualTo(_) => binop!(Instruction::NotEqualTo),
                Node::Greater(_) => binop!(Instruction::Greater),
                Node::GreaterEqual(_) => binop!(Instruction::GreaterEqual),
                Node::Lesser(_) => binop!(Instruction::Lesser),
                Node::LesserEqual(_) => binop!(Instruction::LesserEqual),
                Node::LogicalAnd(_) => binop!(Instruction::LogicalAnd),
                Node::LogicalOr(_) => binop!(Instruction::LogicalOr),
                // Unaries
                // These do not move the stack,
                // and say "+= 0" to make this clear.
                Node::Positive(_) => {
                    scope.stack_pointer += 0;
                    block!().extend(Instruction::Positive);
                }
                Node::Negative(_) => {
                    scope.stack_pointer += 0;
                    block!().extend(Instruction::Negative);
                }
                Node::Name {
                    name,
                    colon_token: _,
                } => {
                    scope.stack_pointer += 0;
                    let s = self.create_string(name.origin)?;
                    block!().extend(Instruction::Name(s));
                }

                Node::Block(block) => {
                    self.add_block(block_id, block, scope.child())?;
                    scope.stack_pointer += 1;
                }
                Node::Bool(boolean, _) => {
                    scope.stack_pointer += 1;
                    block!().extend(if boolean {
                        Instruction::PushTrue
                    } else {
                        Instruction::PushFalse
                    })
                }
                Node::If(If {
                    condition,
                    first,
                    second,
                    diagnostics,
                    ..
                }) => {
                    // For retroactively filling in the jump instructions.
                    fn fill(block: &mut [u8], at: usize) {
                        let pc = block.len() as ProgramCounter;
                        block[at..(at + size_of::<ProgramCounter>())]
                            .copy_from_slice(&pc.to_le_bytes());
                    }

                    try_validate(diagnostics)?;
                    self.add_expression(block_id, condition, scope)?;
                    block!().extend(Instruction::If(0));
                    let if_destination = block!().len() - size_of::<ProgramCounter>();
                    self.add_block(block_id, first, scope.child())?;

                    // the first block always returns a value (though it may be implicit unit)
                    // so there always needs to be an else block with some value as well,
                    // but an empty else block will merely return unit.

                    // This jump is the last instruction of the first block--
                    // it skips over else.
                    block!().extend(Instruction::Jump(0));
                    let jump_destination = block!().len() - size_of::<ProgramCounter>();

                    // Now that the first block is complete,
                    // we can fill in the conditional jump's destination.
                    fill(&mut block!(), if_destination);
                    self.add_block(block_id, second, scope.child())?;

                    fill(&mut block!(), jump_destination);
                }
                Node::Field {
                    dot_token: _,
                    index,
                } => {
                    match index {
                        Token {
                            lexigram: Lexigram::Ident,
                            origin,
                        } => {
                            let s = self.create_string(origin)?;
                            block!().extend(Instruction::PushString(s));
                        }
                        Token {
                            lexigram: Lexigram::Number,
                            origin,
                        } => {
                            let integer = origin.parse()?;
                            block!().extend(Instruction::PushI64(integer));
                        }
                        _ => {
                            panic!("expected an identifier or number in field index, got {index:?}")
                        }
                    }
                    // the stack pointer does not move by the end,
                    // because while Index pops twice, we performed one of the pushes ourselves.
                    scope.stack_pointer += 0;
                    block!().extend(Instruction::Index)
                }
                Node::Struct(structure) => {
                    try_validate(structure.diagnostics)?;
                    self.add_expression(block_id, structure.inner, scope)?;
                    // note that only one value (the inner value's type) exists on the current scope;
                    // this will be important later.
                    let mut child = scope.child();
                    let statics = if let Some(members) = structure.members {
                        for i in members.statements {
                            self.add_statement(block_id, i, &mut child)?;
                        }
                        match members.result {
                            // Eventually this value will be the enum's methods. Force unit for now.
                            BlockResult::Expression(expression) => {
                                self.add_expression(block_id, expression, &mut child)?;
                            }
                            _ => return Err(Error::UnexpectedEnumResult),
                        }
                        let set = child
                            .bindings
                            .into_iter()
                            .rev()
                            .map(|(case, _)| self.create_string(case))
                            .collect::<Result<Vec<_>, Error<'source>>>()?;
                        if set.is_empty() {
                            0
                        } else {
                            self.string_sets.push(set);
                            // this happens after because string sets start at 1;
                            // 0 is an empty set!
                            self.string_sets.len() as StringSet
                        }
                    } else {
                        // missing methods should be treated as none (unit) (obviously)
                        self.blocks[block_id as usize].extend(Instruction::PushUnit);
                        0
                    };
                    // at this point, the stack has grown by 2 + statics.
                    // however only the variants (bottom of the stack) are accounted for in our scope,
                    // and the resulting struct will replace it.
                    scope.stack_pointer += 0;
                    self.blocks[block_id as usize].extend(Instruction::PushStruct { statics });
                }
                Node::Enum(enumeration) => {
                    try_validate(enumeration.diagnostics)?;
                    self.add_expression(block_id, enumeration.variants, scope)?;
                    // note that only one value (the variants) exists on the current scope;
                    // this will be important later.
                    let mut child = scope.child();
                    let statics = if let Some(members) = enumeration.members {
                        for i in members.statements {
                            self.add_statement(block_id, i, &mut child)?;
                        }
                        match members.result {
                            // Eventually this value will be the enum's methods. Force unit for now.
                            BlockResult::Expression(expression) => {
                                self.add_expression(block_id, expression, &mut child)?;
                            }
                            _ => return Err(Error::UnexpectedEnumResult),
                        }
                        let set = child
                            .bindings
                            .into_iter()
                            .rev()
                            .map(|(case, _)| self.create_string(case))
                            .collect::<Result<Vec<_>, Error<'source>>>()?;
                        if set.is_empty() {
                            0
                        } else {
                            self.string_sets.push(set);
                            // this happens after because string sets start at 1;
                            // 0 is an empty set!
                            self.string_sets.len() as StringSet
                        }
                    } else {
                        // missing methods should be treated as none (unit) (obviously)
                        self.blocks[block_id as usize].extend(Instruction::PushUnit);
                        0
                    };
                    // at this point, the stack has grown by 2 + statics.
                    // however only the variants (bottom of the stack) are accounted for in our scope,
                    // and the resulting enum will replace it.
                    scope.stack_pointer += 0;
                    self.blocks[block_id as usize].extend(Instruction::PushEnum { statics });
                }
                Node::Match(_) => todo!(),
            };
        }
        Ok(())
    }
}

impl<'source> TryFrom<Block<'source>> for Program<'source> {
    type Error = Error<'source>;
    fn try_from(block: Block<'source>) -> Result<Self, Self::Error> {
        let mut this = Self::default();
        let block_id = this.create_block()?;
        this.add_block(block_id, block, Scope::default())?;
        Ok(this)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Value {
    index: StackPointer,
}

#[derive(Default)]
pub struct Scope<'parent, 'source> {
    parent: Option<&'parent Scope<'parent, 'source>>,

    stack_pointer: StackPointer,
    bindings: Vec<(&'source str, Value)>,

    name: Option<&'source str>,
    /// If true, this block is a candidate for unnamed break statements.
    ///
    /// This is used to make "for { if { break } }" break out of the for block and not just the if.
    implicit_break: bool,
    /// Indexes of 32-bit addresses that are intended to reference the end of this scope.
    ///
    /// Used to implement `break`.
    break_requests: Cell<Vec<ProgramCounter>>,
}

impl<'parent, 'source> Scope<'parent, 'source> {
    fn get(&self, k: &'source str) -> Option<Value> {
        self.bindings
            .iter()
            .find(|(x, _)| *x == k)
            .copied()
            .map(|(_, x)| x)
            .or_else(|| {
                self.parent
                    .and_then(|parent| parent.get(k))
                    .or_else(|| builtins::from_str(k).map(|index| Value { index }))
            })
    }

    fn insert(&mut self, k: &'source str) {
        self.bindings.push((
            k,
            Value {
                index: self
                    .stack_pointer
                    .checked_sub(1)
                    .expect("attempted to assign variable while stack was empty"),
            },
        ));
    }

    fn request_break(&self, at: ProgramCounter) {
        let mut break_requests = self.break_requests.take();
        break_requests.push(at);
        self.break_requests.set(break_requests);
    }

    fn resolve_breaks(self, program: &mut [u8]) {
        let to = program.len() as ProgramCounter;
        for break_request in self.break_requests.take().into_iter().map(|x| x as usize) {
            program[break_request..(break_request + size_of::<u32>())]
                .copy_from_slice(&to.to_le_bytes());
        }
    }

    fn child(&'parent self) -> Self {
        Self {
            parent: Some(self),
            stack_pointer: self.stack_pointer,
            ..Default::default()
        }
    }

    /// Removes all variable bindings from this scope
    /// and creates a new scope which begins with them.
    fn promote(&mut self) -> Self {
        let lost_size = self.parent.map_or(0, |x| x.stack_pointer);
        let mut new_bindings = Vec::new();
        mem::swap(&mut self.bindings, &mut new_bindings);
        for binding in &mut new_bindings {
            binding.1.index -= lost_size;
        }
        Self {
            parent: None,
            stack_pointer: self.stack_pointer - lost_size,
            bindings: new_bindings,
            ..Default::default()
        }
    }

    fn named(self, s: &'source str) -> Self {
        Self {
            name: Some(s),
            ..self
        }
    }

    fn implicit_break(self) -> Self {
        Self {
            implicit_break: true,
            ..self
        }
    }
}
