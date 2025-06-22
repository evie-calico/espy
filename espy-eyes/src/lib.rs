//! The lexer on its own is merely an iterator of tokens.
//! Use espy-ears to parse espyscript.
//!
//! See [`Lexigram`] for a complete list of token types.
//!
//! ```rust
//! use espy_eyes::Lexer;
//!
//! let lexer = Lexer::from("1 + 2");
//!
//! for token in lexer {
//!     println!("{token:?}");
//! }
//! ```

/// The semantic meaning of a token.
///
/// This is usually called the "token type",
/// but "lexigram" is used to avoid conflict with Rust's `type` keyword.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Lexigram {
    // Keywords
    And,
    Break,
    Discard,
    Else,
    End,
    Enum,
    False,
    For,
    If,
    Impl,
    In,
    Let,
    Match,
    Or,
    Struct,
    Then,
    True,
    With,

    // Symbols
    Ampersand,
    Bang,
    BangEqual,
    Caret,
    CloseBrace,
    CloseParen,
    CloseSquare,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotDotEqual,
    DoubleArrow,
    DoubleEqual,
    Ellipses,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    Minus,
    OpenBrace,
    OpenParen,
    OpenSquare,
    Pipe,
    Plus,
    Semicolon,
    SingleArrow,
    SingleEqual,
    Slash,
    Star,
    Triangle,

    // Values
    Ident,
    Number,
}

/// A unit of espyscript source code.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Token<'source> {
    /// Tracks the string slice that this token originated from.
    ///
    /// Pointer arithmetic may be used to derive the range of the source string that the represents;
    /// use the `origin_range` function for this purpose.
    pub origin: &'source str,
    /// The semantic meaning of the token.
    ///
    /// This is usually called the token's "type",
    /// but "lexigram" is used to avoid conflict with Rust's `type` keyword.
    pub lexigram: Lexigram,
}

impl Token<'_> {
    /// # Panics
    ///
    /// Panics if provided a string slice that does not contain the token's `origin`.
    pub fn origin_range(&self, source: &str) -> (usize, usize) {
        let start = self.origin.as_ptr() as isize - source.as_ptr() as isize;
        let end = start + self.origin.len() as isize;
        if start < 0 || end - start > source.len() as isize {
            panic!("source string does not contain token origin");
        }
        (start as usize, end as usize)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    /// A character with no meaning to the lexer.
    ///
    /// This is mostly ASCII control characters,
    /// unused symbols,
    /// and Unicode codepoints outside of the ASCII range.
    UnexpectedCharacter,
    /// An otherwise-valid identifier that is reserved for future keywords.
    ///
    /// Parsers are free to reinterpret this as an identifier for diagnostic purposes.
    ReservedSymbol,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Error<'source> {
    pub origin: &'source str,
    pub kind: ErrorKind,
}

pub type Result<'source, T = Token<'source>, E = Error<'source>> = std::result::Result<T, E>;

/// An iterator that produces tokens.
///
/// This should be made [`Peekable`](std::iter::Peekable) using the [`Iterator::peekable`] function prior to parsing.
#[derive(Copy, Clone, Default)]
pub struct Lexer<'source> {
    cursor: &'source str,
}

impl<'source> From<&'source str> for Lexer<'source> {
    fn from(source: &'source str) -> Self {
        Self { cursor: source }
    }
}

// These methods combine the `Chars` iterator and the `Peekable` trait without making the cursor innaccessible.
// `Peekable<Chars>` renders `Chars`'s `as_str` inaccessible,
// and since `peek` relies on buffering the result of `next` the resulting slice would be useless anyways.
impl<'source> Lexer<'source> {
    fn next(&mut self) -> Option<char> {
        let mut chars = self.cursor.char_indices();
        let (_, c) = chars.next()?;
        let offset = chars.next().map_or(self.cursor.len(), |(x, _)| x);
        self.cursor = &self.cursor[offset..];
        Some(c)
    }

    fn next_if(&mut self, cond: impl FnOnce(char) -> bool) -> Option<char> {
        if cond(self.peek()?) {
            self.next()
        } else {
            None
        }
    }

    fn peek(&self) -> Option<char> {
        self.cursor.chars().next()
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Result<'source>;
    fn next(&mut self) -> Option<Self::Item> {
        while self
            .next_if(|c| matches!(c, ' ' | '\n' | '\r' | '\t'))
            .is_some()
        {}
        let root = self.cursor;
        let lexigram = match self.next()? {
            // Ident
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut length = 1;
                while self
                    .next_if(|c| {
                        matches!(
                            c,
                            'A'..='Z' | 'a'..='z' | '_' | '0'..='9'
                        )
                    })
                    .is_some()
                {
                    length += 1;
                }
                let ident = &root[0..length];
                match ident {
                    "any" | "array" | "as" | "async" | "await" | "case" | "char" | "class"
                    | "const" | "continue" | "do" | "dyn" | "fixed" | "float" | "fn" | "import"
                    | "include" | "integer" | "iterator" | "loop" | "macro" | "mod" | "move"
                    | "mut" | "never" | "priv" | "pub" | "ref" | "require" | "return" | "safe"
                    | "static" | "string" | "super" | "switch" | "table" | "trait" | "try"
                    | "tuple" | "type" | "union" | "unit" | "unsafe" | "unsigned" | "use"
                    | "where" | "while" | "yield" => {
                        return Some(Err(Error {
                            origin: ident,
                            kind: ErrorKind::ReservedSymbol,
                        }));
                    }
                    "and" => Lexigram::And,
                    "break" => Lexigram::Break,
                    "else" => Lexigram::Else,
                    "end" => Lexigram::End,
                    "enum" => Lexigram::Enum,
                    "false" => Lexigram::False,
                    "for" => Lexigram::For,
                    "if" => Lexigram::If,
                    "impl" => Lexigram::Impl,
                    "in" => Lexigram::In,
                    "let" => Lexigram::Let,
                    "match" => Lexigram::Match,
                    "or" => Lexigram::Or,
                    "struct" => Lexigram::Struct,
                    "true" => Lexigram::True,
                    "then" => Lexigram::Then,
                    "with" => Lexigram::With,
                    "_" => Lexigram::Discard,
                    _ => Lexigram::Ident,
                }
            }
            // Number
            '0'..='9' => {
                while self.next_if(|c| matches!(c, '0'..='9' | '.')).is_some() {}
                Lexigram::Number
            }
            '=' if self.next_if(|c| c == '=').is_some() => Lexigram::DoubleEqual,
            '=' if self.next_if(|c| c == '>').is_some() => Lexigram::DoubleArrow,
            '=' => Lexigram::SingleEqual,
            '!' if self.next_if(|c| c == '=').is_some() => Lexigram::BangEqual,
            '!' => Lexigram::Bang,
            '>' if self.next_if(|c| c == '=').is_some() => Lexigram::GreaterEqual,
            '>' => Lexigram::Greater,
            '<' if self.next_if(|c| c == '=').is_some() => Lexigram::LesserEqual,
            '<' => Lexigram::Lesser,
            '+' => Lexigram::Plus,
            '-' if self.next_if(|c| c == '>').is_some() => Lexigram::SingleArrow,
            '-' => Lexigram::Minus,
            '*' => Lexigram::Star,
            '/' => Lexigram::Slash,
            '.' => {
                if self.next_if(|c| c == '.').is_some() {
                    if self.next_if(|c| c == '=').is_some() {
                        Lexigram::DotDotEqual
                    } else if self.next_if(|c| c == '.').is_some() {
                        Lexigram::Ellipses
                    } else {
                        Lexigram::DotDot
                    }
                } else {
                    Lexigram::Dot
                }
            }
            ',' => Lexigram::Comma,
            '&' => Lexigram::Ampersand,
            '|' if self.next_if(|c| c == '>').is_some() => Lexigram::Triangle,
            '|' => Lexigram::Pipe,
            '^' => Lexigram::Caret,
            '(' => Lexigram::OpenParen,
            ')' => Lexigram::CloseParen,
            '[' => Lexigram::OpenSquare,
            ']' => Lexigram::CloseSquare,
            '{' => Lexigram::OpenBrace,
            '}' => Lexigram::CloseBrace,
            ':' => Lexigram::Colon,
            ';' => Lexigram::Semicolon,
            _ => {
                return Some(Err(Error {
                    origin: &root[..1],
                    kind: ErrorKind::UnexpectedCharacter,
                }));
            }
        };
        let len = self.cursor.as_ptr() as usize - root.as_ptr() as usize;
        Some(Ok(Token {
            origin: &root[..len],
            lexigram,
        }))
    }
}
