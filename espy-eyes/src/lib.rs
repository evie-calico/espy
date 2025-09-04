//! The lexer on its own is merely an iterator of tokens.
//! Use espy-ears to parse espy.
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
    As,
    Break,
    Discard,
    Else,
    End,
    Enum,
    False,
    For,
    If,
    In,
    Let,
    Match,
    Or,
    Return,
    Set,
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
    String,
}

/// A unit of espy source code.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Token<'source> {
    /// Tracks the string slice that this token originated from.
    ///
    /// Pointer arithmetic may be used to derive the range of the source string that the represents;
    /// use the `origin_range` function for this purpose.
    // TODO: An AST's memory footprint could be reduced substantially by shrinking this field. (u32s = half, u32 with u16 len = one third)
    pub origin: &'source str,
    /// The semantic meaning of the token.
    ///
    /// This is usually called the token's "type",
    /// but "lexigram" is used to avoid conflict with Rust's `type` keyword.
    pub lexigram: Lexigram,
}

impl Token<'_> {
    pub fn resolve(&self) -> Result<'static, String, EscapeError> {
        fn resolve_escape(chars: &mut std::str::Chars) -> Result<'static, char, EscapeError> {
            let escaped = match chars
                .next()
                .expect("lexer should always produce a character after a backslash")
            {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '0' => '\0',
                // used by strings
                '"' => '"',
                // used by raw identifiers
                '`' => '`',
                // unused, but available
                '\'' => '\'',
                'u' => {
                    let Some('{') = chars.next() else {
                        return Err(EscapeError::MissingOpenBrace);
                    };
                    let Some((codepoint, _)) = chars.as_str().split_once('}') else {
                        return Err(EscapeError::MissingCloseBrace);
                    };
                    if codepoint.len() > 6 || codepoint.contains(|c: char| !c.is_ascii_hexdigit()) {
                        return Err(EscapeError::InvalidLiteral);
                    }
                    let Some(codepoint) = u32::from_str_radix(codepoint, 16)
                        .ok()
                        .and_then(char::from_u32)
                    else {
                        return Err(EscapeError::InvalidLiteral);
                    };
                    codepoint
                }
                _ => return Err(EscapeError::InvalidEscape),
            };
            Ok(escaped)
        }

        match self.lexigram {
            Lexigram::String => {
                let mut s = String::new();
                // trim quotes
                let mut chars = self.origin[1..(self.origin.len() - 1)].chars();
                while let Some(c) = chars.next() {
                    if c == '\\' {
                        s.push(resolve_escape(&mut chars)?);
                    } else {
                        s.push(c);
                    }
                }
                Ok(s)
            }
            Lexigram::Ident if self.origin.starts_with('`') => {
                let mut s = String::new();
                // trim quotes
                let mut chars = self.origin[1..(self.origin.len() - 1)].chars();
                while let Some(c) = chars.next() {
                    if c == '\\' {
                        s.push(resolve_escape(&mut chars)?);
                    } else {
                        s.push(c);
                    }
                }
                Ok(s)
            }
            _ => Ok(self.origin.into()),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum EscapeError {
    MissingOpenBrace,
    MissingCloseBrace,
    InvalidLiteral,
    InvalidEscape,
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
    /// A quote character was encountered but never terminated.
    UnterminatedString,
    /// A backtick character was encountered but never terminated.
    UnterminatedIdentifier,
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
        while let Some(skipped) = self.next_if(|c| matches!(c, ' ' | '\n' | '\r' | '\t' | '#')) {
            if skipped == '#' {
                while self.next_if(|c| c != '\n').is_some() {}
            }
        }
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
                    "array" | "async" | "await" | "case" | "class" | "const" | "continue"
                    | "do" | "dyn" | "fn" | "impl" | "import" | "include" | "iterator" | "loop"
                    | "macro" | "mod" | "move" | "never" | "priv" | "pub" | "ref" | "require"
                    | "safe" | "static" | "struct" | "super" | "switch" | "trait" | "try"
                    | "tuple" | "type" | "union" | "unsafe" | "use" | "where" | "while"
                    | "yield" => {
                        return Some(Err(Error {
                            origin: ident,
                            kind: ErrorKind::ReservedSymbol,
                        }));
                    }
                    "and" => Lexigram::And,
                    "as" => Lexigram::As,
                    "break" => Lexigram::Break,
                    "else" => Lexigram::Else,
                    "end" => Lexigram::End,
                    "enum" => Lexigram::Enum,
                    "false" => Lexigram::False,
                    "for" => Lexigram::For,
                    "if" => Lexigram::If,
                    "in" => Lexigram::In,
                    "let" => Lexigram::Let,
                    "match" => Lexigram::Match,
                    "or" => Lexigram::Or,
                    "return" => Lexigram::Return,
                    "set" => Lexigram::Set,
                    "then" => Lexigram::Then,
                    "true" => Lexigram::True,
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
            // String
            '"' => {
                loop {
                    match self.next() {
                        Some('\\') => {
                            self.next();
                        }
                        Some('"') => break,
                        None => {
                            return Some(Err(Error {
                                origin: root,
                                kind: ErrorKind::UnterminatedString,
                            }));
                        }
                        _ => {}
                    }
                }
                Lexigram::String
            }
            // Raw Identifier
            '`' => {
                loop {
                    match self.next() {
                        Some('\\') => {
                            self.next();
                        }
                        Some('`') => break,
                        None => {
                            return Some(Err(Error {
                                origin: root,
                                kind: ErrorKind::UnterminatedIdentifier,
                            }));
                        }
                        _ => {}
                    }
                }
                Lexigram::Ident
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
