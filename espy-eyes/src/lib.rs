#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Token<'source> {
    pub lexigram: Lexigram<'source>,
    pub start: usize,
    pub end: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Lexigram<'source> {
    // Keywords
    And,
    Break,
    Let,
    If,
    In,
    Else,
    End,
    For,
    Or,
    Then,
    Discard,

    // Symbols
    Ampersand,
    Caret,
    CloseBrace,
    CloseParen,
    CloseSquare,
    Colon,
    Comma,
    Dot,
    DoubleArrow,
    Ellipses,
    Equals,
    EqualTo,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    Minus,
    Not,
    NotEqualTo,
    OpenBrace,
    OpenParen,
    OpenSquare,
    Pipe,
    Plus,
    RangeExclusive,
    RangeInclusive,
    Semicolon,
    SingleArrow,
    Slash,
    Star,
    Triangle,

    // Values
    Ident(&'source str),
    Number(&'source str),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind<'source> {
    UnexpectedCharacter(char),
    ReservedSymbol(&'source str),
}

impl<'source> ErrorKind<'source> {
    fn at(self, start: usize, end: usize) -> Error<'source> {
        Error {
            kind: self,
            start,
            end,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Error<'source> {
    pub kind: ErrorKind<'source>,
    pub start: usize,
    pub end: usize,
}

pub type Result<'source, T = Token<'source>, E = Error<'source>> = std::result::Result<T, E>;

#[derive(Copy, Clone, Default)]
pub struct Lexer<'source> {
    cursor: &'source str,
    index: usize,
}

impl<'source> From<&'source str> for Lexer<'source> {
    fn from(source: &'source str) -> Self {
        Self {
            cursor: source,
            index: 0,
        }
    }
}

/// These methods combine the `Chars` iterator and the `Peekable` trait without making the cursor innaccessible.
/// `Peekable<Chars>` renders `Chars`'s `as_str` inaccessible,
/// and since `peek` relies on buffering the result of `next` the resulting slice would be useless anyways.
impl<'source> Lexer<'source> {
    fn next(&mut self) -> Option<char> {
        let mut chars = self.cursor.char_indices();
        let (_, c) = chars.next()?;
        let offset = chars.next().map_or(self.cursor.len(), |(x, _)| x);
        self.index += offset;
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
        let start = self.index;
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
                    | "const" | "continue" | "do" | "dyn" | "enum" | "false" | "fixed"
                    | "float" | "fn" | "impl" | "import" | "include" | "integer" | "iterator"
                    | "loop" | "macro" | "match" | "mod" | "move" | "mut" | "never" | "priv"
                    | "pub" | "ref" | "require" | "return" | "safe" | "self" | "Self"
                    | "static" | "string" | "struct" | "super" | "switch" | "table" | "trait"
                    | "true" | "try" | "tuple" | "type" | "union" | "unit" | "unsafe"
                    | "unsigned" | "use" | "where" | "while" | "yield" => {
                        return Some(Err(ErrorKind::ReservedSymbol(ident).at(start, self.index)));
                    }
                    "and" => Lexigram::And,
                    "break" => Lexigram::Break,
                    "for" => Lexigram::For,
                    "else" => Lexigram::Else,
                    "end" => Lexigram::End,
                    "if" => Lexigram::If,
                    "in" => Lexigram::In,
                    "let" => Lexigram::Let,
                    "or" => Lexigram::Or,
                    "then" => Lexigram::Then,
                    "_" => Lexigram::Discard,
                    _ => Lexigram::Ident(ident),
                }
            }
            // Number
            '0'..='9' => {
                let mut length = 1;
                while self.next_if(|c| matches!(c, '0'..='9' | '.')).is_some() {
                    length += 1;
                }
                Lexigram::Number(&root[0..length])
            }
            '=' if self.next_if(|c| c == '=').is_some() => Lexigram::EqualTo,
            '=' if self.next_if(|c| c == '>').is_some() => Lexigram::DoubleArrow,
            '=' => Lexigram::Equals,
            '!' if self.next_if(|c| c == '=').is_some() => Lexigram::NotEqualTo,
            '!' => Lexigram::Not,
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
                        Lexigram::RangeInclusive
                    } else if self.next_if(|c| c == '.').is_some() {
                        Lexigram::Ellipses
                    } else {
                        Lexigram::RangeExclusive
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
            c => {
                return Some(Err(ErrorKind::UnexpectedCharacter(c).at(start, self.index)));
            }
        };
        Some(Ok(Token {
            start,
            end: self.index,
            lexigram,
        }))
    }
}
