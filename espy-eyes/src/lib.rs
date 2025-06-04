#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Token<'source> {
    pub start: usize,
    pub end: usize,
    pub ty: TokenType<'source>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenType<'source> {
    // Keywords
    Let,
    If,
    Else,
    End,
    For,
    Then,

    // Symbols
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
        self.cursor.chars().next().inspect(|_| {
            self.index += 1;
            self.cursor = &self.cursor[1..];
        })
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
    type Item = Token<'source>;
    fn next(&mut self) -> Option<Self::Item> {
        while self
            .next_if(|c| matches!(c, ' ' | '\n' | '\r' | '\t'))
            .is_some()
        {}
        let start = self.index;
        let root = self.cursor;
        let ty = match self.next()? {
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
                    "and" | "any" | "array" | "as" | "async" | "await" | "break" | "case"
                    | "char" | "class" | "const" | "continue" | "do" | "dyn" | "enum" | "false"
                    | "fixed" | "float" | "fn" | "impl" | "import" | "in" | "include"
                    | "integer" | "iterator" | "loop" | "macro" | "match" | "mod" | "move"
                    | "mut" | "never" | "or" | "priv" | "pub" | "ref" | "require" | "return"
                    | "safe" | "self" | "Self" | "static" | "string" | "struct" | "super"
                    | "switch" | "table" | "trait" | "true" | "try" | "tuple" | "type"
                    | "union" | "unit" | "unsafe" | "unsigned" | "use" | "where" | "while"
                    | "yield" => {
                        panic!("the symbol {ident} is reserved")
                    }
                    "for" => TokenType::For,
                    "else" => TokenType::Else,
                    "end" => TokenType::End,
                    "if" => TokenType::If,
                    "let" => TokenType::Let,
                    "then" => TokenType::Then,
                    _ => TokenType::Ident(ident),
                }
            }
            // Number
            '0'..='9' => {
                let mut length = 1;
                while self.next_if(|c| matches!(c, '0'..='9' | '.')).is_some() {
                    length += 1;
                }
                TokenType::Number(&root[0..length])
            }
            '=' if self.next_if(|c| c == '=').is_some() => TokenType::EqualTo,
            '=' if self.next_if(|c| c == '>').is_some() => TokenType::DoubleArrow,
            '=' => TokenType::Equals,
            '!' if self.next_if(|c| c == '=').is_some() => TokenType::NotEqualTo,
            '!' => TokenType::Not,
            '>' if self.next_if(|c| c == '=').is_some() => TokenType::GreaterEqual,
            '>' => TokenType::Greater,
            '<' if self.next_if(|c| c == '=').is_some() => TokenType::LesserEqual,
            '<' => TokenType::Lesser,
            '+' => TokenType::Plus,
            '-' if self.next_if(|c| c == '>').is_some() => TokenType::SingleArrow,
            '-' => TokenType::Minus,
            '*' => TokenType::Star,
            '/' => TokenType::Slash,
            '.' => {
                if self.next_if(|c| c == '.').is_some() {
                    if self.next_if(|c| c == '=').is_some() {
                        TokenType::RangeInclusive
                    } else if self.next_if(|c| c == '.').is_some() {
                        TokenType::Ellipses
                    } else {
                        TokenType::RangeExclusive
                    }
                } else {
                    TokenType::Dot
                }
            }
            ',' => TokenType::Comma,
            '|' if self.next_if(|c| c == '>').is_some() => TokenType::Triangle,
            '|' => TokenType::Pipe,
            '(' => TokenType::OpenParen,
            ')' => TokenType::CloseParen,
            '[' => TokenType::OpenSquare,
            ']' => TokenType::CloseSquare,
            '{' => TokenType::OpenBrace,
            '}' => TokenType::CloseBrace,
            ':' => TokenType::Colon,
            ';' => TokenType::Semicolon,
            c => panic!("unexpected character: {c}"),
        };
        Some(Token {
            start,
            end: self.index,
            ty,
        })
    }
}
