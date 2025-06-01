#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Token<'source> {
    // Keywords
    Let,

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

    // Values
    Ident(&'source str),
    Number(&'source str),
}

pub struct Lexer<'source> {
    cursor: &'source str,
}

impl<'source> From<&'source str> for Lexer<'source> {
    fn from(source: &'source str) -> Self {
        Self { cursor: source }
    }
}

/// These methods combine the `Chars` iterator and the `Peekable` trait without making the cursor innaccessible.
/// `Peekable<Chars>` renders `Chars`'s `as_str` inaccessible,
/// and since `peek` relies on buffering the result of `next` the resulting slice would be useless anyways.
impl<'source> Lexer<'source> {
    fn next(&mut self) -> Option<char> {
        self.cursor.chars().next().inspect(|_| {
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
        let root = self.cursor;
        match self.next()? {
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
                    | "char" | "class" | "const" | "continue" | "do" | "dyn" | "else" | "end"
                    | "enum" | "false" | "fixed" | "float" | "fn" | "for" | "if" | "impl"
                    | "import" | "in" | "include" | "integer" | "iterator" | "loop" | "macro"
                    | "match" | "mod" | "move" | "mut" | "never" | "or" | "priv" | "pub"
                    | "ref" | "require" | "return" | "safe" | "self" | "Self" | "static"
                    | "string" | "struct" | "super" | "switch" | "table" | "trait" | "true"
                    | "try" | "tuple" | "type" | "union" | "unit" | "unsafe" | "unsigned"
                    | "use" | "where" | "while" | "yield" => {
                        panic!("the symbol {ident} is reserved")
                    }
                    "let" => Some(Token::Let),
                    _ => Some(Token::Ident(ident)),
                }
            }
            // Number
            '0'..='9' => {
                let mut length = 1;
                while self.next_if(|c| matches!(c, '0'..='9' | '.')).is_some() {
                    length += 1;
                }
                Some(Token::Number(&root[0..length]))
            }
            '=' if self.next_if(|c| c == '=').is_some() => Some(Token::EqualTo),
            '=' if self.next_if(|c| c == '>').is_some() => Some(Token::DoubleArrow),
            '=' => Some(Token::Equals),
            '!' if self.next_if(|c| c == '=').is_some() => Some(Token::NotEqualTo),
            '!' => Some(Token::Not),
            '>' if self.next_if(|c| c == '=').is_some() => Some(Token::GreaterEqual),
            '>' => Some(Token::Greater),
            '<' if self.next_if(|c| c == '=').is_some() => Some(Token::LesserEqual),
            '<' => Some(Token::Lesser),
            '+' => Some(Token::Plus),
            '-' if self.next_if(|c| c == '>').is_some() => Some(Token::SingleArrow),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Star),
            '/' => Some(Token::Slash),
            '.' => {
                if self.next_if(|c| c == '.').is_some() {
                    if self.next_if(|c| c == '=').is_some() {
                        Some(Token::RangeInclusive)
                    } else if self.next_if(|c| c == '.').is_some() {
                        Some(Token::Ellipses)
                    } else {
                        Some(Token::RangeExclusive)
                    }
                } else {
                    Some(Token::Dot)
                }
            }
            ',' => Some(Token::Comma),
            '|' => Some(Token::Pipe),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '[' => Some(Token::OpenSquare),
            ']' => Some(Token::CloseSquare),
            '{' => Some(Token::OpenBrace),
            '}' => Some(Token::CloseBrace),
            ':' => Some(Token::Colon),
            ';' => Some(Token::Semicolon),
            c => panic!("unexpected character: {c}"),
        }
    }
}
