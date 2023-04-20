use std::fmt;

pub fn lex(input: &str) -> Result<Vec<Sp<Token>>, Sp<char>> {
    Lexer {
        chars: input.chars().collect(),
        loc: Loc {
            line: 1,
            col: 1,
            pos: 0,
        },
    }
    .lex()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
    pub pos: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Default for Span {
    fn default() -> Self {
        Span {
            start: Loc {
                line: 1,
                col: 1,
                pos: 0,
            },
            end: Loc {
                line: 1,
                col: 1,
                pos: 0,
            },
        }
    }
}

impl Span {
    pub fn sp<T>(self, value: T) -> Sp<T> {
        Sp { value, span: self }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Sp<T> {
    pub value: T,
    pub span: Span,
}
impl<T: fmt::Display> fmt::Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span.start, self.value)
    }
}

impl<T> Sp<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Sp<U> {
        Sp {
            value: f(self.value),
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Number(String),
    BinOp(BinOp),
    Dollar,
    Equals,
    OpenParen,
    CloseParen,
    Comma,
    Newline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

struct Lexer {
    chars: Vec<char>,
    loc: Loc,
}

impl Lexer {
    fn next_char_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let c = self.chars.get(self.loc.pos).copied().filter(|c| f(*c))?;
        match c {
            '\n' => {
                self.loc.line += 1;
                self.loc.col = 1;
            }
            '\r' => {}
            _ => self.loc.col += 1,
        }
        self.loc.pos += 1;
        Some(c)
    }
    fn next_char_exact(&mut self, c: char) -> bool {
        self.next_char_if(|c2| c == c2).is_some()
    }
    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }
    fn end(&self, start: Loc, token: Token) -> Sp<Token> {
        Span {
            start,
            end: self.loc,
        }
        .sp(token)
    }
    fn lex(mut self) -> Result<Vec<Sp<Token>>, Sp<char>> {
        let mut tokens = Vec::new();
        loop {
            let start = self.loc;
            if let Some(c) = self.next_char() {
                match c {
                    '=' => tokens.push(self.end(start, Token::Equals)),
                    ',' => tokens.push(self.end(start, Token::Comma)),
                    '(' => tokens.push(self.end(start, Token::OpenParen)),
                    ')' => tokens.push(self.end(start, Token::CloseParen)),
                    '\n' => tokens.push(self.end(start, Token::Newline)),
                    '$' => tokens.push(self.end(start, Token::Dollar)),
                    '+' => tokens.push(self.end(start, Token::BinOp(BinOp::Add))),
                    '*' => tokens.push(self.end(start, Token::BinOp(BinOp::Mul))),
                    '/' => tokens.push(self.end(start, Token::BinOp(BinOp::Div))),
                    c if c.is_ascii_digit() || c == '-' => {
                        let mut num = c.to_string();
                        while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
                            num.push(c);
                        }
                        if self.next_char_exact('.') {
                            num.push('.');
                            while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
                                num.push(c);
                            }
                        }
                        tokens.push(self.end(
                            start,
                            if num == "-" {
                                Token::BinOp(BinOp::Sub)
                            } else {
                                Token::Number(num)
                            },
                        ))
                    }
                    c if is_ident_char(c) => {
                        let mut ident = c.to_string();
                        while let Some(c) =
                            self.next_char_if(|c| is_ident_char(c) || c.is_ascii_digit())
                        {
                            ident.push(c);
                        }
                        tokens.push(self.end(start, Token::Ident(ident)));
                    }
                    c if c.is_whitespace() => {}
                    c => {
                        return Err(Span {
                            start,
                            end: self.loc,
                        }
                        .sp(c))
                    }
                }
            } else {
                break;
            }
        }
        Ok(tokens)
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphabetic() || c as u32 > 127 || "_#".contains(c)
}
