pub fn lex(input: &str) -> Result<Vec<Token>, char> {
    Lexer {
        chars: input.chars().collect(),
        pos: 0,
    }
    .lex()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Number(String),
    BinOp(BinOp),
    Equals,
    OpenParen,
    CloseParen,
    Comma,
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
    pos: usize,
}

impl Lexer {
    fn next_char_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let c = self.chars.get(self.pos).copied().filter(|c| f(*c))?;
        self.pos += 1;
        Some(c)
    }
    fn next_char_exact(&mut self, c: char) -> bool {
        self.next_char_if(|c2| c == c2).is_some()
    }
    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }
    fn lex(mut self) -> Result<Vec<Token>, char> {
        let mut tokens = Vec::new();
        while let Some(c) = self.next_char() {
            match c {
                '=' => tokens.push(Token::Equals),
                ',' => tokens.push(Token::Comma),
                '(' => tokens.push(Token::OpenParen),
                ')' => tokens.push(Token::CloseParen),
                '+' => tokens.push(Token::BinOp(BinOp::Add)),
                '*' => tokens.push(Token::BinOp(BinOp::Mul)),
                '/' => tokens.push(Token::BinOp(BinOp::Div)),
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
                    tokens.push(if num == "-" {
                        Token::BinOp(BinOp::Sub)
                    } else {
                        Token::Number(num)
                    })
                }
                c if c.is_alphabetic() || c == '_' => {
                    let mut ident = c.to_string();
                    while let Some(c) = self.next_char_if(|c| c.is_alphanumeric() || c == '_') {
                        ident.push(c);
                    }
                    tokens.push(Token::Ident(ident));
                }
                c if c.is_whitespace() => {}
                c => return Err(c),
            }
        }
        Ok(tokens)
    }
}
