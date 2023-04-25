use std::fmt;

use crate::{ast::*, lex::*};

#[derive(Debug)]
pub enum ParseError {
    InvalidCharacter(char),
    Expected {
        expectation: String,
        got: Option<Token>,
    },
    InvalidNumber(String),
    UnknownKey(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::InvalidCharacter(c) => write!(f, "Invalid character: {}", c),
            ParseError::Expected {
                expectation,
                got: Some(got),
            } => write!(f, "Expected {expectation}, got {got}"),
            ParseError::Expected {
                expectation,
                got: None,
            } => write!(f, "Expected {expectation}, got end of file"),
            ParseError::InvalidNumber(s) => write!(f, "Invalid number: {}", s),
            ParseError::UnknownKey(s) => write!(f, "Unknown key: {s}"),
        }
    }
}

pub type ParseResult<T = ()> = Result<T, Sp<ParseError>>;

pub struct Parser {
    tokens: Vec<Sp<Token>>,
    curr: usize,
}

pub fn parse(input: &str) -> ParseResult<File> {
    let tokens = lex(input).map_err(|e| e.map(ParseError::InvalidCharacter))?;
    let mut parser = Parser { tokens, curr: 0 };
    let mut items = Vec::new();
    while let Some(item) = parser.item()? {
        items.push(item);
    }
    let mut sheet = None;
    if let Some(sh) = parser.sheet()? {
        sheet = Some(sh);
    }
    if parser.curr < parser.tokens.len() {
        return Err(parser.expected("end of file"));
    }
    Ok(File { items, sheet })
}

impl Parser {
    fn next_token_map<T>(&mut self, f: impl FnOnce(Token) -> Option<T>) -> Option<Sp<T>> {
        let token = self
            .tokens
            .get(self.curr)
            .cloned()
            .and_then(|sp| f(sp.value).map(|val| sp.span.sp(val)))?;
        self.curr += 1;
        Some(token)
    }
    fn try_exact(&mut self, token: impl Into<Token>) -> Option<Span> {
        self.next_token_map(|t| if t == token.into() { Some(()) } else { None })
            .map(|sp| sp.span)
    }
    fn expect(&mut self, token: impl Into<Token>) -> ParseResult {
        let token = token.into();
        if self.try_exact(token.clone()).is_some() {
            Ok(())
        } else {
            Err(self.expected(token.to_string()))
        }
    }
    fn expected(&self, expectation: impl Into<String>) -> Sp<ParseError> {
        if let Some(token) = self
            .tokens
            .get(self.curr.saturating_sub(1))
            .or_else(|| self.tokens.last())
        {
            token.span.sp(ParseError::Expected {
                expectation: expectation.into(),
                got: Some(token.value.clone()),
            })
        } else {
            Span::default().sp(ParseError::Expected {
                expectation: expectation.into(),
                got: None,
            })
        }
    }
    fn item(&mut self) -> ParseResult<Option<Item>> {
        Ok(Some(if self.try_exact("sequence").is_some() {
            let name = self.ident().ok_or_else(|| self.expected("name"))?;
            let sequence = self
                .sequence_value()?
                .ok_or_else(|| self.expected("sequence"))?;
            Item::Sequence(name, sequence)
        } else if let Some(track) = self.track()? {
            Item::Track(track)
        } else {
            return Ok(None);
        }))
    }
    fn sheet(&mut self) -> ParseResult<Option<Sheet>> {
        let Some(name) = self.ident() else {
            return Ok(None);
        };
        let body = if self.try_exact('{').is_some() {
            let mut children = Vec::new();
            while let Some(sheet) = self.sheet()? {
                children.push(sheet);
            }
            self.expect('}')?;
            Some(SheetBody { children })
        } else {
            None
        };
        Ok(Some(Sheet {
            name: name.value,
            body,
        }))
    }
    fn track(&mut self) -> ParseResult<Option<Track>> {
        if self.try_exact("track").is_none() {
            return Ok(None);
        }
        self.expect('(')?;
        let mut sound = "sine".to_string();
        let mut perbeat = 1.0;
        let mut volume = 0.3;
        while let Some(key) = self.ident() {
            match key.value.as_str() {
                "sound" => {
                    self.expect(':')?;
                    sound = self
                        .ident()
                        .ok_or_else(|| self.expected("sound name"))?
                        .value
                }
                "perbeat" => {
                    self.expect(':')?;
                    perbeat = self.number()?.ok_or_else(|| self.expected("number"))?.value
                }
                "volume" => {
                    self.expect(':')?;
                    volume = self.number()?.ok_or_else(|| self.expected("number"))?.value
                }
                _ => return Err(key.span.sp(ParseError::UnknownKey(key.value))),
            }
            if self.try_exact(',').is_none() {
                break;
            }
        }
        self.expect(')')?;
        self.expect('{')?;
        let selectors = self.selectors()?;
        self.expect('}')?;
        Ok(Some(Track {
            sound,
            perbeat,
            volume,
            selectors,
        }))
    }
    fn selectors(&mut self) -> ParseResult<Selectors> {
        let mut selectors = Selectors::new();
        while let Some(name) = self.ident() {
            if self.try_exact('{').is_some() {
                let sub = self.selectors()?;
                selectors.insert(name.value, SelectorValue::Selectors(sub));
                self.expect('}')?;
            } else if let Some(seq) = self.sequence()? {
                selectors.insert(name.value, SelectorValue::Sequence(seq));
            } else {
                return Err(self.expected("{ or ("));
            }
        }
        Ok(selectors)
    }
    fn sequence(&mut self) -> ParseResult<Option<Sequence>> {
        let Some(value) = self.sequence_value()? else {
            return Ok(None);
        };
        let indices = if self.try_exact(':').is_some() {
            Some(
                self.sequence_value()?
                    .ok_or_else(|| self.expected("indices"))?,
            )
        } else {
            None
        };
        Ok(Some(Sequence { value, indices }))
    }
    fn sequence_value(&mut self) -> ParseResult<Option<SequenceValue>> {
        Ok(Some(if let Some(ident) = self.ident() {
            SequenceValue::Ident(ident)
        } else if self.try_exact('[').is_some() {
            let mut values = Vec::new();
            while let Some(value) = self.number_value()? {
                values.push(value);
            }
            self.expect(']')?;
            SequenceValue::List(values)
        } else {
            return Ok(None);
        }))
    }
    fn number_value(&mut self) -> ParseResult<Option<Sp<NumberValue>>> {
        Ok(Some(if let Some(num) = self.number()? {
            num.map(NumberValue::Number)
        } else if let Some(ident) = self.ident() {
            ident.map(NumberValue::Ident)
        } else {
            return Ok(None);
        }))
    }
    fn ident(&mut self) -> Option<Sp<String>> {
        self.next_token_map(|token| match token {
            Token::Ident(ident) => Some(ident),
            _ => None,
        })
    }
    fn number(&mut self) -> ParseResult<Option<Sp<f64>>> {
        self.next_token_map(|token| match token {
            Token::Number(num) => Some(num),
            _ => None,
        })
        .map(|num| {
            num.value
                .parse()
                .map(|n| num.span.sp(n))
                .map_err(|_| num.span.sp(ParseError::InvalidNumber(num.value)))
        })
        .transpose()
    }
}
