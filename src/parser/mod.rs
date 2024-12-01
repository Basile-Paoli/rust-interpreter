use crate::lexer::{Lexer,  Token};
use std::fmt::Display;

mod ast;
mod expression;

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Token),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
        }
    }
}
