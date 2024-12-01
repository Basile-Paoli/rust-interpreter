use crate::lexer::{Lexer, Token};
use std::fmt::Display;
use std::iter::Peekable;

mod ast;
mod expression;

struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Token),
    UnexpectedEof,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            Error::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}
