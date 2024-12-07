use crate::lexer::{Position, Token};
use std::fmt::Display;

#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Token),
    InvalidAssignmentTarget(Position),
    UnexpectedEof,
    DivisionByZero,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            Error::InvalidAssignmentTarget(position) => {
                write!(f, "Invalid assignment target at {}", position)
            }
            Error::UnexpectedEof => write!(f, "Unexpected end of file"),
            Error::DivisionByZero => write!(f, "Division by zero"),
        }
    }
}
