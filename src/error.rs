use crate::lexer::{Position, Token};
use crate::var_type::VarType;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnexpectedToken(Token),
    InvalidAssignmentTarget(Position),
    UnexpectedEof,
    DivisionByZero(Position),
    IoError(Position),
    VariableNotFound(String, Position),
    TypeMismatch(VarType, VarType, Position),
    VariableAlreadyExists(String, Position),
    UnknownVariableType(Position),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterError {
    DivisionByZero,
    TypeMismatch(VarType, VarType),
    UnknownVariableType,
}

pub trait AddPosition<T> {
    fn map_err_with_pos(self, p: Position) -> Result<T, Error>;
}

impl<T> AddPosition<T> for Result<T, InterpreterError> {
    fn map_err_with_pos(self, p: Position) -> Result<T, Error> {
        self.map_err(|e: InterpreterError| match e {
            InterpreterError::DivisionByZero => Error::DivisionByZero(p),
            InterpreterError::TypeMismatch(l, r) => Error::TypeMismatch(l, r, p),
            InterpreterError::UnknownVariableType => Error::UnknownVariableType(p),
        })
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            Error::InvalidAssignmentTarget(position) => {
                write!(f, "Invalid assignment target at {}", position)
            }
            Error::UnexpectedEof => write!(f, "Unexpected end of file"),
            Error::DivisionByZero(position) => write!(f, "Division by zero at {}", position),
            Error::IoError(position) => write!(f, "I/O error at {}", position),
            Error::VariableNotFound(name, position) => {
                write!(f, "Variable not found: {} at {}", name, position)
            }
            Error::TypeMismatch(expected, found, position) => write!(
                f,
                "Type mismatch: {} and {} at {}",
                expected, found, position
            ),
            Error::VariableAlreadyExists(name, position) => {
                write!(f, "Variable already exists: {} at {}", name, position)
            }
            Error::UnknownVariableType(position) => {
                write!(f, "Cannot declare variable without type at {}", position)
            }
        }
    }
}
