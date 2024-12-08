use crate::interpreter::Variable;
use crate::lexer::{Position, Token};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnexpectedToken(Token),
    InvalidAssignmentTarget(Position),
    UnexpectedEof,
    DivisionByZero,
    IoError,
    VariableNotFound(String),
    TypeMismatch(Variable, Variable),
    VariableAlreadyExists(String),
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
            Error::IoError => write!(f, "I/O error"),
            Error::VariableNotFound(name) => write!(f, "Variable not found: {}", name),
            Error::TypeMismatch(left, right) => {
                write!(
                    f,
                    "Type mismatch: {} {} and {} {}",
                    var_type(left),
                    left,
                    var_type(right),
                    right
                )
            }
            Error::VariableAlreadyExists(name) => write!(f, "Variable already exists: {}", name),
        }
    }
}

fn var_type(var: &Variable) -> &'static str {
    match var {
        Variable::Int(_) => "Int",
        Variable::Float(_) => "Float",
        Variable::Empty => "Empty",
    }
}
