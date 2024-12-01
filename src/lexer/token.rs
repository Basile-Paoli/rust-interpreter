use crate::lexer::position::Position;
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    EOF,
    OP(OpKind),
    NUMBER(i32),
    IDENTIFIER(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OpKind {
    ADD,
    SUB,
    MUL,
    DIV,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub position: Position,
    pub kind: TokenKind,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} at line: {}, column: {}",
            self.kind, self.position.line, self.position.column
        )
    }
}
