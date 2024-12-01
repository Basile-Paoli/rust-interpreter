use crate::lexer::position::Position;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::{Debug, Display};

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    Op(Op),
    Assignment(Option<Op>),
    Number(i32),
    Identifier(String),
    Keyword(Keyword),
    Unknown(char),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Op(op) => write!(f, "{}", op),
            TokenKind::Assignment(op) => match op {
                Some(op) => write!(f, "Assignment({})", op),
                None => write!(f, "Assignment"),
            },
            TokenKind::Number(n) => write!(f, "Number({})", n),
            TokenKind::Identifier(s) => write!(f, "Identifier({})", s),
            TokenKind::Keyword(k) => write!(f, "Keyword({})", k),
            TokenKind::Unknown(c) => write!(f, "Unknown({})", c),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op {
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::ADD => write!(f, "Addition"),
            Op::SUB => write!(f, "Subtraction"),
            Op::MUL => write!(f, "Multiplication"),
            Op::DIV => write!(f, "Division"),
            Op::EQ => write!(f, "Equal"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    IF,
    ELSE,
    FOR,
    WHILE,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::IF => write!(f, "if"),
            Keyword::ELSE => write!(f, "else"),
            Keyword::FOR => write!(f, "for"),
            Keyword::WHILE => write!(f, "while"),
        }
    }
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, Keyword> = HashMap::from([
        ("if", Keyword::IF),
        ("else", Keyword::ELSE),
        ("for", Keyword::FOR),
        ("while", Keyword::WHILE),
    ]);
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub position: Position,
    pub kind: TokenKind,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.kind, self.position)
    }
}
