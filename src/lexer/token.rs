use crate::lexer::position::Position;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::{Debug, Display};

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Semicolon(Position),
    LParen(Position),
    RParen(Position),
    Op(Op, Position),
    Assignment(Option<Op>, Position),
    Number(i32, Position),
    Identifier(String, Position),
    Keyword(Keyword, Position),
    Unknown(char, Position),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Semicolon(p) => write!(f, "Semicolon at {}", p),
            Token::LParen(p) => write!(f, "Left Parenthesis at {}", p),
            Token::RParen(p) => write!(f, "Right Parenthesis at {}", p),
            Token::Op(op, p) => write!(f, "{} at {}", op, p),
            Token::Assignment(op, p) => match op {
                Some(op) => write!(f, "Assignment({}) at {}", op, p),
                None => write!(f, "Assignment at {}", p),
            },
            Token::Number(n, p) => write!(f, "Number({}) at {}", n, p),
            Token::Identifier(s, p) => write!(f, "Identifier({}) at {}", s, p),
            Token::Keyword(k, p) => write!(f, "Keyword({}) at {}", k, p),
            Token::Unknown(c, p) => write!(f, "Unknown({}) at {}", c, p),
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
