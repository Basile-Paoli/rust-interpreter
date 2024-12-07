use crate::error::Error;
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;

mod display;
mod expression;

pub use expression::{Assignment, BinOp, Expression, Identifier, Int, LValue};

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Expression(Expression),
}

pub(crate) struct Parser<'a> {
    pub(crate) lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Instruction>, Error> {
        let mut instructions = Vec::new();
        while let Some(instruction) = self.parse_instruction()? {
            instructions.push(instruction);
        }
        Ok(instructions)
    }

    fn parse_instruction(&mut self) -> Result<Option<Instruction>, Error> {
        let next = self.lexer.peek();
        match next {
            None => Ok(None),
            _ => self.parse_expression_instruction(),
        }
    }

    fn parse_expression_instruction(&mut self) -> Result<Option<Instruction>, Error> {
        let expression = self.parse_expression()?;
        match self.lexer.next() {
            Some(Token::Semicolon(_)) => Ok(Some(Instruction::Expression(expression))),
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
        }
    }
}
