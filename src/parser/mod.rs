use crate::error::Error;
use crate::lexer::{Keyword, Lexer, Token};
use std::collections::HashMap;
use std::iter::Peekable;

mod display;
mod expression;
mod type_analysis;
mod var_dec;

use crate::interpreter::VarType;
pub use expression::{ArrayLit, Assignment, BinOp, Expression, Identifier, Int, LValue};
pub use var_dec::VariableDeclaration;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
}

pub struct Parser<'a> {
    pub lexer: Peekable<Lexer<'a>>,
    pub identifiers: HashMap<String, VarType>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input).peekable(),
            identifiers: HashMap::new(),
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
            Some(Token::Keyword(Keyword::LET, _)) => self.parse_variable_declaration().map(Some),
            _ => self.parse_expression_instruction().map(Some),
        }
    }

    fn parse_expression_instruction(&mut self) -> Result<Instruction, Error> {
        let expression = self.expression()?;
        //TODO: if expression is not an assignment or function call, invalid
        match self.lexer.next() {
            Some(Token::Semicolon(_)) => Ok(Instruction::Expression(expression)),
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
        }
    }
}
