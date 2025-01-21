use crate::error::Error;
use crate::lexer::{Keyword, Token};
use crate::parser::{Expression, Parser, VariableDeclaration};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    IfStatement(IfStatement),
}

pub type Block = Vec<Instruction>;

#[derive(Clone, Debug, PartialEq)]
pub enum BlockOrInstruction {
    Block(Block),
    Instruction(Instruction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Box<BlockOrInstruction>,
}

impl<'a> Parser<'a> {
    pub fn parse_instruction(&mut self) -> Result<Option<Instruction>, Error> {
        let next = self.lexer.peek();
        match next {
            None => Ok(None),
            Some(Token::Keyword(Keyword::LET, _)) => self.parse_variable_declaration().map(Some),
            Some(Token::Keyword(Keyword::IF, _)) => self.parse_if_statement().map(Some),
            _ => self.parse_expression_instruction().map(Some),
        }
    }

    fn parse_if_statement(&mut self) -> Result<Instruction, Error> {
        match self.lexer.next() {
            Some(Token::Keyword(Keyword::IF, _)) => {
                match self.lexer.peek() {
                    Some(Token::LParen(_)) => {
                        self.lexer.next();
                    }
                    Some(token) => return Err(Error::UnexpectedToken(token.clone())),
                    None => return Err(Error::UnexpectedEof),
                }
                let condition = self.paren_expr()?;
                let body = Box::new(self.block_or_instruction()?);
                Ok(Instruction::IfStatement(IfStatement { condition, body }))
            }
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
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

    fn block_or_instruction(&mut self) -> Result<BlockOrInstruction, Error> {
        match self.lexer.peek() {
            Some(Token::LBrace(_)) => Ok(BlockOrInstruction::Block(self.block()?)),
            _ => self
                .parse_instruction()?
                .map_or(Err(Error::UnexpectedEof), |i| {
                    Ok(BlockOrInstruction::Instruction(i))
                }),
        }
    }

    pub fn block(&mut self) -> Result<Block, Error> {
        match self.lexer.next() {
            Some(Token::LBrace(_)) => {}
            Some(token) => return Err(Error::UnexpectedToken(token)),
            None => return Err(Error::UnexpectedEof),
        }

        self.identifiers_stack.push(HashMap::new());
        let mut block = Vec::new();

        if let Some(Token::RBrace(_)) = self.lexer.peek() {
            self.lexer.next();
            return Ok(block);
        }

        while let Some(instruction) = self.parse_instruction()? {
            block.push(instruction);
            if let Some(Token::RBrace(_)) = self.lexer.peek() {
                self.lexer.next();
                self.identifiers_stack.pop();
                return Ok(block);
            }
        }
        Err(Error::UnexpectedEof)
    }
}
