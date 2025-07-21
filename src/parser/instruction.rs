use crate::error::Error;
use crate::expect_token;
use crate::lexer::{Keyword, Position, Token};
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
    pub else_body: Option<Box<BlockOrInstruction>>,
    pub position: Position,
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
        let position = expect_token!(self.lexer,
            Token::Keyword(Keyword::IF, position) => position
        )?;

        expect_token!(self.lexer, Token::LParen(_))?;

        let condition = self.paren_expr()?;
        let body = Box::new(self.block_or_instruction()?);
        let else_body = if let Some(Token::Keyword(Keyword::ELSE, _)) = self.lexer.peek() {
            self.lexer.next();
            Some(Box::new(self.block_or_instruction()?))
        } else {
            None
        };
        Ok(Instruction::IfStatement(IfStatement {
            condition,
            body,
            else_body,
            position,
        }))
    }

    fn parse_expression_instruction(&mut self) -> Result<Instruction, Error> {
        let expression = self.expression()?;
        //TODO: if expression is not an assignment or function call, invalid
        expect_token!(self.lexer, Token::Semicolon(_))?;
        Ok(Instruction::Expression(expression))
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
        expect_token!(self.lexer, Token::LBrace(_))?;

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
