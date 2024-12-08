use crate::error::Error;
use crate::lexer::{Position, Token};
use crate::parser::{Expression, Identifier, Instruction, Parser};

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Option<Expression>,
    pub position: Position,
}

impl Parser<'_> {
    pub fn parse_variable_declaration(&mut self) -> Result<Instruction, Error> {
        self.lexer.next();
        let id = self.identifier()?;
        let value = if let Some(Token::Assignment(None, _)) = self.lexer.peek() {
            self.lexer.next();
            Some(self.expression()?)
        } else {
            None
        };
        match self.lexer.next() {
            Some(Token::Semicolon(_)) => {
                Ok(Instruction::VariableDeclaration(VariableDeclaration {
                    name: id.name,
                    value,
                    position: id.position,
                }))
            }
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
        }
    }

    fn identifier(&mut self) -> Result<Identifier, Error> {
        if let Some(Token::Identifier(name, position)) = self.lexer.next() {
            Ok(Identifier { name, position })
        } else {
            Err(Error::UnexpectedEof)
        }
    }
}
