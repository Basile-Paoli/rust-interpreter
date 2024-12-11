use crate::error::Error;
use crate::lexer::{Position, Token};
use crate::parser::{Expression, Instruction, Parser};

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Expression,
    pub position: Position,
}

impl Parser<'_> {
    pub fn parse_variable_declaration(&mut self) -> Result<Instruction, Error> {
        let t = self.lexer.next().unwrap();
        let id = self.identifier()?;

        let next = self.lexer.next();
        if !matches!(next, Some(Token::Assignment(_, _))) {
            return Err(Error::UnexpectedToken(next.unwrap()));
        }

        let value = self.expression()?;

        self.identifiers.insert(id.clone(), value.expr_type());

        match self.lexer.next() {
            Some(Token::Semicolon(_)) => {
                Ok(Instruction::VariableDeclaration(VariableDeclaration {
                    name: id,
                    value,
                    position: t.position(),
                }))
            }
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
        }
    }

    fn identifier(&mut self) -> Result<String, Error> {
        if let Some(Token::Identifier(name, _position)) = self.lexer.next() {
            Ok(name)
        } else {
            Err(Error::UnexpectedEof)
        }
    }
}
