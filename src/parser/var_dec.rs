use crate::error::Error;
use crate::interpreter::VarType;
use crate::lexer::{Keyword, Position, Token};
use crate::parser::{Expression, Instruction, Parser};

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Option<Expression>,
    pub position: Position,
    pub var_type: VarType,
}

impl Parser<'_> {
    pub fn parse_variable_declaration(&mut self) -> Result<Instruction, Error> {
        let t = self.lexer.next().unwrap();
        let id = self.identifier()?;

        let var_type = if let Some(Token::Colon(_)) = self.lexer.peek() {
            self.lexer.next();
            self.type_dec()?
        } else {
            VarType::Empty
        };

        let next = self.lexer.next();
        match next {
            Some(Token::Semicolon(_)) => {
                return if var_type == VarType::Empty {
                    Err(Error::UnknownVariableType(t.position()))
                } else {
                    self.identifiers.insert(id.clone(), var_type.clone());
                    Ok(Instruction::VariableDeclaration(VariableDeclaration {
                        name: id,
                        value: None,
                        position: t.position(),
                        var_type,
                    }))
                }
            }
            Some(Token::Assignment(None, ..)) => {}
            Some(token) => return Err(Error::UnexpectedToken(token)),
            None => return Err(Error::UnexpectedEof),
        }

        let value = self.expression()?;
        let expr_type = value.expr_type();

        self.identifiers.insert(id.clone(), value.expr_type());

        match self.lexer.next() {
            Some(Token::Semicolon(_)) => {
                Ok(Instruction::VariableDeclaration(VariableDeclaration {
                    name: id,
                    value: Some(value),
                    position: t.position(),
                    var_type: expr_type,
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

    fn type_dec(&mut self) -> Result<VarType, Error> {
        let mut root_type = self.type_identifier()?;
        while let Some(Token::LBracket(_)) = self.lexer.peek() {
            self.lexer.next();
            let next = self.lexer.next();
            match next {
                Some(Token::RBracket(_)) => {
                    root_type = VarType::Array(Box::new(root_type));
                }
                Some(token) => return Err(Error::UnexpectedToken(token)),
                None => return Err(Error::UnexpectedEof),
            }
        }
        Ok(root_type)
    }

    fn type_identifier(&mut self) -> Result<VarType, Error> {
        let next = self.lexer.next();
        match &next {
            Some(Token::Keyword(keyword, _)) => match keyword {
                Keyword::INT => Ok(VarType::Int),
                Keyword::FLOAT => Ok(VarType::Float),
                Keyword::STRING => Ok(VarType::String),
                _ => Err(Error::UnexpectedToken(next.unwrap())),
            },
            None => Err(Error::UnexpectedEof),
            Some(token) => Err(Error::UnexpectedToken(token.clone())),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::interpreter::VarType;
    use crate::parser::Instruction;
    use crate::parser::Parser;
    use crate::parser::VariableDeclaration;
    use crate::parser::{Expression, Int};

    #[test]
    fn test_parse_variable_declaration() {
        let mut p = Parser::new("let x: int = 2;");
        let result = p.parse_variable_declaration().unwrap();
        let expected = Instruction::VariableDeclaration(VariableDeclaration {
            name: "x".to_string(),
            value: Some(Expression::Int(Int {
                value: 2,
                position: Position {
                    line: 1,
                    column: 14,
                },
            })),
            position: Position { line: 1, column: 1 },
            var_type: VarType::Int,
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_variable_declaration_no_type() {
        let mut p = Parser::new("let x = 2;");
        let result = p.parse_variable_declaration().unwrap();
        let expected = Instruction::VariableDeclaration(VariableDeclaration {
            name: "x".to_string(),
            value: Some(Expression::Int(Int {
                value: 2,
                position: Position { line: 1, column: 9 },
            })),
            position: Position { line: 1, column: 1 },
            var_type: VarType::Int,
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_variable_declaration_no_value() {
        let mut p = Parser::new("let x: int;");
        let result = p.parse_variable_declaration().unwrap();
        let expected = Instruction::VariableDeclaration(VariableDeclaration {
            name: "x".to_string(),
            value: None,
            position: Position { line: 1, column: 1 },
            var_type: VarType::Int,
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_variable_declaration_array() {
        let mut p = Parser::new("let x: int[] = [2, 3];");
        let result = p.parse_variable_declaration().unwrap();

        let Instruction::VariableDeclaration(VariableDeclaration {
            var_type, value, ..
        }) = result
        else {
            panic!("Expected VariableDeclaration");
        };

        assert_eq!(var_type, VarType::Array(Box::new(VarType::Int)));
        let Some(Expression::Array(array)) = value else {
            panic!("Expected Array");
        };

        let expected = vec![
            Expression::Int(Int {
                value: 2,
                position: Position {
                    line: 1,
                    column: 17,
                },
            }),
            Expression::Int(Int {
                value: 3,
                position: Position {
                    line: 1,
                    column: 20,
                },
            }),
        ];
        assert_eq!(array.elements, expected);
    }
}
