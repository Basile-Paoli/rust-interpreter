use crate::error::{Error, InterpreterError, ToErrorResult};
use crate::lexer::{Keyword, Position, Token};
use crate::parser::{Expression, Instruction, Parser};
use crate::var_type::VarType;

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

        if self.identifier_exists_in_current_scope(&id) {
            return Err(Error::VariableAlreadyExists(id, t.position()));
        }

        let var_type = if let Some(Token::Colon(_)) = self.lexer.peek() {
            self.lexer.next();
            self.type_dec()?
        } else {
            VarType::Empty
        };

        let next = self.lexer.next();
        match next {
            Some(Token::Semicolon(_)) => {
                return if var_type.root_type() == VarType::Empty {
                    Err(Error::UnknownVariableType(t.position()))
                } else {
                    self.insert_identifier(id.clone(), var_type.clone());
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
        let p = t.position();
        let res_type = infer_type(var_type, expr_type).to_error_result(p)?;

        self.insert_identifier(id.clone(), value.expr_type());

        match self.lexer.next() {
            Some(Token::Semicolon(_)) => {
                Ok(Instruction::VariableDeclaration(VariableDeclaration {
                    name: id,
                    value: Some(value),
                    position: t.position(),
                    var_type: res_type,
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

fn infer_type(explicit_type: VarType, expr_type: VarType) -> Result<VarType, InterpreterError> {
    if explicit_type.root_type() == VarType::Empty {
        if expr_type.root_type() == VarType::Empty {
            return Err(InterpreterError::UnknownVariableType);
        }
        Ok(expr_type)
    } else if expr_type.root_type() == VarType::Empty {
        if expr_type.depth() > explicit_type.depth() {
            return Err(InterpreterError::TypeMismatch(explicit_type, expr_type));
        }
        Ok(explicit_type)
    } else if explicit_type == expr_type {
        Ok(explicit_type)
    } else {
        Err(InterpreterError::TypeMismatch(explicit_type, expr_type))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_variable_declaration() {
        let mut p = Parser::new("let x: int = 2;");
        let result = p.parse_variable_declaration().unwrap();
        let expected = Instruction::VariableDeclaration(VariableDeclaration {
            name: "x".to_string(),
            value: Some(Expression::Int(
                2,
                Position {
                    line: 1,
                    column: 14,
                },
            )),
            position: Position { line: 1, column: 1 },
            var_type: VarType::Int,
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_variable_dec_twice() {
        let mut p = Parser::new("let x= 2; let x= 3;");
        p.parse_variable_declaration()
            .expect("Failed to parse first variable declaration");
        let result = p.parse_variable_declaration();
        assert_eq!(
            result,
            Err(Error::VariableAlreadyExists(
                "x".to_string(),
                Position {
                    line: 1,
                    column: 11
                }
            ))
        );
    }

    #[test]
    fn test_variable_dec_scope() {
        let mut p = Parser::new("let x= 2; { let x= 3; }");
        p.parse_variable_declaration()
            .expect("Failed to parse first variable declaration");
        assert!(p.block().is_ok());
    }

    #[test]
    fn test_var_dec_scope_error() {
        let mut p = Parser::new("if (true) { let x = 2; } x = 3;");
        let result = p.parse();
        assert_eq!(
            result,
            Err(Error::VariableNotFound(
                "x".to_string(),
                Position {
                    line: 1,
                    column: 26
                }
            ))
        );
    }

    #[test]
    fn test_var_dec_type_mismatch() {
        let mut p = Parser::new("let x: int = 2.0;");
        let result = p.parse_variable_declaration();
        assert_eq!(
            result,
            Err(Error::TypeMismatch(
                VarType::Int,
                VarType::Float,
                Position { line: 1, column: 1 }
            ))
        );
    }

    #[test]
    fn test_parse_variable_declaration_no_type() {
        let mut p = Parser::new("let x = 2;");
        let result = p.parse_variable_declaration().unwrap();
        let expected = Instruction::VariableDeclaration(VariableDeclaration {
            name: "x".to_string(),
            value: Some(Expression::Int(2, Position { line: 1, column: 9 })),
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
    fn test_parse_var_dec_ambiguous_type() {
        let mut p = Parser::new("let x = [];");
        let result = p.parse_variable_declaration();
        assert_eq!(
            result,
            Err(Error::UnknownVariableType(Position { line: 1, column: 1 }))
        );
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
            Expression::Int(
                2,
                Position {
                    line: 1,
                    column: 17,
                },
            ),
            Expression::Int(
                3,
                Position {
                    line: 1,
                    column: 20,
                },
            ),
        ];
        assert_eq!(array.elements, expected);
    }
}
