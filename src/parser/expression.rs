use crate::lexer::{Op, Position, Token};
use crate::parser::{Error, Parser};
use Op::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BinOp(BinOp),
    Int(Int),
    Float(Float),
    Assignment(Assignment),
    LValue(LValue),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinOp {
    pub op: Op,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    position: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Int {
    pub value: i32,
    pub position: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Float {
    pub value: f64,
    pub position: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub left: Box<LValue>,
    pub right: Box<Expression>,
    pub op: Option<Op>,
    position: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LValue {
    Identifier(Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub position: Position,
}

impl Parser<'_> {
    pub fn expression(&mut self) -> Result<Expression, Error> {
        self.assignment()
    }

    pub fn assignment(&mut self) -> Result<Expression, Error> {
        let mut left = self.add()?;
        if let Some(Token::Assignment(op, position)) =
            self.lexer.next_if(|t| matches!(t, Token::Assignment(..)))
        {
            match left {
                Expression::LValue(lvalue) => {
                    let right = self.add()?;
                    left = Expression::Assignment(Assignment {
                        left: Box::new(lvalue),
                        right: Box::new(right),
                        op,
                        position,
                    });
                }
                _ => return Err(Error::InvalidAssignmentTarget(position)),
            }
        }
        Ok(left)
    }

    pub fn add(&mut self) -> Result<Expression, Error> {
        let mut left = self.mul()?;

        while let Some(Token::Op(op, position)) = self
            .lexer
            .next_if(|t| matches!(t, Token::Op(ADD | SUB, ..)))
        {
            let right = self.mul()?;
            left = Expression::BinOp(BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
            });
        }
        Ok(left)
    }

    pub fn mul(&mut self) -> Result<Expression, Error> {
        let mut left = self.primary()?;

        while let Some(Token::Op(op, position)) = self
            .lexer
            .next_if(|t| matches!(t, Token::Op(MUL | DIV, ..)))
        {
            let right = self.primary()?;
            left = Expression::BinOp(BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
            });
        }
        Ok(left)
    }

    pub fn primary(&mut self) -> Result<Expression, Error> {
        self.lexer
            .next()
            .map_or(Err(Error::UnexpectedEof), |token| match token {
                Token::Int(value, position) => Ok(Expression::Int(Int { value, position })),
                Token::Float(value, position) => Ok(Expression::Float(Float { value, position })),
                Token::Identifier(name, position) => {
                    Ok(Expression::LValue(LValue::Identifier(Identifier {
                        name,
                        position,
                    })))
                }
                Token::LParen(_) => self.paren_expr(),
                _ => Err(Error::UnexpectedToken(token)),
            })
    }

    pub fn paren_expr(&mut self) -> Result<Expression, Error> {
        let expr = self.expression()?;
        match self.lexer.next() {
            Some(Token::RParen(_)) => Ok(expr),
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Position;

    #[test]
    fn test_parse_int() {
        let input = "1";
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();
        assert_eq!(
            expr,
            Expression::Int(Int {
                value: 1,
                position: Position { line: 1, column: 1 }
            })
        );
    }

    #[test]
    fn test_parse_binop() {
        let input = "1 + 2";
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();
        if let Expression::BinOp(binop) = expr {
            assert_eq!(binop.op, ADD);
            assert!(matches!(*binop.left, Expression::Int(Int { value: 1, .. })));
            assert!(matches!(
                *binop.right,
                Expression::Int(Int { value: 2, .. })
            ));
        } else {
            unreachable!("Expected BinOp node");
        }
    }

    #[test]
    fn test_precedence() {
        let input = "1 + 2 * 3";
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();

        if let Expression::BinOp(binop) = expr {
            assert_eq!(binop.op, ADD);
            assert!(matches!(*binop.left, Expression::Int(Int { value: 1, .. })));
            assert!(matches!(
                *binop.right,
                Expression::BinOp(BinOp { op: MUL, .. })
            ));
        } else {
            unreachable!("Expected BinOp node");
        }
    }

    #[test]
    fn test_parent_expr() {
        let input = "(1 + 2) * 3";
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();

        if let Expression::BinOp(binop) = expr {
            assert_eq!(binop.op, MUL);
            assert!(matches!(
                *binop.left,
                Expression::BinOp(BinOp { op: ADD, .. })
            ));
            assert!(matches!(
                *binop.right,
                Expression::Int(Int { value: 3, .. })
            ));
        } else {
            unreachable!("Expected BinOp node");
        }
    }

    #[test]
    fn test_invalid_assignment() {
        let input = "1 + 2 = 3";
        let mut parser = Parser::new(input);
        let err = parser.expression().unwrap_err();
        assert!(matches!(err, Error::InvalidAssignmentTarget(..)));
    }
}
