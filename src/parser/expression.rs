use crate::lexer::{Op, Position, Token};
use crate::parser::{Error, Parser};
use Op::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BinOp(BinOp),
    Int(Int),
    Assignment(Assignment),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinOp {
    op: Op,
    left: Box<Expression>,
    right: Box<Expression>,
    position: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Int {
    value: i32,
    position: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    left: Box<Expression>,
    right: Box<Expression>,
    op: Option<Op>,
    position: Position,
}

impl Parser<'_> {
    pub fn parse_expression(&mut self) -> Result<Expression, Error> {
        self.parse_assignment()
    }

    pub fn parse_assignment(&mut self) -> Result<Expression, Error> {
        let mut left = self.parse_add()?;
        if let Some(Token::Assignment(op, position)) =
            self.lexer.next_if(|t| matches!(t, Token::Assignment(..)))
        {
            // Check for invalid left-hand side
            // TODO
            let right = self.parse_add()?;
            left = Expression::Assignment(Assignment {
                left: Box::new(left),
                right: Box::new(right),
                op,
                position,
            });
        }
        Ok(left)
    }

    pub fn parse_add(&mut self) -> Result<Expression, Error> {
        let mut left = self.parse_mul()?;

        while let Some(Token::Op(op, position)) = self
            .lexer
            .next_if(|t| matches!(t, Token::Op(ADD | SUB, ..)))
        {
            let right = self.parse_mul()?;
            left = Expression::BinOp(BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
            });
        }
        Ok(left)
    }

    pub fn parse_mul(&mut self) -> Result<Expression, Error> {
        let mut left = self.parse_primary()?;

        while let Some(Token::Op(op, position)) = self
            .lexer
            .next_if(|t| matches!(t, Token::Op(MUL | DIV, ..)))
        {
            let right = self.parse_primary()?;
            left = Expression::BinOp(BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
            });
        }
        Ok(left)
    }

    pub fn parse_primary(&mut self) -> Result<Expression, Error> {
        self.lexer
            .next()
            .map_or(Err(Error::UnexpectedEof), |token| match token {
                Token::Number(value, position) => Ok(Expression::Int(Int { value, position })),
                _ => Err(Error::UnexpectedToken(token)),
            })
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
        let expr = parser.parse_expression().unwrap();
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
        let expr = parser.parse_expression().unwrap();
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
        let expr = parser.parse_expression().unwrap();

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
}
