use crate::lexer::Op::*;
use crate::lexer::{Op, Token, TokenKind};
use crate::parser::ast::{AstNode, Node};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BinOp(BinOp),
    Int(Int),
}
use crate::parser::{Error, Parser};
use Expression::*;

#[derive(Clone, Debug, PartialEq)]
pub struct BinOp {
    op: Op,
    operands: Box<[AstNode; 2]>,
}

impl BinOp {
    pub fn new(token: &Token, left: AstNode, right: AstNode) -> AstNode {
        AstNode {
            node: Node::Expression(BinOp(BinOp {
                op: BinOp::op_from_token(token),
                operands: Box::new([left, right]),
            })),
            position: token.position,
        }
    }

    fn op_from_token(token: &Token) -> Op {
        if let TokenKind::Op(op @ (ADD | SUB | MUL | DIV)) = token.kind {
            op
        } else {
            unreachable!()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Int {
    value: i32,
}

impl Int {
    pub fn new(token: &Token) -> AstNode {
        AstNode {
            node: Node::Expression(Int(Int {
                value: match token.kind {
                    TokenKind::Number(n) => n,
                    _ => unreachable!(),
                },
            })),
            position: token.position,
        }
    }
}

impl Parser<'_> {
    pub fn parse_expression(&mut self) -> Result<AstNode, Error> {
        self.parse_add()
    }

    pub fn parse_add(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_mul()?;

        while let Some(token) = self
            .lexer
            .next_if(|t| matches!(t.kind, TokenKind::Op(ADD | SUB)))
        {
            let right = self.parse_mul()?;
            left = BinOp::new(&token, left, right);
        }
        Ok(left)
    }

    pub fn parse_mul(&mut self) -> Result<AstNode, Error> {
        let mut left = self.parse_primary()?;

        while let Some(token) = self
            .lexer
            .next_if(|t| matches!(t.kind, TokenKind::Op(MUL | DIV)))
        {
            let right = self.parse_primary()?;
            left = BinOp::new(&token, left, right);
        }
        Ok(left)
    }

    pub fn parse_primary(&mut self) -> Result<AstNode, Error> {
        self.lexer
            .next()
            .map_or(Err(Error::UnexpectedEof), |token| match token.kind {
                TokenKind::Number(_) => Ok(Int::new(&token)),
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
        let ast = parser.parse_expression().unwrap();
        assert_eq!(
            ast,
            AstNode {
                node: Node::Expression(Int(Int { value: 1 })),
                position: Position::new()
            }
        );
    }

    #[test]
    fn test_parse_binop() {
        let input = "1 + 2";
        let mut parser = Parser::new(input);
        let ast = parser.parse_expression().unwrap();
        if let Node::Expression(BinOp(binop)) = ast.node {
            assert_eq!(binop.op, ADD);
            assert_eq!(
                binop.operands[0].node,
                Node::Expression(Int(Int { value: 1 }))
            );
            assert_eq!(
                binop.operands[1].node,
                Node::Expression(Int(Int { value: 2 }))
            );
        } else {
            unreachable!("Expected BinOp node");
        }
    }

    #[test]
    fn test_precedence() {
        let input = "1 + 2 * 3";
        let mut parser = Parser::new(input);
        let ast = parser.parse_expression().unwrap();

        if let Node::Expression(BinOp(binop)) = ast.node {
            assert_eq!(binop.op, ADD);
            assert_eq!(
                binop.operands[0].node,
                Node::Expression(Int(Int { value: 1 }))
            );
            assert!(matches!(
                binop.operands[1].node,
                Node::Expression(BinOp(BinOp { op: MUL, .. }))
            ));
        } else {
            unreachable!("Expected BinOp node");
        }
    }
}
