use crate::error::ToErrorResult;
use crate::lexer::{Keyword, Op, Position, Token};
use crate::parser::type_analysis::{array_lit_type, operation_type};
use crate::parser::{Error, Parser};
use crate::var_type::VarType;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    BinOp(BinOp),
    Bool(bool, Position),
    Int(i32, Position),
    Float(f64, Position),
    StringLit(String, Position),
    Array(ArrayLit),
    Assignment(Assignment),
    LValue(LValue),
}

impl Expression {
    pub fn expr_type(&self) -> VarType {
        match self {
            Expression::BinOp(binop) => binop.res_type.clone(),
            Expression::Bool(..) => VarType::Bool,
            Expression::Int(..) => VarType::Int,
            Expression::Float(..) => VarType::Float,
            Expression::StringLit(..) => VarType::String,
            Expression::Array(array) => VarType::Array(Box::new(array.array_type.clone())),
            Expression::Assignment(assignment) => assignment.var_type.clone(),
            Expression::LValue(lvalue) => lvalue.var_type(),
        }
    }

    pub fn position(&self) -> Position {
        match self {
            Expression::BinOp(binop) => binop.position,
            Expression::Bool(_, position) => *position,
            Expression::Int(_, position) => *position,
            Expression::Float(_, position) => *position,
            Expression::StringLit(_, position) => *position,
            Expression::Array(array) => array.position,
            Expression::Assignment(assignment) => assignment.position,
            Expression::LValue(lvalue) => lvalue.position(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinOp {
    pub op: Op,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub(crate) position: Position,
    res_type: VarType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayLit {
    pub elements: Vec<Expression>,
    pub position: Position,
    pub array_type: VarType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub left: Box<LValue>,
    pub right: Box<Expression>,
    pub op: Option<Op>,
    pub(crate) position: Position,
    var_type: VarType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LValue {
    Identifier(Identifier),
}

impl LValue {
    fn var_type(&self) -> VarType {
        match self {
            LValue::Identifier(id) => id.var_type.clone(),
        }
    }

    fn position(&self) -> Position {
        match self {
            LValue::Identifier(id) => id.position,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub position: Position,
    pub var_type: VarType,
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
            match &left {
                Expression::LValue(lvalue) => {
                    left = self.new_assignment(op, position, lvalue)?;
                }
                _ => return Err(Error::InvalidAssignmentTarget(position)),
            }
        }
        Ok(left)
    }

    fn new_assignment(
        &mut self,
        op: Option<Op>,
        position: Position,
        lvalue: &LValue,
    ) -> Result<Expression, Error> {
        let right = self.add()?;
        let var_type = lvalue.var_type();
        if let Some(op) = op.clone() {
            let op_type = operation_type(op, lvalue.var_type(), right.expr_type())
                .to_error_result(position)?;
            if op_type != var_type {
                return Err(Error::TypeMismatch(var_type, right.expr_type(), position));
            }
        } else if var_type != right.expr_type() && right.expr_type().root_type() != VarType::Empty {
            return Err(Error::TypeMismatch(var_type, right.expr_type(), position));
        }

        Ok(Expression::Assignment(Assignment {
            left: Box::new(lvalue.clone()),
            right: Box::new(right),
            op,
            position,
            var_type,
        }))
    }

    pub fn add(&mut self) -> Result<Expression, Error> {
        let mut left = self.mul()?;

        while let Some(Token::Op(op, position)) = self
            .lexer
            .next_if(|t| matches!(t, Token::Op(Op::ADD | Op::SUB, ..)))
        {
            let right = self.mul()?;
            let res_type = operation_type(op, left.expr_type(), right.expr_type())
                .to_error_result(position)?;
            left = Expression::BinOp(BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
                res_type,
            });
        }
        Ok(left)
    }

    pub fn mul(&mut self) -> Result<Expression, Error> {
        let mut left = self.primary()?;

        while let Some(Token::Op(op, position)) = self
            .lexer
            .next_if(|t| matches!(t, Token::Op(Op::MUL | Op::DIV, ..)))
        {
            let right = self.primary()?;
            let res_type = operation_type(op, left.expr_type(), right.expr_type())
                .to_error_result(position)?;
            left = Expression::BinOp(BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                position,
                res_type,
            });
        }
        Ok(left)
    }

    pub fn primary(&mut self) -> Result<Expression, Error> {
        self.lexer
            .next()
            .map_or(Err(Error::UnexpectedEof), |token| match token {
                Token::Int(value, position) => Ok(Expression::Int(value, position)),
                Token::Float(value, position) => Ok(Expression::Float(value, position)),
                Token::String(value, position) => Ok(Expression::StringLit(value, position)),
                Token::Keyword(Keyword::FALSE, position) => Ok(Expression::Bool(false, position)),
                Token::Keyword(Keyword::TRUE, position) => Ok(Expression::Bool(true, position)),
                Token::Identifier(name, position) => self.identifier_expression(name, position),
                Token::LParen(_) => self.paren_expr(),
                Token::LBracket(position) => self.array(position),
                _ => Err(Error::UnexpectedToken(token)),
            })
    }

    fn identifier_expression(
        &mut self,
        name: String,
        position: Position,
    ) -> Result<Expression, Error> {
        let var_type = self.get_identifier(&name).cloned();
        match var_type {
            Some(var_type) => Ok(Expression::LValue(LValue::Identifier(Identifier {
                name,
                position,
                var_type,
            }))),
            None => Err(Error::VariableNotFound(name, position)),
        }
    }

    pub fn paren_expr(&mut self) -> Result<Expression, Error> {
        let expr = self.expression()?;
        match self.lexer.next() {
            Some(Token::RParen(_)) => Ok(expr),
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
        }
    }

    pub fn array(&mut self, position: Position) -> Result<Expression, Error> {
        let mut elements = Vec::new();
        while !matches!(self.lexer.peek(), Some(Token::RBracket(..))) {
            let expr = self.expression()?;
            self.lexer.next_if(|t| matches!(t, Token::Comma(..)));
            elements.push(expr);
        }

        match self.lexer.next() {
            Some(Token::RBracket(_)) => {}
            Some(token) => return Err(Error::UnexpectedToken(token)),
            None => return Err(Error::UnexpectedEof),
        }

        let array_type = array_lit_type(&elements).to_error_result(position)?;
        Ok(Expression::Array(ArrayLit {
            elements,
            position,
            array_type,
        }))
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
        assert_eq!(expr, Expression::Int(1, Position { line: 1, column: 1 }));
    }

    #[test]
    fn test_parse_binop() {
        let input = "1 + 2";
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();
        if let Expression::BinOp(binop) = expr {
            assert_eq!(binop.op, Op::ADD);
            assert_eq!(
                *binop.left,
                Expression::Int(1, Position { line: 1, column: 1 })
            );
            assert_eq!(
                *binop.right,
                Expression::Int(2, Position { line: 1, column: 5 })
            );
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
            assert_eq!(binop.op, Op::ADD);
            assert_eq!(
                *binop.left,
                Expression::Int(1, Position { line: 1, column: 1 })
            );
            assert!(matches!(
                *binop.right,
                Expression::BinOp(BinOp { op: Op::MUL, .. })
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
            assert_eq!(binop.op, Op::MUL);
            assert!(matches!(
                *binop.left,
                Expression::BinOp(BinOp { op: Op::ADD, .. })
            ));
            assert_eq!(
                *binop.right,
                Expression::Int(
                    3,
                    Position {
                        line: 1,
                        column: 11
                    }
                )
            );
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

    #[test]
    fn test_array() {
        let input = "[1, 2, 3]";
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();
        if let Expression::Array(array) = expr {
            assert_eq!(array.elements.len(), 3);
            assert_eq!(
                array.elements[0],
                Expression::Int(1, Position { line: 1, column: 2 })
            );
            assert_eq!(
                array.elements[1],
                Expression::Int(2, Position { line: 1, column: 5 })
            );
            assert_eq!(
                array.elements[2],
                Expression::Int(3, Position { line: 1, column: 8 })
            );
        } else {
            unreachable!("Expected Array node");
        }
    }

    #[test]
    fn test_trailing_comma() {
        let input = "[1, 2, 3,]";
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();
        if let Expression::Array(array) = expr {
            assert_eq!(array.elements.len(), 3);
        } else {
            unreachable!("Expected Array node");
        }
    }

    #[test]
    fn test_invalid_comma() {
        let input = "[,]";
        let mut parser = Parser::new(input);
        let err = parser.expression().unwrap_err();
        assert!(matches!(err, Error::UnexpectedToken(..)));

        let input = "[1,,]";
        let mut parser = Parser::new(input);
        let err = parser.expression().unwrap_err();
        assert!(matches!(err, Error::UnexpectedToken(..)));
    }

    #[test]
    fn test_nested_array() {
        let input = "[[1,2], []];";

        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap();
        if let Expression::Array(array) = expr {
            assert_eq!(array.elements.len(), 2);
            if let Expression::Array(inner) = &array.elements[0] {
                assert_eq!(inner.elements.len(), 2);
                assert_eq!(
                    inner.elements[0],
                    Expression::Int(1, Position { line: 1, column: 3 })
                );
                assert_eq!(
                    inner.elements[1],
                    Expression::Int(2, Position { line: 1, column: 5 })
                );
            } else {
                unreachable!("Expected Array node");
            }
            if let Expression::Array(inner) = &array.elements[1] {
                assert_eq!(inner.elements.len(), 0);
            } else {
                unreachable!("Expected Array node");
            }
        } else {
            unreachable!("Expected Array node");
        }
    }

    #[test]
    fn test_invalid_assignment_type() {
        let input = "let x: int = 1; x = 2.0;";
        let mut parser = Parser::new(input);
        let res = parser.parse();
        assert_eq!(
            res,
            Err(Error::TypeMismatch(
                VarType::Int,
                VarType::Float,
                Position {
                    line: 1,
                    column: 19
                }
            ))
        );
    }

    #[test]
    fn test_double_declare() {
        let input = "let x: int = 1; let x: int = 2;";
        let mut parser = Parser::new(input);
        let res = parser.parse();
        assert_eq!(
            res,
            Err(Error::VariableAlreadyExists(
                "x".to_string(),
                Position {
                    line: 1,
                    column: 17
                }
            ))
        );
    }

    #[test]
    fn test_variable_not_in_scope() {
        let input = "if(true) { let x = 1; } x = 2;";
        let mut parser = Parser::new(input);
        let res = parser.parse();
        assert_eq!(
            res,
            Err(Error::VariableNotFound(
                "x".to_string(),
                Position {
                    line: 1,
                    column: 25
                }
            ))
        );
    }
}
