use crate::error::Error;
use crate::interpreter::{Interpreter, Variable};
use crate::lexer::Op;
use crate::parser::{Assignment, BinOp, Expression, LValue};
use std::collections::hash_map::Entry;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn expression(&mut self, expression: Expression) -> Result<Variable, Error> {
        match expression {
            Expression::BinOp(binop) => self.binop(binop),
            Expression::Int(int) => Ok(int.value),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::LValue(lvalue) => Ok(*self.lvalue(lvalue)?),
        }
    }

    fn binop(&mut self, binop: BinOp) -> Result<Variable, Error> {
        let left = self.expression(*binop.left)?;
        let right = self.expression(*binop.right)?;
        match binop.op {
            Op::ADD => Ok(left + right),
            Op::SUB => Ok(left - right),
            Op::MUL => Ok(left * right),
            Op::DIV => Self::division(left, right),
            Op::EQ => Ok((left == right) as i32),
        }
    }

    fn assignment(&mut self, assignment: Assignment) -> Result<Variable, Error> {
        let right = self.expression(*assignment.right)?;
        let left = self.lvalue(*assignment.left)?;
        *left = match assignment.op {
            Some(Op::ADD) => *left + right,
            Some(Op::SUB) => *left - right,
            Some(Op::MUL) => *left * right,
            Some(Op::DIV) => Self::division(*left, right)?,
            None => right,
            _ => unreachable!(),
        };
        Ok(*left)
    }

    fn lvalue(&mut self, lvalue: LValue) -> Result<&mut Variable, Error> {
        match lvalue {
            LValue::Identifier(identifier) => {
                let name = identifier.name;
                match self.variables.entry(name) {
                    Entry::Occupied(o) => Ok(o.into_mut()),
                    Entry::Vacant(v) => Ok(v.insert(0)),
                }
            }
        }
    }

    fn division(left: i32, right: i32) -> Result<Variable, Error> {
        if right == 0 {
            Err(Error::DivisionByZero)
        } else {
            Ok(left / right)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_expression() {
        let mut p = Parser::new("1 + 2 * 3");
        let mut i = Interpreter::new();
        let expr = p.expression().unwrap();
        let result = i.expression(expr).unwrap();
        assert_eq!(result, 7);
    }

    #[test]
    fn test_variable() {
        let mut p = Parser::new("x = 4/2 - 1");
        let mut i = Interpreter::new();
        let expr = p.expression().unwrap();
        let result = i.expression(expr).unwrap();
        assert_eq!(result, 1);

        let expr = Parser::new("x").expression().unwrap();
        let result = i.expression(expr).unwrap();
        assert_eq!(result, 1);
    }

    #[test]
    fn test_division_by_zero() {
        let mut p = Parser::new("x = 1 / 0");
        let mut i = Interpreter::new();
        let expr = p.expression().unwrap();
        let result = i.expression(expr);
        assert!(result.is_err());
    }
}
