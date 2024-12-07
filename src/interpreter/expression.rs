use crate::error::Error;
use crate::interpreter::{Interpreter, VariableReference};
use crate::lexer::Op;
use crate::parser::{Assignment, BinOp, Expression, LValue};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn expression(&mut self, expression: Expression) -> Result<i32, Error> {
        match expression {
            Expression::BinOp(binop) => self.binop(binop),
            Expression::Int(int) => Ok(int.value),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::LValue(lvalue) => Ok(*self.lvalue(lvalue)?.borrow()),
        }
    }

    fn binop(&mut self, binop: BinOp) -> Result<i32, Error> {
        let left = self.expression(*binop.left)?;
        let right = self.expression(*binop.right)?;
        match binop.op {
            Op::ADD => Ok(left + right),
            Op::SUB => Ok(left - right),
            Op::MUL => Ok(left * right),
            Op::DIV => self.division(left, right),
            Op::EQ => Ok((left == right) as i32),
        }
    }

    fn assignment(&mut self, assignment: Assignment) -> Result<i32, Error> {
        let right = self.expression(*assignment.right)?;
        let left = self.lvalue(*assignment.left)?;
        *left.borrow_mut() = match assignment.op {
            Some(Op::ADD) => *left.borrow() + right,
            Some(Op::SUB) => *left.borrow() - right,
            Some(Op::MUL) => *left.borrow() * right,
            Some(Op::DIV) => self.division(*left.borrow(), right)?,
            None => right,
            _ => unreachable!(),
        };
        let x = Ok(*left.borrow());
        x
    }

    fn lvalue(&mut self, lvalue: LValue) -> Result<VariableReference, Error> {
        match lvalue {
            LValue::Identifier(identifier) => {
                let name = identifier.name;
                match self.variables.entry(name) {
                    Entry::Occupied(entry) => Ok(entry.get().clone()),
                    Entry::Vacant(entry) => Ok(entry
                        .insert(VariableReference::new(RefCell::new(0)))
                        .clone()),
                }
            }
        }
    }

    fn division(&self, left: i32, right: i32) -> Result<i32, Error> {
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
        let expr = p.parse_expression().unwrap();
        let result = i.expression(expr).unwrap();
        assert_eq!(result, 7);
    }

    #[test]
    fn test_variable() {
        let mut p = Parser::new("x = 4/2 - 1");
        let mut i = Interpreter::new();
        let expr = p.parse_expression().unwrap();
        let result = i.expression(expr).unwrap();
        assert_eq!(result, 1);

        let expr = Parser::new("x").parse_expression().unwrap();
        let result = i.expression(expr).unwrap();
        assert_eq!(result, 1);
    }

    #[test]
    fn test_division_by_zero() {
        let mut p = Parser::new("x = 1 / 0");
        let mut i = Interpreter::new();
        let expr = p.parse_expression().unwrap();
        let result = i.expression(expr);
        assert!(result.is_err());
    }
}
