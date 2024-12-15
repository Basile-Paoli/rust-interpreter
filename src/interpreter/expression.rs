use crate::error::Error;
use crate::interpreter::array::Array;
use crate::interpreter::operators::{addition, division, multiplication, subtraction};
use crate::interpreter::type_cast::{cast_to_float, cast_to_int, cast_to_string};
use crate::interpreter::{Interpreter, Variable};
use crate::lexer::Op;
use crate::parser::{ArrayLit, Assignment, BinOp, Expression, LValue};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::io::Write;
use std::rc::Rc;

impl<W: Write> Interpreter<W> {
    pub fn expression(&mut self, expression: Expression) -> Result<Variable, Error> {
        match expression {
            Expression::BinOp(binop) => self.binop(binop),
            Expression::Int(int) => Ok(Variable::Int(int.value)),
            Expression::Float(float) => Ok(Variable::Float(float.value)),
            Expression::StringLit(string) => Ok(Variable::String(string.value)),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::LValue(lvalue) => Ok(self.lvalue(lvalue)?.clone()),
            Expression::Array(a) => Ok(self.array(a)?.clone()),
        }
    }

    fn binop(&mut self, binop: BinOp) -> Result<Variable, Error> {
        let left = self.expression(*binop.left)?;
        let right = self.expression(*binop.right)?;
        match binop.op {
            Op::ADD => Ok(addition(&left, &right)?),
            Op::SUB => Ok(subtraction(&left, &right)?),
            Op::MUL => Ok(multiplication(&left, &right)?),
            Op::DIV => division(&left, &right),
            Op::EQ => Ok(Variable::Int((left == right) as i32)),
        }
    }

    fn assignment(&mut self, assignment: Assignment) -> Result<Variable, Error> {
        let right = self.expression(*assignment.right)?;
        let left = self.lvalue(*assignment.left)?;

        let result = match assignment.op {
            Some(Op::ADD) => addition(&left, &right)?,
            Some(Op::SUB) => subtraction(&left, &right)?,
            Some(Op::MUL) => multiplication(&left, &right)?,
            Some(Op::DIV) => division(&left, &right)?,
            None => right,
            _ => unreachable!(),
        };
        assign(left, result)?;
        Ok(left.clone())
    }

    fn lvalue(&mut self, lvalue: LValue) -> Result<&mut Variable, Error> {
        match lvalue {
            LValue::Identifier(identifier) => {
                let name = identifier.name;
                match self.variables.entry(name.clone()) {
                    Entry::Occupied(o) => Ok(o.into_mut()),
                    Entry::Vacant(_) => Err(Error::VariableNotFound(name)),
                }
            }
        }
    }

    fn array(&mut self, a: ArrayLit) -> Result<Variable, Error> {
        let values = a
            .elements
            .iter()
            .map(|v| self.expression(v.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        let res = Array::new(values);
        Ok(Variable::Array(Rc::new(RefCell::new(res))))
    }
}

fn assign(variable: &mut Variable, res: Variable) -> Result<(), Error> {
    match variable {
        Variable::Empty => *variable = res,
        Variable::Float(_) => {
            let val = cast_to_float(&res)?;
            *variable = Variable::Float(val);
        }
        Variable::Int(_) => {
            let val = cast_to_int(&res)?;
            *variable = Variable::Int(val);
        }
        Variable::String(_) => {
            let val = cast_to_string(&res)?;
            *variable = Variable::String(val);
        }
        Variable::Array(a) => {
            a.borrow_mut().assign(res)?;
        }
    }
    Ok(())
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
        assert_eq!(result, Variable::Int(7));
    }

    #[test]
    fn test_division_by_zero() {
        let mut p = Parser::new("1 / 0");
        let mut i = Interpreter::new();
        let expr = p.expression().unwrap();
        let result = i.expression(expr);
        assert!(result.is_err());
    }

    #[test]
    fn test_assignment() {
        let mut p = Parser::new("let x = 2; x = x + 3;");
        let mut i = Interpreter::new();
        let instructions = p.parse().unwrap();
        i.run(instructions).unwrap();
        assert_eq!(i.variables.get("x").unwrap(), &Variable::Int(5));
    }

    // TODO Add support for empty variable declarations (once we have type declaration)
    // #[test]
    // fn test_assignment_to_empty() {
    //     let mut p = Parser::new("let x; x = 3;");
    //     let mut i = Interpreter::new();
    //     let instructions = p.parse().unwrap();
    //     i.run(instructions).unwrap();
    //     assert_eq!(i.variables.get("x").unwrap(), &Variable::Int(3));
    // }

    #[test]
    fn test_type_mismatch() {
        let mut p = Parser::new("let x = 3.0; x += \"3.0\";");
        let instructions = p.parse();
        assert!(instructions.is_err());
    }

    #[test]
    fn test_type_cast() {
        let mut p = Parser::new("let x = 2.0; x += 3;");
        let mut i = Interpreter::new();
        let instructions = p.parse().unwrap();
        i.run(instructions).unwrap();
        assert_eq!(i.variables.get("x").unwrap(), &Variable::Float(5.0));
    }

    #[test]
    fn test_array() {
        let mut p = Parser::new("[1, 2, 3]");
        let mut i = Interpreter::new();
        let expr = p.expression().unwrap();
        let result = i.expression(expr).unwrap();
        if let Variable::Array(array) = result {
            assert_eq!(array.borrow().elements.len(), 3);
            assert_eq!(array.borrow().elements[0], Variable::Int(1));
            assert_eq!(array.borrow().elements[1], Variable::Int(2));
            assert_eq!(array.borrow().elements[2], Variable::Int(3));
        } else {
            panic!("Expected Array node");
        }
    }

    #[test]
    fn test_nested_array() {
        let mut p = Parser::new("[[1, 2], [3, 4]]");
        let mut i = Interpreter::new();
        let expr = p.expression().unwrap();
        let result = i.expression(expr).unwrap();
        if let Variable::Array(array) = result {
            assert_eq!(array.borrow().elements.len(), 2);
            if let Variable::Array(inner) = &array.borrow().elements[0] {
                assert_eq!(inner.borrow().elements.len(), 2);
                assert_eq!(inner.borrow().elements[0], Variable::Int(1));
                assert_eq!(inner.borrow().elements[1], Variable::Int(2));
            } else {
                panic!("Expected Array node");
            }
            if let Variable::Array(inner) = &array.borrow().elements[1] {
                assert_eq!(inner.borrow().elements.len(), 2);
                assert_eq!(inner.borrow().elements[0], Variable::Int(3));
                assert_eq!(inner.borrow().elements[1], Variable::Int(4));
            } else {
                panic!("Expected Array node");
            }
        } else {
            panic!("Expected Array node");
        }
    }
}
