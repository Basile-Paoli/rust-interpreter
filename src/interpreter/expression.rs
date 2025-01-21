use crate::error::{Error, InterpreterError, ToErrorResult};
use crate::interpreter::array::Array;
use crate::interpreter::operators::{addition, division, multiplication, subtraction};
use crate::interpreter::type_cast::{cast_to_bool, cast_to_float, cast_to_int, cast_to_string};
use crate::interpreter::{Interpreter, Variable};
use crate::lexer::Op;
use crate::parser::{ArrayLit, Assignment, BinOp, Expression, LValue};
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

impl<W: Write> Interpreter<W> {
    pub fn expression(&mut self, expression: &Expression) -> Result<Variable, Error> {
        match expression {
            Expression::BinOp(binop) => self.binop(binop),
            Expression::Bool(val, _) => Ok(Variable::Bool(*val)),
            Expression::Int(val, _) => Ok(Variable::Int(*val)),
            Expression::Float(val, _) => Ok(Variable::Float(*val)),
            Expression::StringLit(val, _) => Ok(Variable::String(val.clone())),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::LValue(lvalue) => Ok(self.lvalue(lvalue)?.clone()),
            Expression::Array(a) => Ok(self.array(a)?.clone()),
        }
    }

    fn binop(&mut self, binop: &BinOp) -> Result<Variable, Error> {
        let left = self.expression(&*binop.left)?;
        let right = self.expression(&*binop.right)?;
        match binop.op {
            Op::ADD => addition(&left, &right),
            Op::SUB => subtraction(&left, &right),
            Op::MUL => multiplication(&left, &right),
            Op::DIV => division(&left, &right),
            Op::EQ => Ok(Variable::Int((left == right) as i32)),
        }
        .to_error_result(binop.position)
    }

    fn assignment(&mut self, assignment: &Assignment) -> Result<Variable, Error> {
        let right = self.expression(&assignment.right)?;
        let left = self.lvalue(&assignment.left)?;

        let result = match assignment.op {
            Some(Op::ADD) => addition(&left, &right),
            Some(Op::SUB) => subtraction(&left, &right),
            Some(Op::MUL) => multiplication(&left, &right),
            Some(Op::DIV) => division(&left, &right),
            None => Ok(right),
            _ => unreachable!(),
        }
        .to_error_result(assignment.position)?;
        assign(left, result).to_error_result(assignment.position)?;
        Ok(left.clone())
    }

    fn lvalue(&mut self, lvalue: &LValue) -> Result<&mut Variable, Error> {
        match lvalue {
            LValue::Identifier(identifier) => {
                let name = identifier.name.clone();
                self.get_variable(&name)
                    .to_error_result(identifier.position)
            }
        }
    }

    fn array(&mut self, a: &ArrayLit) -> Result<Variable, Error> {
        let values = a
            .elements
            .iter()
            .map(|v| self.expression(v))
            .collect::<Result<Vec<_>, _>>()?;
        let res = Array::new(values);
        Ok(Variable::Array(Rc::new(RefCell::new(res))))
    }
}

fn assign(variable: &mut Variable, res: Variable) -> Result<(), InterpreterError> {
    match variable {
        Variable::Empty => *variable = res,
        Variable::Bool(_) => {
            let val = cast_to_bool(&res)?;
            *variable = Variable::Bool(val);
        }
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
    use crate::interpreter::VarType;
    use crate::lexer::Position;
    use crate::parser::Parser;

    #[test]
    fn test_expression() {
        let mut p = Parser::new("1 + 2 * 3");
        let mut i = Interpreter::new(Vec::new());
        let expr = p.expression().unwrap();
        let result = i.expression(&expr).unwrap();
        assert_eq!(result, Variable::Int(7));
    }

    #[test]
    fn test_division_by_zero() {
        let mut p = Parser::new("1 / 0");
        let mut i = Interpreter::new(Vec::new());
        let expr = p.expression().unwrap();
        let result = i.expression(&expr);
        assert_eq!(
            result,
            Err(Error::DivisionByZero(Position { line: 1, column: 3 }))
        );
    }

    #[test]
    fn test_assignment() {
        let mut p = Parser::new("let x = 2; x = x + 3;");
        let mut i = Interpreter::new(Vec::new());
        let instructions = p.parse().unwrap();
        i.run(instructions).unwrap();
        assert_eq!(i.current_variables().get("x").unwrap(), &Variable::Int(5));
    }

    #[test]
    fn test_assignment_to_empty() {
        let mut p = Parser::new("let x: int; x = 3;");
        let mut i = Interpreter::new(Vec::new());
        let instructions = p.parse().unwrap();
        i.run(instructions).unwrap();
        assert_eq!(i.current_variables().get("x").unwrap(), &Variable::Int(3));
    }

    #[test]
    fn test_type_mismatch() {
        let mut p = Parser::new("let x = 3.0; x += \"3.0\";");
        let instructions = p.parse();
        assert_eq!(
            instructions,
            Err(Error::TypeMismatch(
                VarType::Float,
                VarType::String,
                Position {
                    line: 1,
                    column: 16
                }
            ))
        );
    }

    #[test]
    fn test_type_cast() {
        let mut p = Parser::new("let x = 2.0; x += 3;");
        let mut i = Interpreter::new(Vec::new());
        let instructions = p.parse().unwrap();
        i.run(instructions).unwrap();
        assert_eq!(
            i.current_variables().get("x").unwrap(),
            &Variable::Float(5.0)
        );
    }

    #[test]
    fn test_array() {
        let mut p = Parser::new("[1, 2, 3]");
        let mut i = Interpreter::new(Vec::new());
        let expr = p.expression().unwrap();
        let result = i.expression(&expr).unwrap();
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
        let mut i = Interpreter::new(Vec::new());
        let expr = p.expression().unwrap();
        let result = i.expression(&expr).unwrap();
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
