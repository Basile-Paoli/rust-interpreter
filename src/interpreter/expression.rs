use crate::error::Error;
use crate::interpreter::operators::{addition, division, multiplication, subtraction};
use crate::interpreter::type_cast::{cast_to_float, cast_to_int};
use crate::interpreter::{Interpreter, Variable};
use crate::lexer::Op;
use crate::parser::{Assignment, BinOp, Expression, LValue};
use std::collections::hash_map::Entry;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn expression(&mut self, expression: Expression) -> Result<Variable, Error> {
        match expression {
            Expression::BinOp(binop) => self.binop(binop),
            Expression::Int(int) => Ok(Variable::Int(int.value)),
            Expression::Float(float) => Ok(Variable::Float(float.value)),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::LValue(lvalue) => Ok(*self.lvalue(lvalue)?),
        }
    }

    fn binop(&mut self, binop: BinOp) -> Result<Variable, Error> {
        let left = self.expression(*binop.left)?;
        let right = self.expression(*binop.right)?;
        match binop.op {
            Op::ADD => Ok(addition(left, right)?),
            Op::SUB => Ok(subtraction(left, right)?),
            Op::MUL => Ok(multiplication(left, right)?),
            Op::DIV => division(left, right),
            Op::EQ => Ok(Variable::Int((left == right) as i32)),
        }
    }

    fn assignment(&mut self, assignment: Assignment) -> Result<Variable, Error> {
        let right = self.expression(*assignment.right)?;
        let left = self.lvalue(*assignment.left)?;

        let result = match assignment.op {
            Some(Op::ADD) => addition(*left, right)?,
            Some(Op::SUB) => subtraction(*left, right)?,
            Some(Op::MUL) => multiplication(*left, right)?,
            Some(Op::DIV) => division(*left, right)?,
            None => right,
            _ => unreachable!(),
        };
        assign(left, result)?;
        Ok(*left)
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
}

fn assign(variable: &mut Variable, res: Variable) -> Result<(), Error> {
    match variable {
        Variable::Empty => *variable = res,
        Variable::Float(_) => {
            let val = cast_to_float(res).ok_or(Error::TypeMismatch(*variable, res))?;
            *variable = Variable::Float(val);
        }
        Variable::Int(_) => {
            let val = cast_to_int(res).ok_or(Error::TypeMismatch(*variable, res))?;
            *variable = Variable::Int(val);
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

    #[test]
    fn test_assignment_to_empty() {
        let mut p = Parser::new("let x; x = 3;");
        let mut i = Interpreter::new();
        let instructions = p.parse().unwrap();
        i.run(instructions).unwrap();
        assert_eq!(i.variables.get("x").unwrap(), &Variable::Int(3));
    }

    #[test]
    fn test_type_mismatch() {
        let mut p = Parser::new("let x; x += 3.0;");
        let mut i = Interpreter::new();
        let instructions = p.parse().unwrap();
        let result = i.run(instructions);
        assert!(result.is_err());
    }

    #[test]
    fn test_type_cast() {
        let mut p = Parser::new("let x = 2.0; x += 3;");
        let mut i = Interpreter::new();
        let instructions = p.parse().unwrap();
        i.run(instructions).unwrap();
        assert_eq!(i.variables.get("x").unwrap(), &Variable::Float(5.0));
    }
}
