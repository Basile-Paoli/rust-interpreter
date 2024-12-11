mod array;
mod expression;
mod operators;
mod type_cast;
mod var_dec;

use crate::error::Error;
use crate::interpreter::array::ArrayVariable;
use crate::parser::{Expression, Instruction};
use std::collections::HashMap;
use std::fmt::Display;
use std::io::{Stdout, Write};

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Int(i32),
    Float(f64),
    String(String),
    Array(ArrayVariable),
    Empty,
}
impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Int(i) => write!(f, "{}", i),
            Variable::Float(fl) => write!(f, "{}", fl),
            Variable::String(s) => write!(f, "{}", s),
            Variable::Array(a) => write!(f, "{}", a.borrow()),
            Variable::Empty => write!(f, "(empty)"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarType {
    Int,
    Float,
    String,
    Empty,
    Array(Box<VarType>),
}

impl VarType {
    pub fn root_type(&self) -> VarType {
        match self {
            VarType::Array(t) => t.root_type(),
            _ => self.clone(),
        }
    }
}

impl Variable {
    pub(crate) fn var_type(&self) -> VarType {
        match self {
            Variable::Int(_) => VarType::Int,
            Variable::Float(_) => VarType::Float,
            Variable::String(_) => VarType::String,
            Variable::Array(a) => VarType::Array(Box::new(a.borrow().elem_type.clone())),
            Variable::Empty => VarType::Empty,
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Int => write!(f, "Int"),
            VarType::Float => write!(f, "Float"),
            VarType::String => write!(f, "String"),
            VarType::Empty => write!(f, "Empty"),
            VarType::Array(t) => write!(f, "Array<{}>", *t),
        }
    }
}
pub struct Interpreter<W: Write = Stdout> {
    variables: HashMap<String, Variable>,
    output: W,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            variables: HashMap::new(),
            output: std::io::stdout(),
        }
    }
}

impl<W: Write> Interpreter<W> {
    pub fn run(&mut self, instructions: Vec<Instruction>) -> Result<(), Error> {
        for instruction in instructions {
            self.instruction(instruction)?;
        }
        Ok(())
    }

    fn instruction(&mut self, instruction: Instruction) -> Result<(), Error> {
        match instruction {
            Instruction::Expression(expression) => self.expression_instruction(expression),
            Instruction::VariableDeclaration(declaration) => self.var_dec(declaration),
        }
    }

    fn expression_instruction(&mut self, expression: Expression) -> Result<(), Error> {
        match expression.clone() {
            Expression::Assignment(_) => self.expression(expression).map(|_| ()),
            _ => self
                .expression(expression)
                .and_then(|result| writeln!(self.output, "{}", result).map_err(|_| Error::IoError)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Parser;
    use std::io::Cursor;

    #[test]
    fn test_output() {
        let mut p = Parser::new("2 + 3;");
        let instructions = p.parse().unwrap();
        let output = Cursor::new(Vec::new());
        let mut i = Interpreter {
            variables: HashMap::new(),
            output,
        };

        i.run(instructions).unwrap();
        assert_eq!(i.output.into_inner(), b"5\n");
    }

    #[test]
    fn test_root_type() {
        let int = VarType::Int;
        let array_int = VarType::Array(Box::new(VarType::Int));
        let array_empty = VarType::Array(Box::new(VarType::Empty));
        let array_array_int = VarType::Array(Box::new(VarType::Array(Box::new(VarType::Int))));
        let array_array_empty = VarType::Array(Box::new(VarType::Array(Box::new(VarType::Empty))));

        assert_eq!(int.root_type(), VarType::Int);
        assert_eq!(array_int.root_type(), VarType::Int);
        assert_eq!(array_empty.root_type(), VarType::Empty);
        assert_eq!(array_array_int.root_type(), VarType::Int);
        assert_eq!(array_array_empty.root_type(), VarType::Empty);
    }
}
