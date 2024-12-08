mod expression;
mod operators;
mod type_cast;
mod var_dec;

use crate::error::Error;
use crate::parser::{Expression, Instruction};
use std::collections::HashMap;
use std::fmt::Display;
use std::io::{Stdout, Write};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum Variable {
    Int(i32),
    Float(f64),
    Empty,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Int(i) => write!(f, "{}", i),
            Variable::Float(fl) => write!(f, "{}", fl),
            Variable::Empty => write!(f, "(empty)"),
        }
    }
}

pub(super) struct Interpreter<W: Write = Stdout> {
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
    use super::{HashMap, Interpreter};
    use crate::parser::Parser;
    use std::io::Cursor;

    #[test]
    fn test_output() {
        let mut p = Parser::new("2;");
        let instructions = p.parse().unwrap();
        let output = Cursor::new(Vec::new());
        let mut i = Interpreter {
            variables: HashMap::new(),
            output,
        };

        i.run(instructions).unwrap();
        assert_eq!(i.output.into_inner(), b"2\n");
    }
}
