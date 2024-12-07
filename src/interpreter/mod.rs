mod expression;

use crate::error::Error;
use crate::parser::{Expression, Instruction};
use std::collections::HashMap;
use std::io::{Stdout, Write};

type Variable = i32;

pub(crate) struct Interpreter<W: Write = Stdout> {
    variables: HashMap<String, Variable>,
    output: W,
}

impl Interpreter {
    pub fn new() -> Interpreter<Stdout> {
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
        }
    }

    fn expression_instruction(&mut self, expression: Expression) -> Result<(), Error> {
        match expression.clone() {
            Expression::Assignment(_) => self.expression(expression).map(|_| ()),
            _ => self.expression(expression).map(|result| {
                writeln!(self.output, "{}", result).unwrap_or(());
            }),
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
