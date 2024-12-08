use crate::error::Error;
use crate::interpreter::{Interpreter, Variable};
use crate::parser::VariableDeclaration;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn var_dec(&mut self, declaration: VariableDeclaration) -> Result<(), Error> {
        let val = match declaration.value {
            Some(expression) => self.expression(expression)?,
            None => Variable::Empty,
        };

        self.variables.insert(declaration.name, val);
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::Variable;

    #[test]
    fn test_var_dec() {
        use super::Interpreter;
        use crate::parser::Parser;

        let mut p = Parser::new("let x = 2;");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new();

        i.run(instructions).unwrap();
        assert_eq!(i.variables.get("x").unwrap(), &Variable::Int(2));
    }

    #[test]
    fn test_empty_var_dec() {
        use super::Interpreter;
        use crate::parser::Parser;

        let mut p = Parser::new("let x;");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new();

        i.run(instructions).unwrap();
        assert_eq!(i.variables.get("x").unwrap(), &Variable::Empty);
    }
}