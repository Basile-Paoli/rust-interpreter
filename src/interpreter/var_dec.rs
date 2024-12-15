use crate::error::Error;
use crate::interpreter::{Interpreter, Variable};
use crate::parser::VariableDeclaration;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn var_dec(&mut self, declaration: VariableDeclaration) -> Result<(), Error> {
        let val = declaration
            .value
            .map_or(Ok(Variable::Empty), |v| self.expression(v))?;

        match self.variables.insert(declaration.name.clone(), val) {
            Some(_) => Err(Error::VariableAlreadyExists(declaration.name)),
            None => Ok(()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

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

        let mut p = Parser::new("let x: int;");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new();

        i.run(instructions).unwrap();
        assert_eq!(i.variables.get("x").unwrap(), &Variable::Empty);
    }

    #[test]
    fn test_var_dec_error() {
        use super::Interpreter;
        use crate::parser::Parser;

        let mut p = Parser::new("let x = 2; let x = 3;");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new();

        let result = i.run(instructions);
        assert_eq!(result, Err(Error::VariableAlreadyExists("x".to_string())));
    }
}
