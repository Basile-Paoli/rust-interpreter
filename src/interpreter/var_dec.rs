use crate::error::Error;
use crate::interpreter::{Interpreter, Variable};
use crate::parser::VariableDeclaration;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn var_dec(&mut self, declaration: &VariableDeclaration) -> Result<(), Error> {
        let val = match declaration.value {
            Some(ref expr) => self.expression(expr)?,
            None => Variable::Empty,
        };

        match self
            .current_variables()
            .insert(declaration.name.clone(), val)
        {
            Some(_) => Err(Error::VariableAlreadyExists(
                declaration.name.clone(),
                declaration.position,
            )),
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
        let mut i = Interpreter::new(Vec::new());

        i.run(instructions).unwrap();
        assert_eq!(i.current_variables().get("x").unwrap(), &Variable::Int(2));
    }

    #[test]
    fn test_empty_var_dec() {
        use super::Interpreter;
        use crate::parser::Parser;

        let mut p = Parser::new("let x: int;");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new(Vec::new());

        i.run(instructions).unwrap();
        assert_eq!(i.current_variables().get("x").unwrap(), &Variable::Empty);
    }
}
