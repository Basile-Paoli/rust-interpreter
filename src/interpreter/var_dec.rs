use crate::error::Error;
use crate::interpreter::Interpreter;
use crate::parser::VariableDeclaration;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn var_dec(&mut self, declaration: VariableDeclaration) -> Result<(), Error> {
        let val = self.expression(declaration.value)?;

        match self.variables.insert(declaration.name.clone(), val) {
            Some(_) => Err(Error::VariableAlreadyExists(declaration.name)),
            None => Ok(()),
        }
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

    // TODO Add support for empty variable declarations (once we have type declaration)
    // #[test]
    // fn test_empty_var_dec() {
    //     use super::Interpreter;
    //     use crate::parser::Parser;
    //
    //     let mut p = Parser::new("let x;");
    //     let instructions = p.parse().unwrap();
    //     let mut i = Interpreter::new();
    //
    //     i.run(instructions).unwrap();
    //     assert_eq!(i.variables.get("x").unwrap(), &Variable::Empty);
    // }

    #[test]
    fn test_var_dec_error() {
        use super::Interpreter;
        use crate::parser::Parser;

        let mut p = Parser::new("let x = 2; let x = 3;");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new();

        let result = i.run(instructions);
        assert!(result.is_err());
    }
}
