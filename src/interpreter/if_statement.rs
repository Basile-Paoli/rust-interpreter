use crate::error::Error;
use crate::interpreter::variable::Variable;
use crate::interpreter::{Scope, Interpreter};
use crate::parser::IfStatement;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn if_statement(&mut self, statement: &IfStatement) -> Result<(), Error> {
        let condition = self.expression(&statement.condition)?;
        if condition == Variable::Bool(true) {
            self.block_or_instruction(&statement.body, Scope::IfStatement)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::variable::Variable;
    use crate::interpreter::Interpreter;
    use crate::parser::Parser;

    #[test]
    fn test_if_statement() {
        let mut p = Parser::new("let x = 3; if (true) { x = 2; }");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new(Vec::new());

        i.run(instructions).unwrap();
        assert_eq!(i.current_variables().get("x").unwrap(), &Variable::Int(2));
    }

    #[test]
    fn test_false_if_statement() {
        let mut p = Parser::new("let x = 3; if (false) { x = 2; }");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new(Vec::new());

        i.run(instructions).unwrap();
        assert_eq!(i.current_variables().get("x").unwrap(), &Variable::Int(3));
    }
}
