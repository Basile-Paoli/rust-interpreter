use crate::error::{Error, ToErrorResult};
use crate::interpreter::type_cast::cast_to_bool;
use crate::interpreter::{Interpreter, Scope};
use crate::parser::IfStatement;
use std::io::Write;

impl<W: Write> Interpreter<W> {
    pub fn if_statement(&mut self, statement: &IfStatement) -> Result<(), Error> {
        let condition = self.expression(&statement.condition)?;
        let bool_condition =
            cast_to_bool(&condition).to_error_result(statement.condition.position())?;
        if bool_condition {
            self.block_or_instruction(&statement.body, Scope::IfStatement)?;
        } else if let Some(else_body) = &statement.else_body {
            self.block_or_instruction(else_body, Scope::IfStatement)?;
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
    
    #[test]
    fn test_if_else_statement() {
        let mut p = Parser::new("let x = 3; if (false) { x = 2; } else { x = 5; }");
        let instructions = p.parse().unwrap();
        let mut i = Interpreter::new(Vec::new());

        i.run(instructions).unwrap();
        assert_eq!(i.current_variables().get("x").unwrap(), &Variable::Int(5));
    }
}
