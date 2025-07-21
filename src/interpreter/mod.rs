mod array;
mod expression;
mod if_statement;
mod operators;
mod type_cast;
mod var_dec;
mod variable;

use crate::error::{Error, InterpreterError};
use crate::parser::{Block, BlockOrInstruction, Expression, Instruction};
use crate::var_type::VarType;
use std::collections::HashMap;
use std::io::Write;
use variable::Variable;

#[derive(Debug, PartialEq, Clone)]
enum CallStackEntry {
    Instruction(Instruction),
    EndBlock,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Scope {
    IfStatement,
    Function,
    Global,
}

pub struct Interpreter<W: Write> {
    call_stack: Vec<CallStackEntry>,
    variable_stack: Vec<(HashMap<String, Variable>, Scope)>,
    output: W,
}

impl<W: Write> Interpreter<W> {
    pub fn new(output: W) -> Interpreter<W> {
        Interpreter {
            call_stack: Vec::new(),
            variable_stack: Vec::new(),
            output,
        }
    }

    pub fn current_variables(&mut self) -> &mut HashMap<String, Variable> {
        &mut self.variable_stack.last_mut().unwrap().0
    }

    pub fn get_variable(&mut self, name: &str) -> Result<&mut Variable, InterpreterError> {
        for variables in self.variable_stack.iter_mut().rev() {
            if let Some(var) = variables.0.get_mut(name) {
                return Ok(var);
            }

            if variables.1 == Scope::Function {
                break;
            }
        }
        Err(InterpreterError::VariableNotFound(name.to_string()))
    }

    pub fn run(&mut self, instructions: Vec<Instruction>) -> Result<(), Error> {
        for instruction in instructions.iter().rev() {
            self.call_stack
                .push(CallStackEntry::Instruction(instruction.clone()));
        }
        self.variable_stack.push((HashMap::new(), Scope::Global));
        while let Some(entry) = self.call_stack.pop() {
            match entry {
                CallStackEntry::Instruction(instruction) => self.instruction(&instruction)?,
                CallStackEntry::EndBlock => {
                    self.variable_stack.pop();
                }
            }
        }
        Ok(())
    }

    fn instruction(&mut self, instruction: &Instruction) -> Result<(), Error> {
        match instruction {
            Instruction::Expression(expression) => self.expression_instruction(expression),
            Instruction::VariableDeclaration(declaration) => self.var_dec(declaration),
            Instruction::IfStatement(if_statement) => self.if_statement(if_statement),
        }
    }

    fn block(&mut self, block: &Block, context: Scope) -> Result<(), Error> {
        self.variable_stack.push((HashMap::new(), context));
        self.call_stack.push(CallStackEntry::EndBlock);
        for instruction in block.iter().rev() {
            self.call_stack
                .push(CallStackEntry::Instruction(instruction.clone()));
        }
        Ok(())
    }

    fn block_or_instruction(
        &mut self,
        body: &BlockOrInstruction,
        context: Scope,
    ) -> Result<(), Error> {
        match body {
            BlockOrInstruction::Block(block) => self.block(block, context),
            BlockOrInstruction::Instruction(instruction) => self.instruction(&instruction),
        }
    }

    fn expression_instruction(&mut self, expression: &Expression) -> Result<(), Error> {
        let position = expression.position();
        match expression {
            Expression::Assignment(_) => self.expression(expression).map(|_| ()),
            //TODO: if expression is not an assignment or function call, invalid
            _ => self.expression(expression).and_then(|result| {
                writeln!(self.output, "{}", result).map_err(|_| Error::IoError(position))
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
        let mut p = Parser::new("2 + 3;");
        let instructions = p.parse().unwrap();
        let output = Cursor::new(Vec::new());
        let mut i = Interpreter::new(output);
        i.run(instructions).unwrap();
        assert_eq!(i.output.into_inner(), b"5\n");
    }
}
