use crate::error::Error;
use crate::lexer::Lexer;
use crate::var_type::VarType;
use std::collections::HashMap;
use std::iter::Peekable;

mod display;
mod expression;
mod instruction;
mod type_analysis;
mod var_dec;

pub use crate::parser::instruction::{Block, BlockOrInstruction, IfStatement, Instruction};
pub use expression::{ArrayLit, Assignment, BinOp, Expression, Identifier, LValue};
pub use var_dec::VariableDeclaration;

#[macro_export]
macro_rules! expect_token {
    ($lexer:expr, $($token:pat => $ret_val:expr),*) => {
        match $lexer.next() {
            $(Some($token) => Ok($ret_val),)*
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::UnexpectedEof),
        }
    };

    ($lexer:expr, $token:pat) => {
        expect_token!($lexer, $token => ())
    };
}

pub struct Parser<'a> {
    pub lexer: Peekable<Lexer<'a>>,
    pub identifiers_stack: Vec<HashMap<String, VarType>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input).peekable(),
            identifiers_stack: vec![HashMap::new()],
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Instruction>, Error> {
        let mut instructions = Vec::new();
        while let Some(instruction) = self.parse_instruction()? {
            instructions.push(instruction);
        }
        Ok(instructions)
    }

    fn insert_identifier(&mut self, id: String, var_type: VarType) {
        self.identifiers_stack
            .last_mut()
            .unwrap()
            .insert(id, var_type);
    }

    fn identifier_exists_in_current_scope(&self, id: &str) -> bool {
        self.identifiers_stack.last().unwrap().contains_key(id)
    }

    fn get_identifier(&self, id: &str) -> Option<&VarType> {
        for identifiers in self.identifiers_stack.iter().rev() {
            if let Some(var_type) = identifiers.get(id) {
                return Some(var_type);
            }
        }
        None
    }
}
