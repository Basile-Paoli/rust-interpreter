use crate::parser::expression::Float;
use crate::parser::{
    Assignment, BinOp, Expression, Identifier, Instruction, Int, LValue, VariableDeclaration,
};
use std::fmt::Display;

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ast_fmt(f, 0)
    }
}

pub trait AstDisplay {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result;
}

const INDENT_SIZE: usize = 2;

impl AstDisplay for Instruction {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}Instruction: \n", "")?;
        match self {
            Instruction::Expression(expression) => expression.ast_fmt(f, indent + INDENT_SIZE),
            Instruction::VariableDeclaration(declaration) => {
                declaration.ast_fmt(f, indent + INDENT_SIZE)
            }
        }
    }
}

impl AstDisplay for Expression {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        match self {
            Expression::BinOp(binop) => binop.ast_fmt(f, indent),
            Expression::Int(int) => int.ast_fmt(f, indent),
            Expression::Float(float) => float.ast_fmt(f, indent),
            Expression::Assignment(assignment) => assignment.ast_fmt(f, indent),
            Expression::LValue(lvalue) => lvalue.ast_fmt(f, indent),
        }
    }
}

impl AstDisplay for Assignment {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(
            f,
            "{:indent$}Assignment{}:\n",
            "",
            self.op
                .map(|op| format!(" ({})", op))
                .unwrap_or("".to_string())
        )?;
        (*self.left).ast_fmt(f, indent + INDENT_SIZE)?;
        self.right.ast_fmt(f, indent + INDENT_SIZE)
    }
}

impl AstDisplay for LValue {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        match self {
            LValue::Identifier(identifier) => identifier.ast_fmt(f, indent),
        }
    }
}

impl AstDisplay for Identifier {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}Identifier: {}\n", "", self.name,)
    }
}

impl AstDisplay for Int {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}Int: {}\n", "", self.value)
    }
}

impl AstDisplay for Float {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}Float: {}\n", "", self.value)
    }
}

impl AstDisplay for BinOp {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}{}: \n", "", self.op)?;
        self.left.ast_fmt(f, indent + INDENT_SIZE)?;
        self.right.ast_fmt(f, indent + INDENT_SIZE)
    }
}

impl AstDisplay for VariableDeclaration {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}VariableDeclaration: {}\n", "", self.name)?;
        if let Some(value) = &self.value {
            value.ast_fmt(f, indent + INDENT_SIZE)
        } else {
            Ok(())
        }
    }
}
