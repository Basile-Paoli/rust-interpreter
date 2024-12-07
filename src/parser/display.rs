use crate::parser::{Assignment, BinOp, Expression, Identifier, Instruction, Int, LValue};
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
        write!(f, "{:indent$}Instruction: \n", "", indent = indent)?;
        match self {
            Instruction::Expression(expression) => expression.ast_fmt(f, indent + INDENT_SIZE),
        }
    }
}

impl AstDisplay for Expression {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}Expression: \n", "", indent = indent)?;
        match self {
            Expression::BinOp(binop) => binop.ast_fmt(f, indent + INDENT_SIZE),
            Expression::Int(int) => int.ast_fmt(f, indent + INDENT_SIZE),
            Expression::Assignment(assignment) => assignment.ast_fmt(f, indent + INDENT_SIZE),
            Expression::LValue(lvalue) => lvalue.ast_fmt(f, indent + INDENT_SIZE),
        }
    }
}

impl AstDisplay for Assignment {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}Assignment: \n", "", indent = indent)?;
        write!(
            f,
            "{:indent$}Operator: {:?}\n",
            "",
            self.op,
            indent = indent + INDENT_SIZE
        )?;
        write!(f, "{:indent$}Left: \n", "", indent = indent + INDENT_SIZE)?;
        (*self.left).ast_fmt(f, indent + INDENT_SIZE * 2)?;
        write!(f, "{:indent$}Right: \n", "", indent = indent + INDENT_SIZE)?;
        self.right.ast_fmt(f, indent + INDENT_SIZE * 2)
    }
}

impl AstDisplay for LValue {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}LValue: \n", "", indent = indent)?;
        match self {
            LValue::Identifier(identifier) => identifier.ast_fmt(f, indent + INDENT_SIZE),
        }
    }
}

impl AstDisplay for Identifier {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(
            f,
            "{:indent$}Identifier: {}\n",
            "",
            self.name,
            indent = indent
        )
    }
}

impl AstDisplay for Int {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}Int: {}\n", "", self.value, indent = indent)
    }
}

impl AstDisplay for BinOp {
    fn ast_fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        write!(f, "{:indent$}BinOp: \n", "", indent = indent)?;
        write!(
            f,
            "{:indent$}Operator: {:?}\n",
            "",
            self.op,
            indent = indent + INDENT_SIZE
        )?;
        write!(f, "{:indent$}Left: \n", "", indent = indent + INDENT_SIZE)?;
        self.left.ast_fmt(f, indent + INDENT_SIZE * 2)?;
        write!(f, "{:indent$}Right: \n", "", indent = indent + INDENT_SIZE)?;
        self.right.ast_fmt(f, indent + INDENT_SIZE * 2)
    }
}
