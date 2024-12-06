use crate::parser::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub enum AstNode {
    Expression(Expression),
}
