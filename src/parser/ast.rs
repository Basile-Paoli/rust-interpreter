use crate::lexer::Position;
use crate::parser::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode {
    pub node: Node,
    pub position: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Expression(Expression),
}
