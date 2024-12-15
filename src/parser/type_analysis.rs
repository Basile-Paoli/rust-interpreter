use crate::error::Error;
use crate::interpreter::VarType;
use crate::lexer::Op;
use crate::parser::Expression;

pub fn plus_type(a: VarType, b: VarType) -> Result<VarType, Error> {
    match (a, b) {
        (VarType::Int, VarType::Int) => Ok(VarType::Int),
        (VarType::Float, VarType::Float)
        | (VarType::Int, VarType::Float)
        | (VarType::Float, VarType::Int) => Ok(VarType::Float),
        _ => Ok(VarType::String),
    }
}

pub fn minus_type(a: VarType, b: VarType) -> Result<VarType, Error> {
    match (&a, &b) {
        (VarType::Int, VarType::Int) => Ok(VarType::Int),
        (VarType::Float, VarType::Float)
        | (VarType::Int, VarType::Float)
        | (VarType::Float, VarType::Int) => Ok(VarType::Float),
        _ => Err(Error::TypeMismatch(a, b)),
    }
}

pub fn mul_type(a: VarType, b: VarType) -> Result<VarType, Error> {
    match (&a, &b) {
        (VarType::Int, VarType::Int) => Ok(VarType::Int),
        (VarType::Float, VarType::Float)
        | (VarType::Int, VarType::Float)
        | (VarType::Float, VarType::Int) => Ok(VarType::Float),
        (VarType::String, VarType::Int) | (VarType::Int, VarType::String) => Ok(VarType::String),
        _ => Err(Error::TypeMismatch(a, b)),
    }
}

pub fn div_type(a: VarType, b: VarType) -> Result<VarType, Error> {
    match (&a, &b) {
        (VarType::Int, VarType::Int) => Ok(VarType::Int),
        (VarType::Float, VarType::Float)
        | (VarType::Int, VarType::Float)
        | (VarType::Float, VarType::Int) => Ok(VarType::Float),
        _ => Err(Error::TypeMismatch(a, b)),
    }
}

pub fn operation_type(op: Op, a: VarType, b: VarType) -> Result<VarType, Error> {
    match op {
        Op::ADD => plus_type(a, b),
        Op::SUB => minus_type(a, b),
        Op::MUL => mul_type(a, b),
        Op::DIV => div_type(a, b),
        Op::EQ => unimplemented!(),
    }
}

pub fn array_lit_type(elements: &Vec<Expression>) -> Result<VarType, Error> {
    if elements.is_empty() {
        return Ok(VarType::Empty);
    }

    let mut array_type = elements[0].expr_type();
    for element in elements {
        if element.expr_type().root_type() != VarType::Empty {
            if array_type.root_type() == VarType::Empty {
                array_type = element.expr_type();
            } else if element.expr_type() != array_type {
                return Err(Error::TypeMismatch(array_type, element.expr_type()));
            }
        }
    }
    Ok(array_type)
}