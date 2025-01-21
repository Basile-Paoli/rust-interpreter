use crate::interpreter::array::ArrayVariable;
use crate::var_type::VarType;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Bool(bool),
    Int(i32),
    Float(f64),
    String(String),
    Array(ArrayVariable),
    Empty,
}
impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Bool(b) => write!(f, "{}", b),
            Variable::Int(i) => write!(f, "{}", i),
            Variable::Float(fl) => write!(f, "{}", fl),
            Variable::String(s) => write!(f, "{}", s),
            Variable::Array(a) => write!(f, "{}", a.borrow()),
            Variable::Empty => write!(f, "(empty)"),
        }
    }
}

impl Variable {
    pub fn var_type(&self) -> VarType {
        match self {
            Variable::Bool(_) => VarType::Bool,
            Variable::Int(_) => VarType::Int,
            Variable::Float(_) => VarType::Float,
            Variable::String(_) => VarType::String,
            Variable::Array(a) => VarType::Array(Box::new(a.borrow().elem_type.clone())),
            Variable::Empty => VarType::Empty,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_root_type() {
        let int = VarType::Int;
        let array_int = VarType::Array(Box::new(VarType::Int));
        let array_empty = VarType::Array(Box::new(VarType::Empty));
        let array_array_int = VarType::Array(Box::new(VarType::Array(Box::new(VarType::Int))));
        let array_array_empty = VarType::Array(Box::new(VarType::Array(Box::new(VarType::Empty))));

        assert_eq!(int.root_type(), VarType::Int);
        assert_eq!(array_int.root_type(), VarType::Int);
        assert_eq!(array_empty.root_type(), VarType::Empty);
        assert_eq!(array_array_int.root_type(), VarType::Int);
        assert_eq!(array_array_empty.root_type(), VarType::Empty);
    }
}
