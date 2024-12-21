use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum VarType {
    Bool,
    Int,
    Float,
    String,
    Empty,
    Array(Box<VarType>),
}

impl VarType {
    pub fn root_type(&self) -> VarType {
        match self {
            VarType::Array(t) => t.root_type(),
            _ => self.clone(),
        }
    }

    pub fn depth(&self) -> usize {
        match self {
            VarType::Array(t) => 1 + t.depth(),
            _ => 0,
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Bool => write!(f, "Bool"),
            VarType::Int => write!(f, "Int"),
            VarType::Float => write!(f, "Float"),
            VarType::String => write!(f, "String"),
            VarType::Empty => write!(f, "Empty"),
            VarType::Array(t) => write!(f, "Array<{}>", *t),
        }
    }
}
