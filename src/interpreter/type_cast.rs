use crate::interpreter::Variable;

pub fn cast_to_int(variable: Variable) -> Option<i32> {
    match variable {
        Variable::Int(i) => Some(i),
        Variable::Float(f) => Some(f as i32),
        _ => None,
    }
}

pub fn cast_to_float(variable: Variable) -> Option<f64> {
    match variable {
        Variable::Int(i) => Some(i as f64),
        Variable::Float(f) => Some(f),
        _ => None,
    }
}

