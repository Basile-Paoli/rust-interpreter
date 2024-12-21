use crate::error::InterpreterError;
use crate::interpreter::{VarType, Variable};

pub fn cast_to_int(variable: &Variable) -> Result<i32, InterpreterError> {
    match variable {
        Variable::Int(i) => Ok(*i),
        _ => Err(InterpreterError::TypeMismatch(
            variable.var_type(),
            VarType::Int,
        )),
    }
}

pub fn cast_to_float(variable: &Variable) -> Result<f64, InterpreterError> {
    match variable {
        Variable::Int(i) => Ok(*i as f64),
        Variable::Float(f) => Ok(*f),
        _ => Err(InterpreterError::TypeMismatch(
            variable.var_type(),
            VarType::Float,
        )),
    }
}

pub fn cast_to_string(variable: &Variable) -> Result<String, InterpreterError> {
    match variable {
        Variable::Int(i) => Ok(i.to_string()),
        Variable::Float(f) => Ok(f.to_string()),
        Variable::String(s) => Ok(s.clone()),
        _ => Err(InterpreterError::TypeMismatch(
            variable.var_type(),
            VarType::String,
        )),
    }
}

pub fn cast_to_bool(variable: &Variable) -> Result<bool, InterpreterError> {
    match variable {
        Variable::Bool(b) => Ok(*b),
        _ => Err(InterpreterError::TypeMismatch(
            variable.var_type(),
            VarType::Bool,
        )),
    }
}
