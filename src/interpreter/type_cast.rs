use crate::error::Error;
use crate::interpreter::{VarType, Variable};

pub fn cast_to_int(variable: &Variable) -> Result<i32, Error> {
    match variable {
        Variable::Int(i) => Ok(*i),
        _ => Err(Error::TypeMismatch(variable.var_type(), VarType::Int)),
    }
}

pub fn cast_to_float(variable: &Variable) -> Result<f64, Error> {
    match variable {
        Variable::Int(i) => Ok(*i as f64),
        Variable::Float(f) => Ok(*f),
        _ => Err(Error::TypeMismatch(variable.var_type(), VarType::Float)),
    }
}

pub fn cast_to_string(variable: &Variable) -> Result<String, Error> {
    match variable {
        Variable::Int(i) => Ok(i.to_string()),
        Variable::Float(f) => Ok(f.to_string()),
        Variable::String(s) => Ok(s.clone()),
        _ => Err(Error::TypeMismatch(variable.var_type(), VarType::String)),
    }
}
