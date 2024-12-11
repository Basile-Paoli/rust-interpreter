use crate::error::Error;
use crate::interpreter::type_cast::{cast_to_float, cast_to_string};
use crate::interpreter::Variable;

pub fn addition(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    match (left, right) {
        (Variable::Int(l), Variable::Int(r)) => Ok(Variable::Int(l + r)),
        (Variable::Float(_), Variable::Int(_)) => float_addition(left, right),
        (Variable::Int(_), Variable::Float(_)) => float_addition(left, right),
        (Variable::Float(_), Variable::Float(_)) => float_addition(left, right),
        _ => string_addition(left, right),
    }
}

fn float_addition(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    let a = cast_to_float(left)?;
    let b = cast_to_float(right)?;
    Ok(Variable::Float(a + b))
}

fn string_addition(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    let a = cast_to_string(left)?;
    let b = cast_to_string(right)?;
    Ok(Variable::String(a + &b))
}

pub fn subtraction(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    match (left, right) {
        (Variable::Int(l), Variable::Int(r)) => Ok(Variable::Int(l - r)),
        _ => float_subtraction(left, right),
    }
}

fn float_subtraction(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    let a = cast_to_float(left)?;
    let b = cast_to_float(right)?;
    Ok(Variable::Float(a - b))
}

pub fn multiplication(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    match (left, right) {
        (Variable::Int(l), Variable::Int(r)) => Ok(Variable::Int(l * r)),
        (Variable::String(s), Variable::Int(i)) | (Variable::Int(i), Variable::String(s)) => {
            string_multiplication(s.clone(), *i)
        }
        _ => float_multiplication(left, right),
    }
}

fn float_multiplication(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    let a = cast_to_float(left)?;
    let b = cast_to_float(right)?;
    Ok(Variable::Float(a * b))
}

fn string_multiplication(s: String, i: i32) -> Result<Variable, Error> {
    Ok(Variable::String(s.repeat(i as usize)))
}

pub fn division(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    match (left, right) {
        (Variable::Int(l), Variable::Int(r)) => int_division(*l, *r),
        _ => float_division(left, right),
    }
}

fn int_division(left: i32, right: i32) -> Result<Variable, Error> {
    if right == 0 {
        Err(Error::DivisionByZero)
    } else {
        Ok(Variable::Int(left / right))
    }
}

fn float_division(left: &Variable, right: &Variable) -> Result<Variable, Error> {
    let a = cast_to_float(left)?;
    let b = cast_to_float(right)?;
    if b == 0.0 {
        Err(Error::DivisionByZero)
    } else {
        Ok(Variable::Float(a / b))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_addition() {
        assert_eq!(
            addition(&Variable::Int(1), &Variable::Int(2)).unwrap(),
            Variable::Int(3)
        );
        assert_eq!(
            addition(&Variable::Float(1.0), &Variable::Float(2.0)).unwrap(),
            Variable::Float(3.0)
        );
        assert_eq!(
            addition(&Variable::Float(1.0), &Variable::Int(2)).unwrap(),
            Variable::Float(3.0)
        );
        assert_eq!(
            addition(
                &Variable::String("a".to_string()),
                &Variable::String("b".to_string())
            )
            .unwrap(),
            Variable::String("ab".to_string())
        );
        assert_eq!(
            addition(&Variable::String("a".to_string()), &Variable::Int(1)).unwrap(),
            Variable::String("a1".to_string())
        )
    }

    #[test]
    fn test_subtraction() {
        assert_eq!(
            subtraction(&Variable::Int(1), &Variable::Int(2)).unwrap(),
            Variable::Int(-1)
        );
        assert_eq!(
            subtraction(&Variable::Float(1.0), &Variable::Float(2.0)).unwrap(),
            Variable::Float(-1.0)
        );
        assert_eq!(
            subtraction(&Variable::Int(1), &Variable::Float(2.0)).unwrap(),
            Variable::Float(-1.0)
        );
        assert_eq!(
            subtraction(&Variable::Float(1.0), &Variable::Int(2)).unwrap(),
            Variable::Float(-1.0)
        );
    }

    #[test]
    fn test_multiplication() {
        assert_eq!(
            multiplication(&Variable::Int(2), &Variable::Int(3)).unwrap(),
            Variable::Int(6)
        );
        assert_eq!(
            multiplication(&Variable::Float(2.0), &Variable::Float(3.0)).unwrap(),
            Variable::Float(6.0)
        );
        assert_eq!(
            multiplication(&Variable::Int(2), &Variable::Float(3.0)).unwrap(),
            Variable::Float(6.0)
        );
        assert_eq!(
            multiplication(&Variable::String("a".to_string()), &Variable::Int(3)).unwrap(),
            Variable::String("aaa".to_string())
        )
    }

    #[test]
    fn test_division() {
        assert_eq!(
            division(&Variable::Int(6), &Variable::Int(3)).unwrap(),
            Variable::Int(2)
        );
        assert_eq!(
            division(&Variable::Float(6.0), &Variable::Float(3.0)).unwrap(),
            Variable::Float(2.0)
        );
        assert_eq!(
            division(&Variable::Int(6), &Variable::Float(3.0)).unwrap(),
            Variable::Float(2.0)
        );
        assert_eq!(
            division(&Variable::Float(6.0), &Variable::Int(3)).unwrap(),
            Variable::Float(2.0)
        );
    }

    #[test]
    fn test_division_by_zero() {
        assert_eq!(
            division(&Variable::Int(1), &Variable::Int(0)),
            Err(Error::DivisionByZero)
        );
        assert_eq!(
            division(&Variable::Float(1.0), &Variable::Float(0.0)),
            Err(Error::DivisionByZero)
        );
        assert_eq!(
            division(&Variable::Int(1), &Variable::Float(0.0)),
            Err(Error::DivisionByZero)
        );
        assert_eq!(
            division(&Variable::Float(1.0), &Variable::Int(0)),
            Err(Error::DivisionByZero)
        );
    }

    #[test]
    fn test_type_mismatch() {
        assert!(matches!(
            addition(&Variable::Empty, &Variable::Int(1)),
            Err(Error::TypeMismatch(_, _))
        ));
        assert!(matches!(
            subtraction(&Variable::Empty, &Variable::Int(1)),
            Err(Error::TypeMismatch(_, _))
        ));
        assert!(matches!(
            multiplication(&Variable::Empty, &Variable::Int(1)),
            Err(Error::TypeMismatch(_, _))
        ));
        assert!(matches!(
            division(&Variable::Empty, &Variable::Int(1)),
            Err(Error::TypeMismatch(_, _))
        ));
    }
}
