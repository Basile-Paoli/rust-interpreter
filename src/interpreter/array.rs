use crate::error::Error;
use crate::interpreter::{VarType, Variable};
use std::cell::{Ref, RefCell};
use std::fmt::Display;
use std::rc::Rc;

pub type ArrayVariable = Rc<RefCell<Array>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub elem_type: VarType,
    pub elements: Vec<Variable>,
}

impl Array {
    pub fn new(mut elements: Vec<Variable>) -> Self {
        let mut elem_type = match elements.first() {
            Some(value) => value.var_type(),
            None => VarType::Empty,
        };

        for element in &elements {
            if element.var_type() != elem_type && elem_type.root_type() == VarType::Empty {
                elem_type = element.var_type();
            }
        }

        if let VarType::Array(subtype) = &elem_type {
            fix_elements_type(subtype, &mut elements);
        }
        Array {
            elem_type,
            elements,
        }
    }

    pub fn push(&mut self, value: Variable) -> Result<(), Error> {
        match self.elem_type {
            VarType::Empty => {
                self.elem_type = value.var_type();
                Ok(self.elements.push(value))
            }
            _ => {
                if value.var_type() == self.elem_type {
                    Ok(self.elements.push(value))
                } else {
                    Err(Error::TypeMismatch(
                        value.var_type(),
                        self.elem_type.clone(),
                    ))
                }
            }
        }
    }

    pub fn assign(&mut self, variable: Variable) -> Result<(), Error> {
        match variable {
            Variable::Array(array) => self.assign_array(array.borrow()),
            _ => Err(Error::TypeMismatch(
                variable.var_type(),
                VarType::Array(Box::new(self.elem_type.clone())),
            )),
        }
    }

    fn assign_array(&mut self, array: Ref<Array>) -> Result<(), Error> {
        if self.elem_type != VarType::Empty
            && array.elem_type != VarType::Empty
            && array.elem_type != self.elem_type
        {
            Err(Error::TypeMismatch(
                array.elem_type.clone(),
                self.elem_type.clone(),
            ))
        } else {
            self.elem_type = if self.elem_type == VarType::Empty {
                array.elem_type.clone()
            } else {
                self.elem_type.clone()
            };
            Ok(self.elements = array.elements.clone())
        }
    }
}

fn fix_elements_type(elements_type: &VarType, array_vars: &mut Vec<Variable>) {
    for array_var in array_vars {
        if let Variable::Array(array) = array_var {
            let elem_type = array.borrow().elem_type.clone();
            if elem_type != *elements_type {
                array.borrow_mut().elem_type = elements_type.clone();
                if let VarType::Array(subtype) = elements_type {
                    fix_elements_type(subtype, &mut array.borrow_mut().elements);
                }
            }
        }
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, value) in self.elements.iter().enumerate() {
            write!(f, "{}", value)?;
            if i < self.elements.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_new() {
        let array = Array::new(vec![Variable::Int(1), Variable::Int(2)]);

        assert_eq!(array.elem_type, VarType::Int);
        assert_eq!(array.elements.len(), 2);
        assert_eq!(array.elements[0], Variable::Int(1));
        assert_eq!(array.elements[1], Variable::Int(2));
    }

    #[test]
    fn test_new_empty() {
        let array = Array::new(vec![]);

        assert_eq!(array.elem_type, VarType::Empty);
        assert_eq!(array.elements.len(), 0);
    }

    #[test]
    fn test_push() {
        let mut array = Array::new(vec![Variable::Int(1)]);
        array.push(Variable::Int(2)).unwrap();

        assert_eq!(array.elem_type, VarType::Int);
        assert_eq!(array.elements.len(), 2);
        assert_eq!(array.elements[0], Variable::Int(1));
        assert_eq!(array.elements[1], Variable::Int(2));
    }

    #[test]
    fn test_push_empty() {
        let mut array = Array::new(vec![]);
        array.push(Variable::Int(1)).unwrap();

        assert_eq!(array.elem_type, VarType::Int);
        assert_eq!(array.elements.len(), 1);
        assert_eq!(array.elements[0], Variable::Int(1));
    }

    #[test]
    fn test_push_mismatch() {
        let mut array = Array::new(vec![Variable::Int(1)]);
        assert!(array.push(Variable::Float(2.0)).is_err());
    }

    #[test]
    fn test_assign() {
        let mut array = Array::new(vec![Variable::Int(1)]);
        let other = Array::new(vec![Variable::Int(2)]);
        array
            .assign(Variable::Array(Rc::new(RefCell::new(other))))
            .unwrap();

        assert_eq!(array.elem_type, VarType::Int);
        assert_eq!(array.elements.len(), 1);
        assert_eq!(array.elements[0], Variable::Int(2));
    }

    #[test]
    fn test_assign_mismatch() {
        let mut array = Array::new(vec![Variable::Int(1)]);
        let other = Array::new(vec![Variable::Float(2.0)]);
        assert!(array
            .assign(Variable::Array(Rc::new(RefCell::new(other))).clone())
            .is_err());
    }

    #[test]
    fn test_assign_empty() {
        let mut array = Array::new(vec![]);
        let other = Array::new(vec![Variable::Int(2)]);
        array
            .assign(Variable::Array(Rc::new(RefCell::new(other))))
            .unwrap();

        assert_eq!(array.elem_type, VarType::Int);
        assert_eq!(array.elements.len(), 1);
        assert_eq!(array.elements[0], Variable::Int(2));

        let mut array = Array::new(vec![Variable::Int(1)]);
        let other = Array::new(vec![]);
        array
            .assign(Variable::Array(Rc::new(RefCell::new(other))))
            .unwrap();

        assert_eq!(array.elem_type, VarType::Int);
        assert_eq!(array.elements.len(), 0);
    }

    #[test]
    fn test_array_in_array() {
        let array = Array::new(vec![Variable::Int(1)]);
        let outer = Array::new(vec![Variable::Array(Rc::new(RefCell::new(array)))]);

        assert_eq!(outer.elem_type, VarType::Array(Box::new(VarType::Int)));
        assert_eq!(outer.elements.len(), 1);

        let Variable::Array(inner) = outer.elements[0].clone() else {
            panic!();
        };
        assert_eq!(inner.borrow().elem_type, VarType::Int);
        assert_eq!(inner.borrow().elements.len(), 1);
        assert_eq!(inner.borrow().elements[0], Variable::Int(1));
    }

    #[test]
    fn test_nested_array_empty() {
        // Array tested : [[], [3, 4]]
        let inner1 = Array::new(vec![]);
        let inner2 = Array::new(vec![Variable::Int(3), Variable::Int(4)]);
        let outer = Array::new(vec![
            Variable::Array(Rc::new(RefCell::new(inner1))),
            Variable::Array(Rc::new(RefCell::new(inner2))),
        ]);

        assert_eq!(outer.elem_type, VarType::Array(Box::new(VarType::Int)));
        assert_eq!(outer.elements.len(), 2);

        let Variable::Array(inner) = outer.elements[0].clone() else {
            panic!();
        };

        assert_eq!(inner.borrow().elem_type, VarType::Int);
        assert_eq!(inner.borrow().elements.len(), 0);

        let Variable::Array(inner) = outer.elements[1].clone() else {
            panic!();
        };

        assert_eq!(inner.borrow().elem_type, VarType::Int);
        assert_eq!(inner.borrow().elements.len(), 2);
        assert_eq!(inner.borrow().elements[0], Variable::Int(3));
        assert_eq!(inner.borrow().elements[1], Variable::Int(4));
    }

    #[test]
    fn test_nested_array_complex() {
        // Array tested : [[[1,2]],[]]
        let first_layer = Array::new(vec![Variable::Int(1), Variable::Int(2)]);
        let second_layer = Array::new(vec![Variable::Array(Rc::new(RefCell::new(first_layer)))]);
        let second_layer_empty = Array::new(vec![]);
        let third_layer = Array::new(vec![
            Variable::Array(Rc::new(RefCell::new(second_layer))),
            Variable::Array(Rc::new(RefCell::new(second_layer_empty))),
        ]);

        assert_eq!(
            third_layer.elem_type,
            VarType::Array(Box::new(VarType::Array(Box::new(VarType::Int))))
        );

        assert_eq!(third_layer.elements.len(), 2);

        let Variable::Array(second) = third_layer.elements[0].clone() else {
            panic!();
        };

        assert_eq!(second.borrow().elements.len(), 1);
        assert_eq!(
            second.borrow().elem_type,
            VarType::Array(Box::new(VarType::Int))
        );

        let Variable::Array(second_empty) = third_layer.elements[1].clone() else {
            panic!();
        };

        assert_eq!(second_empty.borrow().elements.len(), 0);
        assert_eq!(
            second_empty.borrow().elem_type,
            VarType::Array(Box::new(VarType::Int))
        );
    }
}
