use std::rc::Rc;

use derive_new::new;
use serde::{Deserialize, Serialize};

use crate::expr::Literal;

const NIL_TYPE: &str = "nil";
const BOOLEAN_TYPE: &str = "bool";
const NUMBER_TYPE: &str = "number";
const STRING_TYPE: &str = "string";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Nil;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Rc<str>),
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Nil => Self::Nil,
            Literal::Boolean(b) => Self::Boolean(b),
            Literal::Number(n) => Self::Number(n),
            Literal::String(s) => Self::String(s),
        }
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Nil => Self::Nil,
            Literal::Boolean(b) => Self::Boolean(*b),
            Literal::Number(n) => Self::Number(*n),
            Literal::String(s) => Self::String(s.clone()),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Self::Number(n)
    }
}

impl From<Rc<str>> for Value {
    fn from(s: Rc<str>) -> Self {
        Self::String(s)
    }
}

impl From<&Rc<str>> for Value {
    fn from(s: &Rc<str>) -> Self {
        Self::String(s.clone())
    }
}

impl From<Nil> for Value {
    fn from(_: Nil) -> Self {
        Self::Nil
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, new)]
pub struct ConversionError {
    converting_value: Value,
    to_type: &'static str,
}

impl TryFrom<&Value> for Nil {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Nil => Ok(Nil),
            _ => Err(ConversionError::new(value.clone(), NIL_TYPE)),
        }
    }
}

impl TryFrom<&Value> for bool {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(*b),
            _ => Err(ConversionError::new(value.clone(), BOOLEAN_TYPE)),
        }
    }
}

impl TryFrom<&Value> for f64 {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(*n),
            _ => Err(ConversionError::new(value.clone(), NUMBER_TYPE)),
        }
    }
}

impl TryFrom<&Value> for Rc<str> {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(s.clone()),
            _ => Err(ConversionError::new(value.clone(), STRING_TYPE)),
        }
    }
}
