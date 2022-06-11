use std::{error::Error, fmt::Display, rc::Rc};

use serde::{Deserialize, Serialize};

use crate::{callable::Callable, expr::Literal};

pub const NIL_TYPE: &str = "nil";
pub const BOOLEAN_TYPE: &str = "bool";
pub const NUMBER_TYPE: &str = "number";
pub const STRING_TYPE: &str = "string";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Nil;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Rc<str>),
    Callable(Rc<dyn Callable>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum SerializableValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Rc<str>),
}

impl From<&Value> for SerializableValue {
    fn from(v: &Value) -> Self {
        match v {
            Value::Nil => SerializableValue::Nil,
            Value::Boolean(x) => SerializableValue::Boolean(*x),
            Value::Number(x) => SerializableValue::Number(*x),
            Value::String(x) => SerializableValue::String(x.clone()),
            Value::Callable(_) => unreachable!(),
        }
    }
}

impl From<SerializableValue> for Value {
    fn from(v: SerializableValue) -> Self {
        match v {
            SerializableValue::Nil => Value::Nil,
            SerializableValue::Boolean(x) => Value::Boolean(x),
            SerializableValue::Number(x) => Value::Number(x),
            SerializableValue::String(x) => Value::String(x),
        }
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let serializable: SerializableValue = self.into();
        serializable.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializableValue::deserialize(deserializer).map(|x| x.into())
    }
}

impl Value {
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn as_rc_str(&self) -> Option<&Rc<str>> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s.as_ref()),
            _ => None,
        }
    }

    pub fn as_nil(&self) -> Option<Nil> {
        match self {
            Value::Nil => Some(Nil),
            _ => None,
        }
    }

    pub fn as_rc_callable(&self) -> Option<&Rc<dyn Callable>> {
        match self {
            Value::Callable(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_callable(&self) -> Option<&dyn Callable> {
        match self {
            Value::Callable(c) => Some(c.as_ref()),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => {
                // We diverge with IEEE 754 here to comply with the lox language spec.
                (l0 == r0) || (l0.is_nan() && r0.is_nan())
            }
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for Value {}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => b.fmt(f),
            Value::Number(n) => n.fmt(f),
            Value::String(s) => write!(f, "{}", s.as_ref()),
            Value::Callable(c) => c.fmt(f),
        }
    }
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_new::new)]
pub struct ConversionError {
    converting_from: Value,
    kind: ConversionErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConversionErrorKind {
    ConversionToString,
    ConversionToNumber,
    ConversionToNil,
}

impl ConversionErrorKind {
    pub fn conversion_to(&self) -> &'static str {
        match self {
            Self::ConversionToString => STRING_TYPE,
            Self::ConversionToNumber => NUMBER_TYPE,
            Self::ConversionToNil => NIL_TYPE,
        }
    }
}

impl Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Could not coerce value {} to {}",
            self.converting_from,
            self.kind.conversion_to()
        )
    }
}

impl Error for ConversionError {}

impl TryFrom<&Value> for Nil {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value.as_nil() {
            Some(x) => Ok(x),
            None => Err(ConversionError::new(
                value.clone(),
                ConversionErrorKind::ConversionToNil,
            )),
        }
    }
}

impl TryFrom<Value> for Nil {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value.as_nil() {
            Some(x) => Ok(x),
            _ => Err(ConversionError::new(
                value,
                ConversionErrorKind::ConversionToNil,
            )),
        }
    }
}

impl From<&Value> for bool {
    fn from(v: &Value) -> Self {
        v.as_bool()
    }
}

impl TryFrom<&Value> for f64 {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value.as_number() {
            Some(n) => Ok(n),
            None => Err(ConversionError::new(
                value.clone(),
                ConversionErrorKind::ConversionToNumber,
            )),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value.as_number() {
            Some(n) => Ok(n),
            None => Err(ConversionError::new(
                value,
                ConversionErrorKind::ConversionToNumber,
            )),
        }
    }
}

impl TryFrom<&Value> for Rc<str> {
    type Error = ConversionError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value.as_rc_str() {
            Some(s) => Ok(s.clone()),
            None => Err(ConversionError::new(
                value.clone(),
                ConversionErrorKind::ConversionToString,
            )),
        }
    }
}

impl TryFrom<Value> for Rc<str> {
    type Error = ConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        // this converts directly to avoid an extraneous clone of the Rc
        match value {
            Value::String(s) => Ok(s),
            _ => Err(ConversionError::new(
                value,
                ConversionErrorKind::ConversionToString,
            )),
        }
    }
}
