use std::{
    fmt::{Debug, Display},
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    interpreter::{Environment, RuntimeError},
    value::Value,
};

pub trait Callable: Display + Debug {
    fn call(&self, env: &mut Environment, arguments: Vec<Value>) -> Result<Value, RuntimeError>;

    fn arity(&self) -> usize;
}

#[derive(Debug)]
pub struct Clock;

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

impl Callable for Clock {
    fn call(&self, _env: &mut Environment, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        assert!(arguments.is_empty());

        Ok(Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("system time before epoch")
                .as_secs_f64(),
        ))
    }

    fn arity(&self) -> usize {
        0
    }
}
