use std::{rc::Rc, fmt::Display};

use serde::{Deserialize, Serialize};

use crate::{
    expr::{BinaryExpr, Expr, Literal, UnaryExpr},
    token::{Keyword, Token},
    value::{ConversionError, Value},
};

#[derive(Debug, Clone, Serialize, Deserialize, derive_new::new)]
pub struct Interpreter {
    #[new(default)]
    environment: Environment,
}

impl Interpreter {
    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        expr.eval(&mut self.environment)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Environment {}

#[derive(Debug, Clone, Serialize, Deserialize, derive_new::new)]
pub struct RuntimeError {
    kind: RuntimeErrorKind,
}

#[derive(Debug, Clone, Serialize, Deserialize, thiserror::Error)]
pub enum RuntimeErrorKind {
    #[error("Operator {0:?} was used with an unexpected value: {1}")]
    UnexpectedValueForOperator(Token, #[source] ConversionError),

    #[error(
        "Operator {0:?} can only be used with numbers, \
        but was used with values {1} and {2}"
    )]
    ExpectedNumbersForOperator(Token, Value, Value),

    #[error(
        "Operator {0:?} can only be when both operands are either numbers or strings, \
        but was used with values {1} and {2}"
    )]
    ExpectedMatchingNumbersOrStringsForOperator(Token, Value, Value),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub trait Evaluate {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError>;
}

pub trait StaticEvaluate: Evaluate {
    fn static_eval(&self) -> Value;
}

impl<T> Evaluate for T
where
    T: StaticEvaluate,
{
    fn eval(&self, _env: &mut Environment) -> Result<Value, RuntimeError> {
        Ok(self.static_eval())
    }
}

impl StaticEvaluate for Literal {
    fn static_eval(&self) -> Value {
        self.into()
    }
}

impl Evaluate for Expr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            Expr::Unary(inner) => inner.eval(env),
            Expr::Grouping(inner) => inner.eval(env),
            Expr::Literal(inner) => inner.eval(env),
            Expr::Binary(inner) => inner.eval(env),
        }
    }
}

impl Evaluate for UnaryExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let inner = self.right.eval(env)?;

        match self.operator {
            Token::Minus => {
                let maybe_number: Result<f64, _> = inner.try_into();
                match maybe_number {
                    Ok(n) => Ok(Value::Number(-n)),
                    Err(e) => Err(RuntimeError::new(
                        RuntimeErrorKind::UnexpectedValueForOperator(self.operator.clone(), e),
                    )),
                }
            }
            Token::Bang => Ok((!inner.as_bool()).into()),
            _ => unreachable!("operator {:?} used in UnaryExpr: {:?}", self.operator, self),
        }
    }
}

impl Evaluate for BinaryExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        // Execution is left-to-right.
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;

        match self.operator {
            Token::Plus => {
                // We might be adding numbers or strings, handle both cases.
                if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
                    Ok(Value::Number(l + r))
                } else if let (Value::String(l), Value::String(r)) = (&left, &right) {
                    let output = l.to_string() + r.as_ref();
                    Ok(Value::String(Rc::from(output.as_str())))
                } else {
                    Err(RuntimeError::new(
                        RuntimeErrorKind::ExpectedMatchingNumbersOrStringsForOperator(
                            self.operator.clone(),
                            left,
                            right,
                        ),
                    ))
                }
            }
            Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Greater
            | Token::GreaterEqual
            | Token::Less
            | Token::LessEqual => {
                // We are doing math or algebraic comparisons on numbers.
                if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
                    match self.operator {
                        Token::Minus => Ok(Value::Number(l - r)),
                        Token::Star => Ok(Value::Number(l * r)),
                        Token::Slash => Ok(Value::Number(l / r)),
                        Token::Greater => Ok(Value::Boolean(l > r)),
                        Token::Less => Ok(Value::Boolean(l < r)),
                        Token::GreaterEqual => {
                            // Account for our special IEEE 754 non-compliant number equality.
                            Ok(Value::Boolean((l >= r) || (l.is_nan() && r.is_nan())))
                        }
                        Token::LessEqual => {
                            // Account for our special IEEE 754 non-compliant number equality.
                            Ok(Value::Boolean((l <= r) || (l.is_nan() && r.is_nan())))
                        }
                        _ => unreachable!(),
                    }
                } else {
                    Err(RuntimeError::new(
                        RuntimeErrorKind::ExpectedNumbersForOperator(
                            self.operator.clone(),
                            left,
                            right,
                        ),
                    ))
                }
            }
            Token::Keyword(Keyword::And | Keyword::Or) => {
                // We are doing logical operations on booleans.
                // TODO: Revisit this if it turns out that `and` and `or` should short-circuit.
                //       Right now, they do not.
                let left_bool = left.as_bool();
                let right_bool = right.as_bool();

                match self.operator {
                    Token::Keyword(Keyword::And) => Ok(Value::Boolean(left_bool && right_bool)),
                    Token::Keyword(Keyword::Or) => Ok(Value::Boolean(left_bool || right_bool)),
                    _ => unreachable!(),
                }
            }
            Token::EqualEqual | Token::BangEqual => {
                // We are checking values for equality, all value types are allowed here.
                match self.operator {
                    Token::EqualEqual => Ok(Value::Boolean(left == right)),
                    Token::BangEqual => Ok(Value::Boolean(left != right)),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(
                "operator {:?} used in BinaryExpr: {:?}",
                self.operator, self
            ),
        }
    }
}
