use std::{cell::RefCell, collections::BTreeMap, fmt::Display, ops::Deref, rc::Rc};

use serde::{Deserialize, Serialize};

use crate::{
    callable::Clock,
    expr::{BinaryExpr, Expr, Literal, UnaryExpr},
    stmt::Stmt,
    token::{Keyword, Token},
    value::{ConversionError, Value},
};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Interpreter {
    globals: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Environment::default();
        globals
            .borrow_mut()
            .define(Rc::from("clock"), Value::Callable(Rc::new(Clock)));

        Self { globals }
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        expr.eval(&mut self.globals)
    }

    pub fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        stmt.eval(&mut self.globals).map(|_| ())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default, derive_new::new)]
pub struct Environment(Rc<RefCell<RawEnvironment>>);

impl Deref for Environment {
    type Target = RefCell<RawEnvironment>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<RawEnvironment> for Environment {
    fn from(raw_env: RawEnvironment) -> Self {
        Self(Rc::new(RefCell::new(raw_env)))
    }
}

impl Environment {
    pub(crate) fn new_nested(&self) -> Self {
        let raw_env = RawEnvironment::new(Some(self.clone()));
        Self::from(raw_env)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default, derive_new::new)]
pub struct RawEnvironment {
    enclosing: Option<Environment>,

    #[new(default)]
    bindings: BTreeMap<Rc<str>, Value>,
}

impl RawEnvironment {
    fn get(&self, name: &str) -> Option<Value> {
        let maybe_value = self.bindings.get(name);
        if maybe_value.is_some() {
            maybe_value.map(|x| x.to_owned())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            None
        }
    }

    pub(crate) fn resolve(&self, name: &str) -> Result<Value, RuntimeError> {
        self.get(name)
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::UndefinedVariable(name.to_owned())))
    }

    pub(crate) fn define(&mut self, name: Rc<str>, value: Value) {
        self.bindings.insert(name, value);
    }

    pub(crate) fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        let maybe_value = self.bindings.get_mut(name);
        if let Some(v) = maybe_value {
            *v = value;
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(RuntimeError::new(RuntimeErrorKind::UndefinedVariable(
                name.to_owned(),
            )))
        }
    }
}

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

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Can only call functions and classes.")]
    CalledNonCallable,

    #[error("Expected {0} arguments for function call, but got {1} arguments instead.")]
    IncorrectNumberOfArguments(usize, usize),
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
            Expr::Variable(ident) => env.borrow().resolve(ident.as_ref()),
            Expr::Assignment(ident, expr) => {
                let value = expr.eval(env)?;
                env.borrow_mut().assign(ident.as_ref(), value.clone())?;
                Ok(value)
            }
            Expr::Call(inner) => {
                let callee = inner.callee.eval(env)?;
                let maybe_arguments: Result<Vec<Value>, RuntimeError> =
                    inner.arguments.iter().map(|arg| arg.eval(env)).collect();
                let arguments = maybe_arguments?;

                if let Some(callable) = callee.as_callable() {
                    if callable.arity() == arguments.len() {
                        callable.call(env, arguments)
                    } else {
                        Err(RuntimeError::new(
                            RuntimeErrorKind::IncorrectNumberOfArguments(
                                callable.arity(),
                                arguments.len(),
                            ),
                        ))
                    }
                } else {
                    Err(RuntimeError::new(RuntimeErrorKind::CalledNonCallable))
                }
            }
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

        // Check whether we're in a short-circuiting binary expression.
        // If so, return early.
        // If not, evaluate the right side and keep going.
        match self.operator {
            Token::Keyword(Keyword::And) => {
                if !left.as_bool() {
                    return Ok(left);
                }
            }
            Token::Keyword(Keyword::Or) => {
                if left.as_bool() {
                    return Ok(left);
                }
            }
            _ => {}
        }

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
                // We already determined that we didn't need to short-circuit earlier,
                // so if we're here, then the result is always the right side's value.
                Ok(right)
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

impl Evaluate for Stmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            Stmt::Print(expr) => {
                let value = expr.eval(env)?;
                println!("{value}");
                Ok(Value::Nil)
            }
            Stmt::Expression(expr) => expr.eval(env).map(|_| Value::Nil),
            Stmt::VarDeclaration(decl) => {
                let initial_value = if let Some(initializer) = &decl.initializer {
                    initializer.eval(env)?
                } else {
                    Value::Nil
                };
                env.borrow_mut().define(decl.name.clone(), initial_value);
                Ok(Value::Nil)
            }
            Stmt::Block(inner_stmts) => {
                let mut inner_env = env.new_nested();
                for stmt in inner_stmts {
                    stmt.eval(&mut inner_env)?;
                }
                Ok(Value::Nil)
            }
            Stmt::If(if_stmt) => {
                let condition = if_stmt.condition.eval(env)?;
                if condition.as_bool() {
                    if_stmt.then_branch.eval(env)?;
                    Ok(Value::Nil)
                } else {
                    if_stmt
                        .else_branch
                        .as_ref()
                        .map_or(Ok(Value::Nil), |x| x.eval(env).map(|_| Value::Nil))
                }
            }
            Stmt::While(while_stmt) => loop {
                let condition = while_stmt.condition.eval(env)?;
                if !condition.as_bool() {
                    break Ok(Value::Nil);
                }

                while_stmt.body.eval(env)?;
            },
        }
    }
}
