use std::rc::Rc;

use serde::{Serialize, Deserialize};

use crate::token::Token;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(Box<UnaryExpr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Rc<String>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnaryExpr {
    operator: Token,
    right: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinaryExpr {
    left: Expr,
    operator: Token,
    right: Expr,
}
