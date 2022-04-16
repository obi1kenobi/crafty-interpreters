use std::rc::Rc;

use serde::{Deserialize, Serialize};

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
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}
