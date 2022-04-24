use std::{rc::Rc};

use serde::{Deserialize, Serialize};

use crate::token::Token;

pub trait Visitable {
    fn accept<V: Visitor>(visitor: V) -> Option<V::Output>;
}

pub trait Visitor {
    type Output;

    fn visit_expr(_expr: Expr) -> Option<Self::Output> {
        None
    }

    fn visit_literal(_literal: Literal) -> Option<Self::Output> {
        None
    }

    fn visit_unary(_unary: UnaryExpr) -> Option<Self::Output> {
        None
    }

    fn visit_binary(_binary: BinaryExpr) -> Option<Self::Output> {
        None
    }

    fn visit_grouping(_grouping: Expr) -> Option<Self::Output> {
        None
    }
}

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
