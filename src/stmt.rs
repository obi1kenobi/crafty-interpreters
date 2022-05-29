use std::rc::Rc;

use serde::{Serialize, Deserialize};

use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    VarDeclaration(VarDeclaration),
    Block(Vec<Stmt>),
    If(IfStatement),
    While(WhileStatement),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_new::new)]
pub struct VarDeclaration {
    pub name: Rc<str>,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_new::new)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Box<Option<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_new::new)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Self {
        Self::Expression(expr)
    }
}
