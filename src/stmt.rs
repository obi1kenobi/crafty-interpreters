use std::rc::Rc;

use serde::{Serialize, Deserialize};

use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    VarDeclaration(VarDeclaration),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_new::new)]
pub struct VarDeclaration {
    pub name: Rc<str>,
    pub initializer: Option<Expr>,
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Self {
        Self::Expression(expr)
    }
}
