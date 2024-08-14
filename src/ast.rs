use crate::{token::Token, type_checker::Type};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Let(Token, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    FunctionDecl(Token, Vec<Token>, Box<Stmt>),
    Return(Token, Option<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: Token,
    },
    Unary {
        operator: Token,
        expr: Box<Expr>,
    },
    Literal(Literal),
    Grouping(Box<Expr>),
    Variable(Token),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpr {
    pub expr: Expr,
    pub ty: Option<Type>,
}

impl TypedExpr {
    pub fn new(expr: Expr, ty: Option<Type>) -> Self {
        Self { expr, ty }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}
