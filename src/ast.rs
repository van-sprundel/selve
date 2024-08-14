use crate::token::Token;

#[derive(Debug, PartialEq)]
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
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Let(Token, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    FunctionDecl(Token, Vec<Token>, Box<Stmt>),
    Return(Token, Option<Expr>),
}
