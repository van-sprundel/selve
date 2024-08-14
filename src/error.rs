use std::error;

use thiserror::Error;

use crate::token::{Token, TokenKind};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Error)]
pub enum Error {
    #[error("Parser error: {0:?}")]
    Parser(ParserError),
    #[error("Typed error: {0:?}")]
    Lexer(LexerError),
    #[error("Typed error: {0:?}")]
    Typed(TypedError),
}

#[derive(Debug, PartialEq, Error)]
pub enum ParserError {
    #[error("Expected token {expected:?}")]
    UnexpectedToken { expected: TokenKind },
    #[error("Expected identifier")]
    ExpectedIdentifier,
    #[error("Expected function name")]
    ExpectedFunctionName,
    #[error("Unexpected character")]
    UnexpectedCharacter,
    #[error("Unexpected end of input")]
    UnexpectedEOF,
    #[error("Invalid expression")]
    InvalidExpression,
    #[error("Invalid assignment target")]
    InvalidAssignmentTarget,
    #[error("Mismatched parentheses")]
    MismatchedParentheses,
    #[error("Invalid statement")]
    InvalidStatement,
}

#[derive(Debug, PartialEq, Error)]
pub enum LexerError {}

#[derive(Debug, PartialEq, Error)]
pub enum TypedError {
    #[error("Undefined variable {0}")]
    UndefinedVariable(String),
    #[error("Symbol not registered")]
    UndefinedSymbol,
    #[error("Mismatched type")]
    TypeMismatch,
    #[error("Invalid token")]
    InvalidToken,
    #[error("Invalid operator")]
    InvalidOperator,
    #[error("Argument mismatch")]
    ArgumentMismatch,
    #[error("Not callable")]
    NotCallable,
}

impl From<TypedError> for Error {
    fn from(value: TypedError) -> Self {
        Self::Typed(value)
    }
}

impl From<LexerError> for Error {
    fn from(value: LexerError) -> Self {
        Self::Lexer(value)
    }
}

impl From<ParserError> for Error {
    fn from(value: ParserError) -> Self {
        Self::Parser(value)
    }
}
