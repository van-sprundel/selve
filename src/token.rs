#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // Keywords
    Let,
    Fn,
    If,
    Else,
    Return,

    // Literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    False,
    True,
    Nil,

    // Operands
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percentage,
    Bang,
    Greater,
    Less,
    Equals,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    SlashEquals,
    PercentageEquals,
    BangEquals,
    EqualsEquals,
    GreaterEquals,
    LessEquals,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Semicolon,

    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}
