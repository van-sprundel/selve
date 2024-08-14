use crate::token::{Token, TokenKind};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            line: 1,
            column: 1,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(&c) = self.input.peek() {
            match c {
                '(' => self.single_char_token(TokenKind::LeftParen),
                ')' => self.single_char_token(TokenKind::RightParen),
                '{' => self.single_char_token(TokenKind::LeftBrace),
                '}' => self.single_char_token(TokenKind::RightBrace),
                '.' => self.single_char_token(TokenKind::Dot),
                ',' => self.single_char_token(TokenKind::Comma),
                ';' => self.single_char_token(TokenKind::Semicolon),
                '+' => self.single_char_token(TokenKind::Plus),
                '-' => self.single_char_token(TokenKind::Minus),
                '*' => self.single_char_token(TokenKind::Asterisk),
                '/' => {
                    self.advance();
                    if self.match_next('/') {
                        self.create_token(TokenKind::SlashSlash)
                    } else {
                        self.single_char_token(TokenKind::Slash)
                    }
                }
                '%' => self.single_char_token(TokenKind::Percentage),
                '=' => self.equals_token(),
                c if c.is_alphabetic() => self.identifier(),
                c if c.is_ascii_digit() => self.number(),
                _ => todo!("Implement error handling for unexpected characters"),
            }
        } else {
            self.create_token(TokenKind::Eof)
        }
    }

    fn single_char_token(&mut self, kind: TokenKind) -> Token {
        self.advance();
        self.create_token(kind)
    }

    fn equals_token(&mut self) -> Token {
        self.advance();
        if self.match_next('=') {
            self.create_token(TokenKind::EqualsEquals)
        } else {
            self.create_token(TokenKind::Equals)
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.input.next();
        if let Some(c) = c {
            self.column += 1;
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            }
        }
        c
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.input.peek() == Some(&expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.input.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn create_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            line: self.line,
            column: self.column,
        }
    }

    fn identifier(&mut self) -> Token {
        let mut value = String::new();

        while let Some(c) = self.input.peek().cloned() {
            if !c.is_alphanumeric() || c.is_whitespace() {
                break;
            }

            self.advance();
            value.push(c);
        }

        if value.is_empty() {
            panic!("Identifier is empty");
        }

        // reserved identifiers
        match &*value {
            "let" => self.create_token(TokenKind::Let),
            "fn" => self.create_token(TokenKind::Fn),
            _ => self.create_token(TokenKind::Identifier(value)),
        }
    }

    fn number(&mut self) -> Token {
        let mut value = String::new();

        while let Some(c) = self.input.peek().cloned() {
            if !c.is_numeric() || c.is_whitespace() {
                break;
            }

            self.advance();
            value.push(c);
        }

        let value = match value.parse::<i64>() {
            Ok(v) => v,
            Err(e) => panic!("Couldnt parse number: {e}"),
        };

        self.create_token(TokenKind::Integer(value))
    }
}

#[rustfmt::skip]
#[cfg(test)]
mod test {
    use super::*;
    use rstest::rstest;

    #[rstest]
    // Let declarations
    #[case("let x = 5;", vec![
        Token { kind: TokenKind::Let, line: 1, column: 4 }, 
        Token { kind: TokenKind::Identifier("x".to_string()), line: 1, column: 6 }, 
        Token { kind: TokenKind::Equals, line: 1, column: 8 }, 
        Token { kind: TokenKind::Integer(5), line: 1, column: 10 }, 
        Token { kind: TokenKind::Semicolon, line: 1, column: 11 }, 
        Token { kind: TokenKind::Eof, line: 1, column: 11 }
    ])]
    #[case("let x = 5 + 6 / (2 % 5) * 4;", vec![
        Token { kind: TokenKind::Let, line: 1, column: 4 }, 
        Token { kind: TokenKind::Identifier("x".to_string()), line: 1, column: 6 }, 
        Token { kind: TokenKind::Equals, line: 1, column: 8 }, 
        Token { kind: TokenKind::Integer(5), line: 1, column: 10 }, 
        Token { kind: TokenKind::Plus, line: 1, column: 12 }, 
        Token { kind: TokenKind::Integer(6), line: 1, column: 14 }, 
        Token { kind: TokenKind::Slash, line: 1, column: 16 }, 
        Token { kind: TokenKind::LeftParen, line: 1, column: 18 }, 
        Token { kind: TokenKind::Integer(2), line: 1, column: 19 }, 
        Token { kind: TokenKind::Percentage, line: 1, column: 21 }, 
        Token { kind: TokenKind::Integer(5), line: 1, column: 23 }, 
        Token { kind: TokenKind::RightParen, line: 1, column: 24 }, 
        Token { kind: TokenKind::Asterisk, line: 1, column: 26 }, 
        Token { kind: TokenKind::Integer(4), line: 1, column: 28 }, 
        Token { kind: TokenKind::Semicolon, line: 1, column: 29 }, 
        Token { kind: TokenKind::Eof, line: 1, column: 29 }
    ])]
    // Function declarations
    #[case(r#"fn foo() {}"#, vec![
        Token { kind: TokenKind::Fn, line: 1, column: 3 }, 
        Token { kind: TokenKind::Identifier("foo".to_string()), line: 1, column: 7 }, 
        Token { kind: TokenKind::LeftParen, line: 1, column: 8 }, 
        Token { kind: TokenKind::RightParen, line: 1, column: 9 }, 
        Token { kind: TokenKind::LeftBrace, line: 1, column: 11 }, 
        Token { kind: TokenKind::RightBrace, line: 1, column: 12 }, 
        Token { kind: TokenKind::Eof, line: 1, column: 12 }
    ])]
    #[case(r#"fn foo() {
        let x = 5;
    }"#, vec![
        Token { kind: TokenKind::Fn, line: 1, column: 3 }, 
        Token { kind: TokenKind::Identifier("foo".to_string()), line: 1, column: 7 }, 
        Token { kind: TokenKind::LeftParen, line: 1, column: 8 }, 
        Token { kind: TokenKind::RightParen, line: 1, column: 9 }, 
        Token { kind: TokenKind::LeftBrace, line: 1, column: 11 }, 
        Token { kind: TokenKind::Let, line: 2, column: 12 }, 
        Token { kind: TokenKind::Identifier("x".to_string()), line: 2, column: 14 }, 
        Token { kind: TokenKind::Equals, line: 2, column: 16 }, 
        Token { kind: TokenKind::Integer(5), line: 2, column: 18 }, 
        Token { kind: TokenKind::Semicolon, line: 2, column: 19 }, 
        Token { kind: TokenKind::RightBrace, line: 3, column: 6 }, 
        Token { kind: TokenKind::Eof, line: 3, column: 6 }
    ])]
    fn should_succeed(#[case] input: &str, #[case] expected: Vec<Token>) {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            tokens.push(token.clone());

            if token.kind == TokenKind::Eof {
                break;
            }
        }

        assert_eq!(tokens, expected);
    }
}
