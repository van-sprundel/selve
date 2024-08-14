use crate::{
    ast::{Expr, Literal, Stmt},
    token::{Token, TokenKind},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();

        while !self.eof() {
            statements.push(self.declaration());
        }

        statements
    }

    fn declaration(&mut self) -> Stmt {
        match self.peek().kind {
            TokenKind::Let => self.let_declaration(),
            TokenKind::Fn => self.function_declaration(),
            _ => self.statement(),
        }
    }

    fn let_declaration(&mut self) -> Stmt {
        self.consume(&TokenKind::Let, "Expect 'let' keyword.");

        let token = self.advance();
        if let TokenKind::Identifier(_) = token.clone().kind {
        } else {
            panic!("Expected name identifier");
        };

        let next_token = self.advance();
        let initializer = if TokenKind::Equals == next_token.kind {
            Some(self.expression())
        } else if TokenKind::Semicolon == next_token.kind {
            None
        } else {
            panic!("Unexpected character");
        };

        self.consume(
            &TokenKind::Semicolon,
            "Expect ';' after variable declaration.",
        );
        Stmt::Let(token, initializer)
    }

    fn function_declaration(&mut self) -> Stmt {
        self.consume(&TokenKind::Fn, "Expect 'fn' keyword.");

        let token = self.advance();
        if let TokenKind::Identifier(name) = token.clone().kind {
            name
        } else {
            panic!("Expected function name.");
        };

        self.consume(&TokenKind::LeftParen, "Expect '(' after function name.");
        let parameters = self.parameters();
        self.consume(&TokenKind::RightParen, "Expect ')' after parameters.");

        self.consume(&TokenKind::LeftBrace, "Expect '{' before function body.");
        let body = self.block();

        Stmt::FunctionDecl(token, parameters, Box::new(body))
    }

    fn parameters(&mut self) -> Vec<Token> {
        let mut parameters = Vec::new();

        if !self.check(&TokenKind::RightParen) {
            loop {
                self.consume_parameter(&mut parameters);
                if self.peek().kind != TokenKind::Comma {
                    break;
                }
            }
        }

        parameters
    }

    fn consume_parameter(&mut self, parameters: &mut Vec<Token>) {
        if parameters.len() >= 255 {
            self.error(self.peek(), "Can't have more than 255 parameters.");
        }

        let token = self.advance();
        if let TokenKind::Identifier(name) = token.clone().kind {
        } else {
            panic!("Expected identifier name");
        }

        parameters.push(token);
    }

    fn statement(&mut self) -> Stmt {
        match self.peek().kind {
            TokenKind::If => self.if_statement(),
            TokenKind::Return => self.return_statement(),
            TokenKind::LeftBrace => self.block(),
            _ => self.expression_statement(),
        }
    }

    fn if_statement(&mut self) -> Stmt {
        self.consume(&TokenKind::LeftParen, "Expect '(' after 'if'.");

        let condition = self.expression();

        self.consume(&TokenKind::RightParen, "Expect ')' after if condition.");

        let then_branch = Box::new(self.statement());
        let else_branch = if self.peek().kind == TokenKind::Else {
            Some(Box::new(self.statement()))
        } else {
            None
        };

        Stmt::If(condition, then_branch, else_branch)
    }

    fn return_statement(&mut self) -> Stmt {
        let keyword = self.previous().clone();
        let value = if !self.check(&TokenKind::Semicolon) {
            Some(self.expression())
        } else {
            None
        };

        self.consume(&TokenKind::Semicolon, "Expect ';' after return value.");
        Stmt::Return(keyword, value)
    }

    fn block(&mut self) -> Stmt {
        let mut statements = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.eof() {
            statements.push(self.declaration());
        }

        self.consume(&TokenKind::RightBrace, "Expect '}' after block.");
        Stmt::Block(statements)
    }

    fn expression_statement(&mut self) -> Stmt {
        let expr = self.expression();

        self.consume(&TokenKind::Semicolon, "Expect ';' after expression.");
        Stmt::Expr(expr)
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while let TokenKind::BangEquals | TokenKind::EqualsEquals = self.peek().kind {
            self.advance();

            let operator = self.advance().clone();
            let right = self.comparison();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while let TokenKind::Equals
        | TokenKind::BangEquals
        | TokenKind::PlusEquals
        | TokenKind::MinusEquals
        | TokenKind::AsteriskEquals
        | TokenKind::SlashEquals
        | TokenKind::PercentageEquals = self.peek().kind
        {
            self.advance();

            let operator = self.previous().clone();
            let right = self.term();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while let TokenKind::Plus | TokenKind::Minus = self.peek().kind {
            self.advance();

            let operator = self.previous().clone();
            let right = self.factor();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while let TokenKind::Slash | TokenKind::Asterisk | TokenKind::Percentage = self.peek().kind
        {
            self.advance();

            let operator = self.previous().clone();
            let right = self.unary();

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        match self.peek().kind {
            TokenKind::Bang | TokenKind::Minus => {
                self.advance();
                let operator = self.previous().clone();
                let right = self.unary();
                return Expr::Unary {
                    operator,
                    expr: Box::new(right),
                };
            }
            _ => {}
        }

        self.primary()
    }

    fn primary(&mut self) -> Expr {
        match self.advance().kind {
            TokenKind::Integer(value) => Expr::Literal(Literal::Integer(value)),
            TokenKind::Float(value) => Expr::Literal(Literal::Float(value)),
            TokenKind::String(value) => Expr::Literal(Literal::String(value.clone())),
            TokenKind::Identifier(name) => Expr::Literal(Literal::String(name.clone())),
            TokenKind::LeftParen => {
                let expr = self.expression();
                self.consume(&TokenKind::RightParen, "Expect ')' after expression.");
                Expr::Grouping(Box::new(expr))
            }
            _ => panic!("Expect expression."),
        }
    }

    fn consume(&mut self, kind: &TokenKind, message: &str) -> Token {
        if self.check(kind) {
            self.advance()
        } else {
            panic!("{}", message);
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.eof() {
            false
        } else {
            &self.peek().kind == kind
        }
    }

    fn advance(&mut self) -> Token {
        let current = self.peek().clone();
        if !self.eof() {
            self.current += 1;
        }

        current
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn error(&self, token: &Token, message: &str) {
        eprintln!("Error at line {}: {}", token.line, message);
    }
}

#[rustfmt::skip]
#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    // Let declarations
    #[test_case(vec![
        Token { kind: TokenKind::Let, line: 1, column: 4 }, 
        Token { kind: TokenKind::Identifier("x".to_string()), line: 1, column: 6 }, 
        Token { kind: TokenKind::Equals, line: 1, column: 8 }, 
        Token { kind: TokenKind::Integer(5), line: 1, column: 10 }, 
        Token { kind: TokenKind::Semicolon, line: 1, column: 11 }, 
        Token { kind: TokenKind::Eof, line: 1, column: 11 }
      ], vec![
        Stmt::Let(
            Token { kind: TokenKind::Identifier("x".to_string()), line: 1, column: 6 }, 
            Some(Expr::Literal(Literal::Integer(5)))
        )
    ]; "simple_let_tokens")]
    #[test_case(vec![
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
      ], vec![
            Stmt::Let(
                Token { kind: TokenKind::Identifier("x".to_string()), line: 1, column: 6 }, 
                Some(Expr::Binary { 
                    left: Box::new(Expr::Literal(Literal::Integer(5))) , 
                    right: Box::new(Expr::Binary {
                        left: Box::new(Expr::Binary {
                            left: Box::new(Expr::Literal(Literal::Integer(6))),
                            right: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                                left: Box::new(Expr::Literal(Literal::Integer(2))), 
                                right: Box::new(Expr::Literal(Literal::Integer(5))), 
                                operator: Token { kind: TokenKind::Percentage, line: 1, column: 21 },
                            }))), 
                            operator: Token { kind: TokenKind::Slash, line: 1, column: 16 }
                        }),
                        right: Box::new(Expr::Literal(Literal::Integer(4))),
                        operator: Token { kind: TokenKind::Asterisk, line: 1, column: 26 }
                    }),
                    operator: Token { kind: TokenKind::Plus, line: 1, column: 12}
                })
            )
    ]; "complex_let_tokens")]
    // Function declarations
    #[test_case(vec![
        Token { kind: TokenKind::Fn, line: 1, column: 3 }, 
        Token { kind: TokenKind::Identifier("foo".to_string()), line: 1, column: 7 }, 
        Token { kind: TokenKind::LeftParen, line: 1, column: 8 }, 
        Token { kind: TokenKind::RightParen, line: 1, column: 9 }, 
        Token { kind: TokenKind::LeftBrace, line: 1, column: 11 }, 
        Token { kind: TokenKind::RightBrace, line: 1, column: 12 }, 
        Token { kind: TokenKind::Eof, line: 1, column: 12 }
      ], vec![
            Stmt::FunctionDecl(
                Token {kind: TokenKind::Identifier("foo".to_string()), line: 1, column: 7}, 
                vec![], 
                Box::new(Stmt::Block(vec![]))
            )
    ]; "empty_fn_tokens")]
    #[test_case(vec![
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
      ], vec![
            Stmt::FunctionDecl(
                Token { kind: TokenKind::Identifier("foo".to_string()), line: 1, column: 7 }, 
                vec![], 
                Box::new(Stmt::Block(vec![
                        Stmt::Let(
                            Token { kind: TokenKind::Identifier("x".to_string()), line: 2, column: 14 }, 
                            Some(Expr::Literal(Literal::Integer(5)))
                        )
                ]))
            )
    ]; "simple_fn_tokens")]
    fn should_succeed( input: Vec<Token>, expected: Vec<Stmt>) {
        let mut parser = Parser::new(input);
        let ast = parser.parse();

        assert_eq!(ast, expected);
    }
}
