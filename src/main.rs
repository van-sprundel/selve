#![allow(unused)]

use std::io::BufRead;

use lexer::Lexer;
use parser::Parser;
use token::TokenKind;
mod ast;
mod lexer;
mod parser;
mod token;

fn main() {
    loop {
        repl();
    }
}

fn repl() {
    let mut input = String::new();
    std::io::stdin().lock().read_line(&mut input).unwrap();

    let mut lexer = Lexer::new(&input);
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next_token();
        tokens.push(token.clone());

        if token.kind == TokenKind::Eof {
            break;
        }
    }

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    println!("{:?}", ast);
}
