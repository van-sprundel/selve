#![allow(unused)]

use codegen::compile;
use lexer::Lexer;
use parser::Parser;
use std::io::BufRead;
use token::TokenKind;
use type_checker::TypeChecker;

mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod symbol_table;
mod token;
mod type_checker;

pub use error::Result;

fn main() {
    loop {
        repl();
    }
}

fn repl() -> Result<()> {
    let mut input = String::new();
    std::io::stdin().lock().read_line(&mut input).unwrap();

    // Lexer
    let mut lexer = Lexer::new(&input);
    let tokens = lexer.parse();

    // Parser
    let mut parser = Parser::new(tokens);
    let mut ast = parser.parse()?;

    // Semantic analysis
    let mut type_checker = TypeChecker::new();
    type_checker.check(&mut ast).expect("Type check failed");

    println!("{:?}", ast);

    // LLVM IR
    compile(&ast);

    Ok(())
}
