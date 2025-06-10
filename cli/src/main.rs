use std::error::Error;

use ken::ast::eval;
use ken::lexer::Lexer;
use ken::parser::Parser;
use ken::span::Spand;
use rustyline::DefaultEditor;

fn report<E: Error>(src: &str, err: Spand<E>) {
    let (lo, _) = err.span.decode(src).unwrap();
    eprintln!("error:{}:{}: {}", lo.line, lo.column, err.kind);
}

fn main() -> rustyline::Result<()> {
    let mut rl = DefaultEditor::new()?;

    loop {
        let input = rl.readline(">> ")?;

        let lexer = Lexer::new(&input);
        let tokens = match lexer.lex_all() {
            Ok(tokens) => tokens,
            Err(err) => {
                report(&input, err);
                continue;
            }
        };
        let mut parser = Parser::new(tokens);
        let expr = match parser.parse_expr() {
            Ok(expr) => expr,
            Err(err) => {
                report(&input, err);
                continue;
            }
        };

        let x = eval(&expr);
        println!("{x}");
    }
}
