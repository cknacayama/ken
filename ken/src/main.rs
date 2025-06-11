use std::error::Error;

use kenc::codegen::Codegen;
use kenc::lexer::Lexer;
use kenc::parser::Parser;
use kenc::span::Spand;
use kenvm::Vm;
use rustyline::DefaultEditor;

fn report<E: Error>(src: &str, err: Spand<E>) {
    let (lo, _) = err.span.decode(src).unwrap();
    eprintln!("error:{}:{}: {}", lo.line, lo.column, err.kind);
}

fn repl(rl: &mut DefaultEditor) -> rustyline::Result<()> {
    let mut vm = Vm::new();
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
        let mut code = Codegen::new(0);
        code.compile_expr(expr);
        let function = code.finish();

        match vm.run(&function) {
            Ok((ret, _)) => println!("{ret}"),
            Err(err) => eprintln!("{err:?}"),
        }
    }
}

fn main() -> rustyline::Result<()> {
    let mut rl = DefaultEditor::new()?;
    repl(&mut rl)
}
