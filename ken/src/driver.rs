use std::fmt::Write;
use std::io::Read;

use codespan_reporting::files::SimpleFile;
use kenc::CompileError;
use kenc::codegen::{Codegen, GlobalMap};
use kenc::lexer::Lexer;
use kenc::parser::Parser;
use kenvm::Vm;
use kenvm::obj::Function;

use crate::cli::Cli;
use crate::editor::{Editor, EditorRead};
use crate::report::{Report, SimpleReport};

pub struct Driver {
    file:       SimpleFile<String, String>,
    repl:       bool,
    max_errors: usize,
    quiet:      bool,
    vm:         Vm,
    global:     GlobalMap,
}

impl Default for Driver {
    fn default() -> Self {
        Self::new()
    }
}

impl Driver {
    #[must_use]
    pub fn new() -> Self {
        Self::from_config(<Cli as clap::Parser>::parse())
    }

    fn read_stdin() -> String {
        let mut input = String::new();
        std::io::stdin()
            .read_to_string(&mut input)
            .expect("Should read input from stdin");
        input
    }

    #[must_use]
    fn from_config(cfg: Cli) -> Self {
        let (file, repl) = if cfg.stdin {
            (
                SimpleFile::new("<stdin>".to_string(), Self::read_stdin()),
                false,
            )
        } else {
            cfg.file.map_or_else(
                || (SimpleFile::new("<stdin>".to_string(), String::new()), true),
                |path| {
                    let source = std::fs::read_to_string(&path).expect("Should be valid file path");
                    (SimpleFile::new(path, source), false)
                },
            )
        };
        Self {
            file,
            repl,
            quiet: cfg.quiet,
            max_errors: cfg.max_errors,
            vm: Vm::default(),
            global: GlobalMap::default(),
        }
    }

    pub fn run(mut self) {
        if self.repl {
            let _ = self.repl();
        } else {
            self.file();
        }
    }

    fn file(&mut self) {
        let function = match self.compile() {
            Ok(f) => f,
            Err(err) => {
                self.report_compile_error(err);
                return;
            }
        };

        match self.vm.eval(&function) {
            Ok(_) => {}
            Err(err) => {
                self.report(&[err]);
            }
        }
    }

    fn report<E>(&self, errors: &[E])
    where
        E: Report,
    {
        if self.quiet {
            return;
        }
        let mut displayed = 0;
        for e in errors.iter().take(self.max_errors) {
            e.report(&self.file);
            displayed += 1;
        }

        let mut message = format!("could not run {}", self.file.name());

        let _ = message.write_fmt(format_args!(
            " due to {} previous {} ({} emitted)",
            errors.len(),
            if errors.len() > 1 { "errors" } else { "error" },
            displayed
        ));

        let error = SimpleReport::new(message);
        error.report(&self.file);
    }

    fn report_compile_error(&self, error: CompileError) {
        match error {
            CompileError::Lex(spands) => self.report(&spands),
            CompileError::Parse(spands) => self.report(&spands),
            CompileError::Codegen(err) => self.report(&[err]),
        }
    }

    fn compile(&mut self) -> Result<Function, CompileError> {
        let lexer = Lexer::new(self.file.source());
        let tokens = lexer.lex_all()?;

        let mut parser = Parser::new(tokens);
        let ast = parser.parse_all()?;

        let mut code = Codegen::new(None, 0, &mut self.global);
        for stmt in ast {
            code.compile_stmt(stmt)?;
        }
        Ok(code.finish())
    }

    fn repl(&mut self) -> std::io::Result<()> {
        let mut editor = Editor::default();
        loop {
            let signal = editor.read()?;
            let input = match signal {
                EditorRead::Read(input) => input,
                EditorRead::Break => break,
                EditorRead::Continue => continue,
            };
            self.file = SimpleFile::new("<stdin>".to_string(), input);

            let function = match self.compile() {
                Ok(f) => f,
                Err(err) => {
                    self.report_compile_error(err);
                    continue;
                }
            };

            match self.vm.eval(&function) {
                Ok(ret) => println!("{ret}"),
                Err(err) => {
                    self.report(&[err]);
                }
            }
        }

        Ok(())
    }
}
