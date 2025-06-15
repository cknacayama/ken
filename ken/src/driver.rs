use std::fmt::Write;
use std::io::Read;

use codespan_reporting::files::{Files, SimpleFile};
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
        }
    }

    pub fn run(self) {
        if self.repl {
            let _ = self.repl();
        } else {
            self.file();
        }
    }

    fn file(&self) {
        let function = match self.compile(self.file.source()) {
            Ok(f) => f,
            Err(err) => {
                self.report_compile_error(&self.file, err);
                return;
            }
        };
        let mut vm = Vm::new();

        match vm.eval(&function) {
            Ok(_) => {}
            Err(err) => {
                err.report(&self.file);
            }
        }
    }

    fn report<'a, F, E>(&self, errors: &[E], files: &'a F)
    where
        F: Files<'a, FileId = ()>,
        E: Report,
    {
        if self.quiet {
            return;
        }
        let mut displayed = 0;
        for e in errors.iter().take(self.max_errors) {
            e.report(files);
            displayed += 1;
        }

        let mut message = format!("could not compile {}", self.file.name());

        let _ = message.write_fmt(format_args!(
            " due to {} previous {} ({} emitted)",
            errors.len(),
            if errors.len() > 1 { "errors" } else { "error" },
            displayed
        ));

        let error = SimpleReport::new(message);
        error.report(files);
    }

    fn report_compile_error(&self, file: &SimpleFile<String, String>, error: CompileError) {
        match error {
            CompileError::Lex(spands) => self.report(&spands, file),
            CompileError::Parse(spands) => self.report(&spands, file),
            CompileError::Codegen(err) => self.report(&[err], file),
        }
    }

    fn compile(&self, input: &str) -> Result<Function, CompileError> {
        let lexer = Lexer::new(input);
        let tokens = lexer.lex_all()?;

        let mut parser = Parser::new(tokens);
        let ast = parser.parse_all()?;

        let mut global = GlobalMap::default();
        let mut code = Codegen::new("", 0, &mut global);
        for stmt in ast {
            code.compile_stmt(stmt)?;
        }
        Ok(code.finish())
    }

    fn repl(&self) -> std::io::Result<()> {
        let mut vm = Vm::new();
        let mut editor = Editor::default();
        loop {
            let signal = editor.read()?;
            let input = match signal {
                EditorRead::Read(input) => input,
                EditorRead::Break => break,
                EditorRead::Continue => continue,
            };

            let function = match self.compile(&input) {
                Ok(f) => f,
                Err(err) => {
                    let file = SimpleFile::new("<stdin>".to_string(), input);
                    self.report_compile_error(&file, err);
                    continue;
                }
            };

            match vm.eval(&function) {
                Ok(ret) => println!("{ret}"),
                Err(err) => {
                    let file = SimpleFile::new("<stdin>".to_string(), input);
                    err.report(&file);
                }
            }
        }

        Ok(())
    }
}
