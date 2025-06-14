use std::fmt::Write;
use std::io::Read;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::{Files, SimpleFile};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use kenc::CompileError;
use kenc::codegen::Codegen;
use kenc::lexer::Lexer;
use kenc::parser::Parser;
use kenvm::obj::Function;
use kenvm::{FrameError, Vm};

use crate::cli::Cli;
use crate::editor::{Editor, EditorRead};
use crate::report::Report;

pub struct Driver {
    file:       Option<SimpleFile<String, String>>,
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
        let file = if cfg.stdin {
            Some(SimpleFile::new("<stdin>".to_string(), Self::read_stdin()))
        } else {
            cfg.file.map(|path| {
                let source = std::fs::read_to_string(&path).expect("Should be valid file path");
                SimpleFile::new(path, source)
            })
        };
        Self {
            file,
            quiet: cfg.quiet,
            max_errors: cfg.max_errors,
        }
    }

    pub fn run(self) {
        if let Some(_file) = self.file {
            todo!()
        } else {
            let _ = self.repl();
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
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        let mut displayed = 0;
        {
            let mut writer = writer.lock();
            for e in errors.iter().take(self.max_errors) {
                let report = e.report();
                let _ = term::emit(&mut writer, &config, files, &report);
                displayed += 1;
            }
        }

        let mut message = self.file.as_ref().map_or_else(
            || "could not compile".to_string(),
            |file| format!("could not compile {}", file.name()),
        );

        let _ = message.write_fmt(format_args!(
            " due to {} previous {} ({} emitted)",
            errors.len(),
            if errors.len() > 1 { "errors" } else { "error" },
            displayed
        ));

        let error = Diagnostic::error().with_message(message);

        let _ = term::emit(&mut writer.lock(), &config, files, &error);
    }

    fn report_compile_error(&self, name: &str, source: String, error: CompileError) {
        let file = SimpleFile::new(name, source);
        match error {
            CompileError::Lex(spands) => self.report(&spands, &file),
            CompileError::Parse(spands) => self.report(&spands, &file),
            CompileError::Codegen(err) => self.report(&[err], &file),
        }
    }

    fn report_runtime_error(&self, name: &str, source: String, error: &FrameError) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let report = error.report();
        let file = SimpleFile::new(name, source);
        let mut writer = writer.lock();
        let _ = term::emit(&mut writer, &config, &file, &report);
    }

    fn compile(&self, input: &str) -> Result<Function, CompileError> {
        let lexer = Lexer::new(input);
        let tokens = lexer.lex_all()?;

        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().map_err(|e| vec![e])?;

        let mut code = Codegen::new(0);
        code.compile_expr(expr)?;
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
                    self.report_compile_error("<stdin>", input, err);
                    continue;
                }
            };

            match vm.eval(&function) {
                Ok(ret) => println!("{ret}"),
                Err(err) => {
                    self.report_runtime_error("<stdin>", input, &err);
                }
            }
        }

        Ok(())
    }
}
