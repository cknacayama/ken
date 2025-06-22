use std::error::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use kenspan::Spand;
use kenvm::FrameError;

pub trait Report {
    fn diagnose(&self) -> Diagnostic<()>;

    fn report<'a, F>(&self, file: &'a F)
    where
        F: Files<'a, FileId = ()>,
    {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let report = self.diagnose();
        let mut writer = writer.lock();
        let _ = term::emit(&mut writer, &config, file, &report);
    }
}

pub struct SimpleReport {
    message: String,
}

impl SimpleReport {
    #[must_use]
    pub const fn new(message: String) -> Self {
        Self { message }
    }
}

impl Report for SimpleReport {
    fn diagnose(&self) -> Diagnostic<()> {
        Diagnostic::error().with_message(&self.message)
    }
}

impl<T: Error> Report for Spand<T> {
    fn diagnose(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message(self.kind())
            .with_label(Label::primary((), self.span))
    }
}

impl Report for FrameError {
    fn diagnose(&self) -> Diagnostic<()> {
        let mut spans = self.spans().iter().copied();
        Diagnostic::error()
            .with_message(self.runtime())
            .with_label(Label::primary((), spans.next().unwrap()))
            .with_labels_iter(
                spans.map(|span| Label::secondary((), span).with_message("from here")),
            )
    }
}

impl<E: Report> Report for Box<E> {
    fn diagnose(&self) -> Diagnostic<()> {
        self.as_ref().diagnose()
    }
}
