use std::error::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use kenspan::Spand;
use kenvm::FrameError;

pub trait Report {
    fn report(&self) -> Diagnostic<()>;
}

impl<T: Error> Report for Spand<T> {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message(self.kind())
            .with_label(Label::primary((), self.span))
    }
}

impl Report for FrameError {
    fn report(&self) -> Diagnostic<()> {
        let mut spans = self.spans().iter().copied();
        Diagnostic::error()
            .with_message(self.runtime())
            .with_label(Label::primary((), spans.next().unwrap()))
            .with_labels_iter(spans.map(|span| Label::secondary((), span)))
    }
}
