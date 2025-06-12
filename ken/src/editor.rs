use std::borrow::Cow;

use reedline::{Prompt, Reedline, Signal};

pub struct Editor {
    editor: Reedline,
    prompt: KenPrompt,
}

impl Default for Editor {
    fn default() -> Self {
        use reedline::Vi;

        let edit_mode = Vi::default();

        let editor = Reedline::create().with_edit_mode(Box::new(edit_mode));
        let prompt = KenPrompt;

        Self { editor, prompt }
    }
}

pub enum EditorRead {
    Read(String),
    Break,
    Continue,
}

impl Editor {
    pub fn read(&mut self) -> std::io::Result<EditorRead> {
        match self.editor.read_line(&self.prompt)? {
            Signal::Success(input) if input.is_empty() => Ok(EditorRead::Continue),
            Signal::Success(input) => Ok(EditorRead::Read(input)),
            Signal::CtrlC | Signal::CtrlD => Ok(EditorRead::Break),
        }
    }
}

pub struct KenPrompt;

impl Prompt for KenPrompt {
    fn render_prompt_left(&self) -> Cow<'_, str> {
        Cow::Borrowed(">> ")
    }

    fn render_prompt_right(&self) -> Cow<'_, str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _prompt_mode: reedline::PromptEditMode) -> Cow<'_, str> {
        Cow::Borrowed("")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        Cow::Borrowed(".. ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: reedline::PromptHistorySearch,
    ) -> Cow<'_, str> {
        let prefix = match history_search.status {
            reedline::PromptHistorySearchStatus::Passing => "",
            reedline::PromptHistorySearchStatus::Failing => "failing ",
        };

        Cow::Owned(format!(
            "({}reverse-search: {}) ",
            prefix, history_search.term
        ))
    }
}
