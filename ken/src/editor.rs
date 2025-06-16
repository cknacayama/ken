use std::borrow::Cow;
use std::io;

use reedline::{Prompt, Reedline, Signal};

pub struct Editor {
    editor: Reedline,
    prompt: KenPrompt,
}

impl Default for Editor {
    fn default() -> Self {
        use reedline::{
            EditCommand, KeyCode, KeyModifiers, ReedlineEvent, Vi, default_vi_insert_keybindings,
            default_vi_normal_keybindings,
        };

        let mut insert = default_vi_insert_keybindings();
        let command = EditCommand::InsertChar('\t');
        insert.add_binding(
            KeyModifiers::empty(),
            KeyCode::Tab,
            ReedlineEvent::Edit(vec![command]),
        );
        let normal = default_vi_normal_keybindings();

        let edit_mode = Vi::new(insert, normal);

        let editor = Reedline::create().with_edit_mode(Box::new(edit_mode));
        let prompt = KenPrompt(0);

        Self { editor, prompt }
    }
}

pub enum EditorRead {
    Read(String),
    Break,
    Continue,
}

#[derive(Clone, Copy)]
struct Delim {
    opening: char,
    closing: char,
}

impl Delim {
    const fn new(opening: char, closing: char) -> Self {
        Self { opening, closing }
    }
}

enum LineStatus {
    Finished,
    Delimited(Delim),
    More,
}

fn line_status(line: &str) -> LineStatus {
    match line.trim_end().chars().last().unwrap() {
        '(' => LineStatus::Delimited(Delim::new('(', ')')),
        '{' => LineStatus::Delimited(Delim::new('{', '}')),
        '[' => LineStatus::Delimited(Delim::new('[', ']')),
        '+' | '-' | '*' | '/' | '%' | '^' | '<' | '>' | '=' | '.' => LineStatus::More,
        _ => LineStatus::Finished,
    }
}

impl Editor {
    fn read_more(&mut self, mut input: String) -> io::Result<EditorRead> {
        let read = self.read()?;
        if let EditorRead::Read(more) = read {
            input.push_str(&more);
        }
        Ok(EditorRead::Read(input))
    }

    fn read_delim(&mut self, mut input: String, delim: Delim) -> io::Result<EditorRead> {
        self.prompt.inc();
        let mut count = 1;
        while count > 0 {
            let EditorRead::Read(more) = self.read_line()? else {
                continue;
            };
            input.push_str(&more);

            let more = more.trim();
            if more.ends_with(delim.opening) {
                count += 1;
            } else if more.ends_with(delim.closing) {
                count -= 1;
            }
        }
        self.prompt.dec();
        Ok(EditorRead::Read(input))
    }

    fn read_line(&mut self) -> io::Result<EditorRead> {
        match self.editor.read_line(&self.prompt)? {
            Signal::Success(input) if input.is_empty() => Ok(EditorRead::Continue),
            Signal::Success(input) => Ok(EditorRead::Read(input)),
            Signal::CtrlC | Signal::CtrlD => Ok(EditorRead::Break),
        }
    }

    pub fn read(&mut self) -> io::Result<EditorRead> {
        self.prompt.inc();
        let read = match self.read_line()? {
            EditorRead::Read(input) => match line_status(&input) {
                LineStatus::Finished => Ok(EditorRead::Read(input)),
                LineStatus::Delimited(delim) => self.read_delim(input, delim),
                LineStatus::More => self.read_more(input),
            },
            read => Ok(read),
        };
        self.prompt.dec();
        read
    }
}

#[derive(Clone, Copy)]
struct KenPrompt(usize);

impl KenPrompt {
    const fn inc(&mut self) {
        self.0 += 1;
    }

    const fn dec(&mut self) {
        self.0 -= 1;
    }

    const fn depth(self) -> usize {
        self.0
    }
}

impl Prompt for KenPrompt {
    fn render_prompt_left(&self) -> Cow<'_, str> {
        if self.depth() > 1 {
            self.render_prompt_multiline_indicator()
        } else {
            Cow::Borrowed(">> ")
        }
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
