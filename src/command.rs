use crossterm::event::KeyEvent;

use crate::{lexer, parser};

#[derive(Default)]
pub struct Command {
    command_str: String,
    cursor: usize,
    last_out: String,
}

impl Command {
    pub fn update(&mut self, key_event: KeyEvent) {
        let KeyEvent {
            code,
            state: _,
            kind,
            modifiers: _,
        } = key_event;
        if kind != crossterm::event::KeyEventKind::Press {
            return;
        }
        match code {
            crossterm::event::KeyCode::Char(c) => {
                self.command_str.insert(self.cursor, c);
                self.cursor += 1;
            }
            crossterm::event::KeyCode::Left => {
                if self.cursor == 0 {
                    return;
                }
                self.cursor -= 1;
            }
            crossterm::event::KeyCode::Right => {
                if self.cursor >= self.command_str.len() {
                    return;
                }
                self.cursor += 1;
            }
            crossterm::event::KeyCode::Delete => {
                if self.cursor >= self.command_str.len() {
                    return;
                }
                self.command_str.remove(self.cursor);
            }
            crossterm::event::KeyCode::Backspace => {
                if self.cursor == 0 {
                    return;
                }
                self.cursor -= 1;
                self.command_str.remove(self.cursor);
            }
            crossterm::event::KeyCode::Enter => {
                self.command_str.push('\n');
                self.submit();
            }
            _ => {}
        }
    }

    pub fn submit(&mut self) {
        self.cursor = 0;
        let tokens = lexer::lex(&self.command_str);
        match parser::Program::parse(&tokens) {
            Ok(p) => self.last_out = p.exec().stdout,
            Err(e) => self.last_out = e,
        }
        self.command_str.clear();
    }

    pub fn render(&self, scr: &mut crate::screen::Screen, x: usize, y: usize, _linewrap: bool) {
        scr.write_at(x, y, &self.command_str);
        scr.write_at(x, y + 2, &self.last_out);
        scr.set_cursor_pos(x + self.cursor, y);
    }
}
