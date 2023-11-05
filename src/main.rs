#[macro_use]
extern crate lazy_static;
extern crate crossterm;
mod builtins;
mod command;
mod lexer;
mod parser;
mod screen;

use crossterm::event::{KeyEvent, KeyModifiers};

use std::sync::Mutex;

lazy_static! {
    pub static ref SHOULD_EXIT: Mutex<bool> = Mutex::new(false);
}

fn render(cmd: &command::Command, scr: &mut screen::Screen) {
    scr.clear();
    cmd.render(scr, 0, 0, false);
}

fn main() -> Result<(), String> {
    // let tokens = lexer::lex(r#"until [[ $a -eq 1 ]] do ls done"#);

    // let tree = parser::Program::parse(&tokens);
    // println!("{:#?}", tree);

    // return Ok(());
    let mut the_screen = screen::Screen::default();
    let mut the_command = command::Command::default();
    loop {
        if *SHOULD_EXIT.lock().unwrap() {
            break;
        }
        if crossterm::event::poll(std::time::Duration::from_millis(10))
            .map_err(|e| e.to_string())?
        {
            if let crossterm::event::Event::Key(KeyEvent {
                code,
                state,
                kind,
                modifiers,
            }) = crossterm::event::read().map_err(|e| e.to_string())?
            {
                if kind == crossterm::event::KeyEventKind::Press {
                    if let KeyModifiers::CONTROL = modifiers {
                        if let crossterm::event::KeyCode::Char('q') = code {
                            break;
                        }
                    }
                }
                the_command.update(KeyEvent {
                    code,
                    state,
                    kind,
                    modifiers,
                });
                render(&the_command, &mut the_screen);
            }
        }
        if the_screen.dirty {
            the_screen.flush().map_err(|e| e.to_string())?;
        }
    }

    Ok(())
}
