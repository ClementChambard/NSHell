use std::io::Write;

pub struct Screen {
    width: usize,
    height: usize,
    cur_x: usize,
    cur_y: usize,
    data: Vec<String>,
    _wrapping: bool,
    pub dirty: bool,
    content_changed: bool,
    cursor_moved: bool,
}

impl Default for Screen {
    fn default() -> Self {
        // TODO: get rid of unwraps
        let (width, height) = crossterm::terminal::size().unwrap();
        let (width, height) = (width as usize, height as usize - 1);
        // crossterm::execute!(std::io::stdout(), crossterm::terminal::EnterAlternateScreen).unwrap();
        crossterm::execute!(
            std::io::stdout(),
            crossterm::terminal::Clear(crossterm::terminal::ClearType::All)
        )
        .unwrap();
        crossterm::execute!(std::io::stdout(), crossterm::cursor::MoveTo(0, 0)).unwrap();
        crossterm::terminal::enable_raw_mode().unwrap();
        std::io::stdout().flush().unwrap();
        let mut data = Vec::new();
        for _ in 0..height {
            data.push(" ".repeat(width));
        }
        Self {
            width,
            height,
            cur_x: 0,
            cur_y: 0,
            data,
            _wrapping: false,
            dirty: false,
            content_changed: false,
            cursor_moved: false,
        }
    }
}

impl Drop for Screen {
    fn drop(&mut self) {
        self.clear();
        self.flush().unwrap();
        crossterm::terminal::disable_raw_mode().unwrap();
        // crossterm::execute!(std::io::stdout(), crossterm::terminal::LeaveAlternateScreen).unwrap();
    }
}

impl Screen {
    pub fn clear(&mut self) {
        for i in 0..self.height {
            self.data[i] = " ".repeat(self.width);
        }
        self.dirty = true;
    }
    pub fn write_char_at(&mut self, x: usize, y: usize, c: char) {
        if x >= self.width || y >= self.height {
            return;
        }
        // ok to unwrap here
        if self.data[y].chars().nth(x).unwrap() == c {
            return;
        }
        let mut chars: Vec<char> = self.data[y].chars().collect();
        chars[x] = c;
        self.data[y] = chars.into_iter().collect();
        self.dirty = true;
        self.content_changed = true;
    }
    pub fn write_at(&mut self, x: usize, y: usize, s: &str) {
        let mut offset_x = 0;
        let mut offset_y = 0;
        for c in s.chars() {
            if c == '\n' {
                offset_y += 1;
                offset_x = 0;
                continue;
            }
            self.write_char_at(x + offset_x, y + offset_y, c);
            offset_x += 1;
        }
    }
    pub fn set_cursor_pos(&mut self, x: usize, y: usize) {
        self.cur_x = x;
        self.cur_y = y;
        self.dirty = true;
        self.cursor_moved = true;
    }
    pub fn flush(&mut self) -> std::io::Result<()> {
        self.dirty = false;
        if self.content_changed {
            std::io::stdout().write_all(crossterm::cursor::MoveTo(0, 0).to_string().as_bytes())?;
            for s in &self.data {
                std::io::stdout().write_all(s.as_bytes())?;
                std::io::stdout().write_all(&[b'\r', b'\n'])?;
            }
        }
        if self.content_changed || self.cursor_moved {
            std::io::stdout().write_all(
                crossterm::cursor::MoveTo(self.cur_x as u16, self.cur_y as u16)
                    .to_string()
                    .as_bytes(),
            )?;
        }
        std::io::stdout().flush()?;
        Ok(())
    }
}
