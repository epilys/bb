/*
 * bb
 *
 * Copyright 2019 Manos Pitsidianakis
 *
 * This file is part of bb.
 *
 * bb is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * bb is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with bb. If not, see <http://www.gnu.org/licenses/>.
 */

/*! The application's state.

The UI crate has an Box<Component>-Component-System design. The System part, is also the application's state, so they're both merged in the `State` struct.

`State` owns all the Components of the UI. In the application's main event loop, input is handed to the state in the form of `UIEvent` objects which traverse the component graph. Components decide to handle each input or not.

Input is received in the main loop from threads which listen on the stdin for user input, observe folders for file changes etc. The relevant struct is `ThreadEvent`.
*/

use super::*;
use crossbeam::channel::{Receiver, Sender};
use std::collections::VecDeque;
use std::io::Write;

use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use termion::{clear, cursor, style};

type StateStdout = termion::screen::AlternateScreen<termion::raw::RawTerminal<std::io::Stdout>>;

struct InputHandler {
    rx: Receiver<bool>,
    tx: Sender<bool>,
}

impl InputHandler {
    fn restore(&self, tx: Sender<ThreadEvent>) {
        let stdin = std::io::stdin();
        let rx = self.rx.clone();
        std::thread::Builder::new()
            .name("input-thread".to_string())
            .spawn(move || {
                get_events(
                    stdin,
                    |k| {
                        tx.send(ThreadEvent::Input(k)).unwrap();
                    },
                    &rx,
                )
            })
            .unwrap();
    }
    fn kill(&self) {
        self.tx.send(false).unwrap();
    }
}

/// A State object to manage and own components and components of the UI. `State` is responsible for
/// managing the terminal
pub struct State {
    cols: usize,
    rows: usize,

    grid: CellBuffer,
    stdout: Option<StateStdout>,
    components: Vec<Box<Component>>,
    pub dirty_areas: VecDeque<Area>,
    sender: Sender<ThreadEvent>,
    receiver: Receiver<ThreadEvent>,
    input: InputHandler,
}

impl Drop for State {
    fn drop(&mut self) {
        // When done, restore the defaults to avoid messing with the terminal.
        write!(
            self.stdout(),
            "{}{}{}{}{}",
            clear::All,
            style::Reset,
            cursor::Goto(1, 1),
            cursor::Show,
            BracketModeEnd,
        )
        .unwrap();
        self.flush();
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    pub fn new() -> Self {
        /* Create a channel to communicate with other threads. The main process is the sole receiver.
         * */
        let (sender, receiver) =
            crossbeam::channel::bounded(32 * ::std::mem::size_of::<ThreadEvent>());

        /*
         * Create async channel to block the input-thread if we need to fork and stop it from reading
         * stdin, see get_events() for details
         * */
        let (input_sender, input_receiver) = crossbeam::channel::unbounded();

        let termsize = termion::terminal_size().ok();
        let cols = termsize.map(|(w, _)| w).unwrap_or(0) as usize;
        let rows = termsize.map(|(_, h)| h).unwrap_or(0) as usize;

        let _stdout = std::io::stdout();
        _stdout.lock();
        let stdout = AlternateScreen::from(_stdout.into_raw_mode().unwrap());

        let mut s = State {
            cols,
            rows,
            grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
            stdout: Some(stdout),
            components: Vec::with_capacity(1),
            sender,
            receiver,
            dirty_areas: VecDeque::new(),
            input: InputHandler {
                rx: input_receiver,
                tx: input_sender,
            },
        };

        write!(
            s.stdout(),
            "{}{}{}{}",
            BracketModeStart,
            cursor::Hide,
            clear::All,
            cursor::Goto(1, 1)
        )
        .unwrap();
        s.flush();
        s.restore_input();
        s
    }

    /// Switch back to the terminal's main screen (The command line the user sees before opening
    /// the application)
    pub fn switch_to_main_screen(&mut self) {
        write!(
            self.stdout(),
            "{}{}",
            termion::screen::ToMainScreen,
            cursor::Show
        )
        .unwrap();
        self.flush();
        self.stdout = None;
        self.input.kill();
    }
    pub fn switch_to_alternate_screen(&mut self) {
        let s = std::io::stdout();
        s.lock();
        self.stdout = Some(AlternateScreen::from(s.into_raw_mode().unwrap()));

        write!(
            self.stdout(),
            "{}{}{}{}",
            termion::screen::ToAlternateScreen,
            cursor::Hide,
            clear::All,
            cursor::Goto(1, 1)
        )
        .unwrap();
        self.flush();
    }

    pub fn receiver(&self) -> Receiver<ThreadEvent> {
        self.receiver.clone()
    }

    /// On `SIGWNICH` the `State` redraws itself according to the new terminal size.
    pub fn update_size(&mut self) {
        let termsize = termion::terminal_size().ok();
        let termcols = termsize.map(|(w, _)| w);
        let termrows = termsize.map(|(_, h)| h);
        if termcols.unwrap_or(72) as usize != self.cols
            || termrows.unwrap_or(120) as usize != self.rows
        {
            eprintln!(
                "Size updated, from ({}, {}) -> ({:?}, {:?})",
                self.cols, self.rows, termcols, termrows
            );
        }
        self.cols = termcols.unwrap_or(72) as usize;
        self.rows = termrows.unwrap_or(120) as usize;
        self.grid.resize(self.cols, self.rows, Cell::with_char(' '));

        self.rcv_event(UIEvent::Resize);

        // Invalidate dirty areas.
        self.dirty_areas.clear();
    }

    /// Force a redraw for all dirty components.
    pub fn redraw(&mut self, tick: bool) {
        for i in 0..self.components.len() {
            self.draw_component(i, tick);
        }
        let mut areas: Vec<Area> = self.dirty_areas.drain(0..).collect();
        /* Sort by x_start, ie upper_left corner's x coordinate */
        areas.sort_by(|a, b| (a.0).0.partial_cmp(&(b.0).0).unwrap());
        /* draw each dirty area */
        let rows = self.rows;
        for y in 0..rows {
            let mut segment = None;
            for ((x_start, y_start), (x_end, y_end)) in &areas {
                if y < *y_start || y > *y_end {
                    continue;
                }
                if let Some((x_start, x_end)) = segment.take() {
                    self.draw_horizontal_segment(x_start, x_end, y);
                }
                match segment {
                    ref mut s @ None => {
                        *s = Some((*x_start, *x_end));
                    }
                    ref mut s @ Some(_) if s.unwrap().1 < *x_start => {
                        self.draw_horizontal_segment(s.unwrap().0, s.unwrap().1, y);
                        *s = Some((*x_start, *x_end));
                    }
                    ref mut s @ Some(_) if s.unwrap().1 < *x_end => {
                        self.draw_horizontal_segment(s.unwrap().0, s.unwrap().1, y);
                        *s = Some((s.unwrap().1, *x_end));
                    }
                    Some((_, ref mut x)) => {
                        *x = *x_end;
                    }
                }
            }
            if let Some((x_start, x_end)) = segment {
                self.draw_horizontal_segment(x_start, x_end, y);
            }
        }
        self.flush();
    }

    /// Draw only a specific `area` on the screen.
    fn draw_horizontal_segment(&mut self, x_start: usize, x_end: usize, y: usize) {
        write!(
            self.stdout(),
            "{}",
            cursor::Goto(x_start as u16 + 1, (y + 1) as u16)
        )
        .unwrap();
        for x in x_start..=x_end {
            let c = self.grid[(x, y)];
            if c.bg() != Color::Default {
                write!(self.stdout(), "{}", termion::color::Bg(c.bg().as_termion())).unwrap();
            }
            if c.fg() != Color::Default {
                write!(self.stdout(), "{}", termion::color::Fg(c.fg().as_termion())).unwrap();
            }
            if c.attrs() != Attr::Default {
                write!(self.stdout(), "\x1B[{}m", c.attrs() as u8).unwrap();
            }
            if !c.empty() {
                write!(self.stdout(), "{}", c.ch()).unwrap();
            }

            if c.bg() != Color::Default {
                write!(
                    self.stdout(),
                    "{}",
                    termion::color::Bg(termion::color::Reset)
                )
                .unwrap();
            }
            if c.fg() != Color::Default {
                write!(
                    self.stdout(),
                    "{}",
                    termion::color::Fg(termion::color::Reset)
                )
                .unwrap();
            }
            if c.attrs() != Attr::Default {
                write!(self.stdout(), "\x1B[{}m", Attr::Default as u8).unwrap();
            }
        }
    }

    /// Draw the entire screen from scratch.
    pub fn render(&mut self) {
        self.update_size();
        let cols = self.cols;
        let rows = self.rows;
        self.dirty_areas.push_back(((0, 0), (cols - 1, rows - 1)));

        self.redraw(true);
    }

    pub fn draw_component(&mut self, idx: usize, tick: bool) {
        if self.cols < 80 || self.cols < 24 {
            return;
        }

        let component = &mut self.components[idx];
        let upper_left = (0, 0);
        let bottom_right = (self.cols - 1, self.rows - 1);

        if component.is_dirty() {
            component.draw(
                &mut self.grid,
                (upper_left, bottom_right),
                &mut self.dirty_areas,
                tick,
            );
        }
    }
    pub fn register_component(&mut self, component: Box<Component>) {
        self.components.push(component);
    }
    /// The application's main loop sends `UIEvents` to state via this method.
    pub fn rcv_event(&mut self, mut event: UIEvent) {
        /* inform each component */
        for i in 0..self.components.len() {
            self.components[i].process_event(&mut event);
        }
    }

    fn flush(&mut self) {
        if let Some(s) = self.stdout.as_mut() {
            s.flush().unwrap();
        }
    }

    fn stdout(&mut self) -> &mut StateStdout {
        self.stdout.as_mut().unwrap()
    }

    pub fn restore_input(&self) {
        self.input.restore(self.sender.clone());
    }
}
