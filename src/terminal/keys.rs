// bb
//
// Copyright 2019 Manos Pitsidianakis
//
// This file is part of bb.
//
// bb is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// bb is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with bb. If not, see <http://www.gnu.org/licenses/>.

use std::{fmt, io};

use termion::{
    event::{Event as TermionEvent, Key as TermionKey},
    input::TermRead,
};

use crate::crossbeam::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Key {
    /// Backspace.
    Backspace,
    /// Left arrow.
    Left,
    /// Right arrow.
    Right,
    /// Up arrow.
    Up,
    /// Down arrow.
    Down,
    /// Home key.
    Home,
    /// End key.
    End,
    /// Page Up key.
    PageUp,
    /// Page Down key.
    PageDown,
    /// Delete key.
    Delete,
    /// Insert key.
    Insert,
    /// Function keys.
    ///
    /// Only function keys 1 through 12 are supported.
    F(u8),
    /// Normal character.
    Char(char),
    /// Alt modified character.
    Alt(char),
    /// Ctrl modified character.
    ///
    /// Note that certain keys may not be modifiable with `ctrl`, due to
    /// limitations of terminals.
    Ctrl(char),
    /// Null byte.
    Null,
    /// Esc key.
    Esc,
    Paste(String),
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Key::*;
        match self {
            F(n) => write!(f, "F{}", n),
            Char('\t') => write!(f, "Tab"),
            Char('\n') => write!(f, "Enter"),
            Char(' ') => write!(f, "Space"),
            Char(c) => write!(f, "{}", c),
            Alt(c) => write!(f, "M-{}", c),
            Ctrl(c) => write!(f, "C-{}", c),
            Paste(_) => write!(f, "Pasted buf"),
            Null => write!(f, "Null byte"),
            Esc => write!(f, "Esc"),
            Backspace => write!(f, "Backspace"),
            Left => write!(f, "Left"),
            Right => write!(f, "Right"),
            Up => write!(f, "Up"),
            Down => write!(f, "Down"),
            Home => write!(f, "Home"),
            End => write!(f, "End"),
            PageUp => write!(f, "PageUp"),
            PageDown => write!(f, "PageDown"),
            Delete => write!(f, "Delete"),
            Insert => write!(f, "Insert"),
        }
    }
}

impl<'a> From<&'a String> for Key {
    fn from(v: &'a String) -> Self {
        Key::Paste(v.to_string())
    }
}

impl From<TermionKey> for Key {
    fn from(k: TermionKey) -> Self {
        match k {
            TermionKey::Backspace => Key::Backspace,
            TermionKey::Left => Key::Left,
            TermionKey::Right => Key::Right,
            TermionKey::Up => Key::Up,
            TermionKey::Down => Key::Down,
            TermionKey::Home => Key::Home,
            TermionKey::End => Key::End,
            TermionKey::PageUp => Key::PageUp,
            TermionKey::PageDown => Key::PageDown,
            TermionKey::Delete => Key::Delete,
            TermionKey::Insert => Key::Insert,
            TermionKey::F(u) => Key::F(u),
            TermionKey::Char(c) => Key::Char(c),
            TermionKey::Alt(c) => Key::Alt(c),
            TermionKey::Ctrl(c) => Key::Ctrl(c),
            TermionKey::Null => Key::Null,
            TermionKey::Esc => Key::Esc,
            _ => Key::Char(' '),
        }
    }
}

impl PartialEq<Key> for &Key {
    fn eq(&self, other: &Key) -> bool {
        **self == *other
    }
}

#[derive(PartialEq)]
enum InputMode {
    Normal,
    Paste,
}

pub fn get_events(
    stdin: io::Stdin,
    mut closure: impl FnMut(Key),
    rx: &crossbeam::channel::Receiver<bool>,
) {
    let mut input_mode = InputMode::Normal;
    let mut paste_buf = String::with_capacity(256);
    for c in stdin.events() {
        select! {
            default => {},
            recv(rx) -> _ => {
                return;
            }
        };
        match c {
            Ok(TermionEvent::Key(TermionKey::Ctrl('c'))) if input_mode == InputMode::Normal => {
                let self_pid = nix::unistd::Pid::this();
                nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGINT).unwrap();
            }
            Ok(TermionEvent::Key(TermionKey::Ctrl('4'))) if input_mode == InputMode::Normal => {
                let self_pid = nix::unistd::Pid::this();
                nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGQUIT).unwrap();
            }
            Ok(TermionEvent::Key(k)) if input_mode == InputMode::Normal => {
                closure(Key::from(k));
            }
            Ok(TermionEvent::Key(TermionKey::Char(k))) if input_mode == InputMode::Paste => {
                paste_buf.push(k);
            }
            Ok(TermionEvent::Unsupported(ref k)) if k.as_slice() == BRACKET_PASTE_START => {
                input_mode = InputMode::Paste;
            }
            Ok(TermionEvent::Unsupported(ref k)) if k.as_slice() == BRACKET_PASTE_END => {
                input_mode = InputMode::Normal;
                let ret = Key::from(&paste_buf);
                paste_buf.clear();
                closure(ret);
            }
            _ => {} // Mouse events or errors.
        }
    }
}

// CSI events we use

// Some macros taken from termion:
/// Create a CSI-introduced sequence.
macro_rules! csi {
    ($( $l:expr ),*) => { concat!("\x1B[", $( $l ),*) };
}

/// Derive a CSI sequence struct.
macro_rules! derive_csi_sequence {
    ($(#[$outer:meta])*
    ($name:ident, $value:expr)) => {
        $(#[$outer])*
        #[derive(Copy, Clone)]
        pub struct $name;

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, csi!($value))
            }
        }

        impl AsRef<[u8]> for $name {
            fn as_ref(&self) -> &'static [u8] {
                csi!($value).as_bytes()
            }
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &'static str {
                csi!($value)
            }
        }
    };
}

derive_csi_sequence!(
    #[doc = "Empty struct with a Display implementation that returns the byte sequence to start [Bracketed Paste Mode](http://www.xfree86.org/current/ctlseqs.html#Bracketed%20Paste%20Mode)"]
    (BracketModeStart, "?2004h")
);

derive_csi_sequence!(
    #[doc = "Empty struct with a Display implementation that returns the byte sequence to end [Bracketed Paste Mode](http://www.xfree86.org/current/ctlseqs.html#Bracketed%20Paste%20Mode)"]
    (BracketModeEnd, "?2004l")
);

pub const BRACKET_PASTE_START: &[u8] = b"\x1B[200~";
pub const BRACKET_PASTE_END: &[u8] = b"\x1B[201~";

derive_csi_sequence!(
    #[doc = "`CSI Ps ; Ps ; Ps t`, where `Ps = 2 2 ; 0`  -> Save xterm icon and window title on \
             stack."]
    (SaveWindowTitleIconToStack, "22;0t")
);

derive_csi_sequence!(
    #[doc = "Restore window title and icon from terminal's title stack. `CSI Ps ; Ps ; Ps t`, \
             where `Ps = 2 3 ; 0`  -> Restore xterm icon and window title from stack."]
    (RestoreWindowTitleIconFromStack, "23;0t")
);
